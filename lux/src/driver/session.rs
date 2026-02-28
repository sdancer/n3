use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use crate::syntax::ast::{Item, Module};
use crate::syntax::lexer::Lexer;
use crate::syntax::parser::{ParseError, Parser, ParserOptions};
use crate::syntax::span::Span;
use crate::syntax::token::TokenKind;
use crate::types::env::TypeEnv;
use crate::types::infer::InferenceContext;
use crate::types::types::{Scheme, Type};
use crate::types::unify::TypeError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SecurityProfile {
    Trusted,
    Sandboxed,
}

#[derive(Debug, Clone)]
pub struct SessionConfig {
    pub security_profile: SecurityProfile,
    pub allowed_imports: Option<HashSet<String>>,
}

impl SessionConfig {
    pub fn trusted() -> Self {
        Self {
            security_profile: SecurityProfile::Trusted,
            allowed_imports: None,
        }
    }

    pub fn sandboxed_default() -> Self {
        Self {
            security_profile: SecurityProfile::Sandboxed,
            allowed_imports: Some(
                ["prelude", "list", "option", "result"]
                    .into_iter()
                    .map(str::to_string)
                    .collect(),
            ),
        }
    }

    pub fn allow_extern(&self) -> bool {
        matches!(self.security_profile, SecurityProfile::Trusted)
    }
}

pub struct Session {
    pub source_files: Vec<PathBuf>,
    pub output_dir: PathBuf,
    pub errors: Vec<CompileError>,
    pub config: SessionConfig,
}

#[derive(Debug)]
pub enum CompileError {
    Parse(ParseError),
    Type(TypeError),
    Io(std::io::Error),
    Security(SecurityError),
}

#[derive(Debug, Clone)]
pub enum SecurityError {
    ExternDisallowed(Span),
    ImportDisallowed { module: String, span: Span },
}

impl From<ParseError> for CompileError {
    fn from(e: ParseError) -> Self {
        CompileError::Parse(e)
    }
}

impl From<TypeError> for CompileError {
    fn from(e: TypeError) -> Self {
        CompileError::Type(e)
    }
}

impl From<std::io::Error> for CompileError {
    fn from(e: std::io::Error) -> Self {
        CompileError::Io(e)
    }
}

impl From<SecurityError> for CompileError {
    fn from(e: SecurityError) -> Self {
        CompileError::Security(e)
    }
}

impl Session {
    pub fn new(output_dir: PathBuf) -> Self {
        Self::with_config(output_dir, SessionConfig::trusted())
    }

    pub fn with_config(output_dir: PathBuf, config: SessionConfig) -> Self {
        Session {
            source_files: Vec::new(),
            output_dir,
            errors: Vec::new(),
            config,
        }
    }

    pub fn add_file(&mut self, path: PathBuf) {
        self.source_files.push(path);
    }

    pub fn compile_source(&mut self, source: &str) -> Result<Module, CompileError> {
        // Lex
        let tokens = Lexer::new(source).tokenize();
        for token in &tokens {
            if let TokenKind::Error(msg) = &token.kind {
                return Err(ParseError::new(format!("Lexer error: {}", msg), token.span).into());
            }
        }

        // Parse with security-aware parser settings.
        let mut parser = Parser::new_with_options(
            tokens,
            ParserOptions {
                allow_extern: self.config.allow_extern(),
            },
        );
        let module = parser.parse_module()?;

        self.validate_module_security(&module)?;

        // Type check
        let mut ctx = InferenceContext::new();
        let mut env = TypeEnv::new();
        self.register_builtins(&mut env);

        // First pass: register type names so recursive/forward references can resolve.
        for item in &module.items {
            match item {
                Item::Enum(enum_def) => {
                    ctx.register_type(enum_def.name.clone(), enum_def.type_params.clone(), vec![]);
                }
                Item::Struct(struct_def) => {
                    ctx.register_struct(
                        struct_def.name.clone(),
                        struct_def.type_params.clone(),
                        vec![],
                    );
                }
                _ => {}
            }
        }

        // Second pass: populate variant and field types.
        for item in &module.items {
            match item {
                Item::Enum(enum_def) => {
                    let mut type_params_map = HashMap::new();
                    for (i, param) in enum_def.type_params.iter().enumerate() {
                        type_params_map.insert(param.clone(), i as crate::types::types::TyVar);
                    }

                    let mut variants = Vec::new();
                    for variant in &enum_def.variants {
                        let mut field_types = Vec::new();
                        for field_ty in &variant.fields {
                            let ty = ctx.type_expr_to_type(field_ty, &type_params_map)?;
                            field_types.push(ty);
                        }
                        variants.push((variant.name.clone(), field_types));
                    }
                    ctx.register_type(
                        enum_def.name.clone(),
                        enum_def.type_params.clone(),
                        variants,
                    );
                }
                Item::Struct(struct_def) => {
                    let mut type_params_map = HashMap::new();
                    for (i, param) in struct_def.type_params.iter().enumerate() {
                        type_params_map.insert(param.clone(), i as crate::types::types::TyVar);
                    }

                    let mut fields = Vec::new();
                    for field in &struct_def.fields {
                        let ty = ctx.type_expr_to_type(&field.ty, &type_params_map)?;
                        fields.push((field.name.clone(), ty));
                    }
                    ctx.register_struct(
                        struct_def.name.clone(),
                        struct_def.type_params.clone(),
                        fields,
                    );
                }
                _ => {}
            }
        }

        // Register extern function declarations.
        for item in &module.items {
            if let Item::Extern(extern_block) = item {
                for extern_fn in &extern_block.decls {
                    let mut type_params_map = HashMap::new();
                    for (i, param) in extern_fn.type_params.iter().enumerate() {
                        type_params_map.insert(param.clone(), i as crate::types::types::TyVar);
                    }

                    let mut param_types = Vec::new();
                    for param_ty in &extern_fn.params {
                        let ty = ctx.type_expr_to_type(param_ty, &type_params_map)?;
                        param_types.push(ty);
                    }

                    let return_type =
                        ctx.type_expr_to_type(&extern_fn.return_type, &type_params_map)?;
                    ctx.register_extern_fn(
                        &extern_fn.module,
                        &extern_fn.name,
                        &extern_fn.type_params,
                        param_types,
                        return_type,
                    );
                }
            }
        }

        // Register functions in environment so they can call each other.
        for item in &module.items {
            if let Item::Function(func) = item {
                let mut param_types = Vec::new();
                for param in &func.params {
                    let ty = if let Some(ty_expr) = &param.ty {
                        ctx.type_expr_to_type(ty_expr, &HashMap::new())?
                    } else {
                        ctx.fresh_var()
                    };
                    param_types.push(ty);
                }
                let ret_type = if let Some(ret) = &func.return_type {
                    ctx.type_expr_to_type(ret, &HashMap::new())?
                } else {
                    ctx.fresh_var()
                };

                let fn_type = Type::Function(param_types, Box::new(ret_type));
                env.insert(func.name.clone(), Scheme::mono(fn_type));
            }
        }

        // Type check each function.
        for item in &module.items {
            if let Item::Function(func) = item {
                let mut func_env = env.clone();
                for param in &func.params {
                    let param_type = if let Some(ty) = &param.ty {
                        ctx.type_expr_to_type(ty, &HashMap::new())?
                    } else {
                        ctx.fresh_var()
                    };
                    func_env.insert(param.name.clone(), Scheme::mono(param_type));
                }

                let body_type = ctx.infer_expr(&func_env, &func.body)?;
                if let Some(ret) = &func.return_type {
                    let ret_type = ctx.type_expr_to_type(ret, &HashMap::new())?;
                    ctx.unify(&body_type, &ret_type, func.span)?;
                }
            }
        }

        Ok(module)
    }

    fn validate_module_security(&self, module: &Module) -> Result<(), CompileError> {
        if self.config.security_profile == SecurityProfile::Sandboxed {
            for item in &module.items {
                if let Item::Extern(extern_block) = item {
                    return Err(SecurityError::ExternDisallowed(extern_block.span).into());
                }
            }
        }

        if let Some(allowed) = &self.config.allowed_imports {
            for item in &module.items {
                if let Item::Use(use_decl) = item {
                    if !allowed.contains(&use_decl.module) {
                        return Err(SecurityError::ImportDisallowed {
                            module: use_decl.module.clone(),
                            span: use_decl.span,
                        }
                        .into());
                    }
                }
            }
        }

        Ok(())
    }

    fn register_builtins(&self, env: &mut TypeEnv) {
        let any = Type::Any;
        let int = Type::Int;
        let float = Type::Float;
        let bool_ty = Type::Bool;
        let string = Type::String;
        let atom = Type::Atom;
        let unit = Type::Unit;
        let pid = Type::Pid;
        let ref_ty = Type::Ref;

        let mut register_builtin = |name: &str, params: Vec<Type>, ret: Type| {
            let fn_type = Type::Function(params, Box::new(ret));
            env.insert(name.to_string(), Scheme::mono(fn_type));
        };

        // Pure computational built-ins available in all profiles.
        register_builtin(
            "length",
            vec![Type::List(Box::new(any.clone()))],
            int.clone(),
        );
        register_builtin("hd", vec![Type::List(Box::new(any.clone()))], any.clone());
        register_builtin(
            "tl",
            vec![Type::List(Box::new(any.clone()))],
            Type::List(Box::new(any.clone())),
        );
        register_builtin(
            "reverse",
            vec![Type::List(Box::new(any.clone()))],
            Type::List(Box::new(any.clone())),
        );
        register_builtin(
            "sort",
            vec![Type::List(Box::new(any.clone()))],
            Type::List(Box::new(any.clone())),
        );
        register_builtin(
            "append",
            vec![
                Type::List(Box::new(any.clone())),
                Type::List(Box::new(any.clone())),
            ],
            Type::List(Box::new(any.clone())),
        );
        register_builtin(
            "flatten",
            vec![Type::List(Box::new(any.clone()))],
            Type::List(Box::new(any.clone())),
        );
        register_builtin(
            "take",
            vec![Type::List(Box::new(any.clone())), int.clone()],
            Type::List(Box::new(any.clone())),
        );
        register_builtin(
            "drop",
            vec![Type::List(Box::new(any.clone())), int.clone()],
            Type::List(Box::new(any.clone())),
        );
        register_builtin(
            "nth",
            vec![Type::List(Box::new(any.clone())), int.clone()],
            any.clone(),
        );
        register_builtin(
            "member",
            vec![any.clone(), Type::List(Box::new(any.clone()))],
            bool_ty.clone(),
        );
        register_builtin(
            "unique",
            vec![Type::List(Box::new(any.clone()))],
            Type::List(Box::new(any.clone())),
        );
        register_builtin(
            "zip",
            vec![
                Type::List(Box::new(any.clone())),
                Type::List(Box::new(any.clone())),
            ],
            Type::List(Box::new(any.clone())),
        );
        register_builtin(
            "unzip",
            vec![Type::List(Box::new(any.clone()))],
            Type::Tuple(vec![
                Type::List(Box::new(any.clone())),
                Type::List(Box::new(any.clone())),
            ]),
        );
        register_builtin(
            "enumerate",
            vec![Type::List(Box::new(any.clone()))],
            Type::List(Box::new(any.clone())),
        );

        register_builtin("abs", vec![int.clone()], int.clone());
        register_builtin("max", vec![int.clone(), int.clone()], int.clone());
        register_builtin("min", vec![int.clone(), int.clone()], int.clone());
        register_builtin("rem", vec![int.clone(), int.clone()], int.clone());
        register_builtin("div", vec![int.clone(), int.clone()], int.clone());
        register_builtin("band", vec![int.clone(), int.clone()], int.clone());
        register_builtin("bor", vec![int.clone(), int.clone()], int.clone());
        register_builtin("bxor", vec![int.clone(), int.clone()], int.clone());
        register_builtin("bnot", vec![int.clone()], int.clone());
        register_builtin("bsl", vec![int.clone(), int.clone()], int.clone());
        register_builtin("bsr", vec![int.clone(), int.clone()], int.clone());

        register_builtin("to_string", vec![any.clone()], string.clone());
        register_builtin("to_int", vec![any.clone()], int.clone());
        register_builtin("to_float", vec![any.clone()], float.clone());
        register_builtin("to_atom", vec![string.clone()], atom.clone());

        register_builtin(
            "fst",
            vec![Type::Tuple(vec![any.clone(), any.clone()])],
            any.clone(),
        );
        register_builtin(
            "snd",
            vec![Type::Tuple(vec![any.clone(), any.clone()])],
            any.clone(),
        );
        register_builtin("elem", vec![any.clone(), int.clone()], any.clone());
        register_builtin(
            "set_elem",
            vec![any.clone(), int.clone(), any.clone()],
            any.clone(),
        );
        register_builtin(
            "tuple_to_list",
            vec![any.clone()],
            Type::List(Box::new(any.clone())),
        );
        register_builtin(
            "list_to_tuple",
            vec![Type::List(Box::new(any.clone()))],
            any.clone(),
        );

        register_builtin("is_int", vec![any.clone()], bool_ty.clone());
        register_builtin("is_float", vec![any.clone()], bool_ty.clone());
        register_builtin("is_number", vec![any.clone()], bool_ty.clone());
        register_builtin("is_atom", vec![any.clone()], bool_ty.clone());
        register_builtin("is_list", vec![any.clone()], bool_ty.clone());
        register_builtin("is_tuple", vec![any.clone()], bool_ty.clone());
        register_builtin("is_map", vec![any.clone()], bool_ty.clone());
        register_builtin("is_bool", vec![any.clone()], bool_ty.clone());
        register_builtin("is_string", vec![any.clone()], bool_ty.clone());
        register_builtin("is_pid", vec![any.clone()], bool_ty.clone());
        register_builtin("is_function", vec![any.clone()], bool_ty.clone());
        register_builtin("is_binary", vec![any.clone()], bool_ty.clone());

        register_builtin("str_length", vec![string.clone()], int.clone());
        register_builtin(
            "str_concat",
            vec![string.clone(), string.clone()],
            string.clone(),
        );
        register_builtin(
            "str_split",
            vec![string.clone(), string.clone()],
            Type::List(Box::new(string.clone())),
        );
        register_builtin(
            "str_join",
            vec![Type::List(Box::new(string.clone())), string.clone()],
            string.clone(),
        );
        register_builtin("str_trim", vec![string.clone()], string.clone());
        register_builtin("str_upper", vec![string.clone()], string.clone());
        register_builtin("str_lower", vec![string.clone()], string.clone());
        register_builtin(
            "str_replace",
            vec![string.clone(), string.clone(), string.clone()],
            string.clone(),
        );
        register_builtin(
            "str_contains",
            vec![string.clone(), string.clone()],
            bool_ty.clone(),
        );
        register_builtin(
            "str_starts_with",
            vec![string.clone(), string.clone()],
            bool_ty.clone(),
        );
        register_builtin(
            "str_ends_with",
            vec![string.clone(), string.clone()],
            bool_ty.clone(),
        );
        register_builtin(
            "str_slice",
            vec![string.clone(), int.clone(), int.clone()],
            string.clone(),
        );
        register_builtin(
            "str_char_at",
            vec![string.clone(), int.clone()],
            int.clone(),
        );
        register_builtin(
            "chars",
            vec![string.clone()],
            Type::List(Box::new(int.clone())),
        );
        register_builtin(
            "str_from_chars",
            vec![Type::List(Box::new(int.clone()))],
            string.clone(),
        );

        register_builtin(
            "map_put",
            vec![
                Type::Map(Box::new(any.clone()), Box::new(any.clone())),
                any.clone(),
                any.clone(),
            ],
            Type::Map(Box::new(any.clone()), Box::new(any.clone())),
        );
        register_builtin(
            "map_get",
            vec![
                Type::Map(Box::new(any.clone()), Box::new(any.clone())),
                any.clone(),
            ],
            any.clone(),
        );
        register_builtin(
            "map_remove",
            vec![
                Type::Map(Box::new(any.clone()), Box::new(any.clone())),
                any.clone(),
            ],
            Type::Map(Box::new(any.clone()), Box::new(any.clone())),
        );
        register_builtin(
            "map_keys",
            vec![Type::Map(Box::new(any.clone()), Box::new(any.clone()))],
            Type::List(Box::new(any.clone())),
        );
        register_builtin(
            "map_values",
            vec![Type::Map(Box::new(any.clone()), Box::new(any.clone()))],
            Type::List(Box::new(any.clone())),
        );
        register_builtin(
            "map_has_key",
            vec![
                Type::Map(Box::new(any.clone()), Box::new(any.clone())),
                any.clone(),
            ],
            bool_ty.clone(),
        );
        register_builtin(
            "map_merge",
            vec![
                Type::Map(Box::new(any.clone()), Box::new(any.clone())),
                Type::Map(Box::new(any.clone()), Box::new(any.clone())),
            ],
            Type::Map(Box::new(any.clone()), Box::new(any.clone())),
        );
        register_builtin(
            "map_size",
            vec![Type::Map(Box::new(any.clone()), Box::new(any.clone()))],
            int.clone(),
        );
        register_builtin(
            "map_to_list",
            vec![Type::Map(Box::new(any.clone()), Box::new(any.clone()))],
            Type::List(Box::new(any.clone())),
        );
        register_builtin(
            "list_to_map",
            vec![Type::List(Box::new(any.clone()))],
            Type::Map(Box::new(any.clone()), Box::new(any.clone())),
        );
        register_builtin("size", vec![any.clone()], int.clone());

        register_builtin("byte_size", vec![string.clone()], int.clone());
        register_builtin("bit_size", vec![string.clone()], int.clone());
        register_builtin(
            "binary_to_list",
            vec![string.clone()],
            Type::List(Box::new(int.clone())),
        );
        register_builtin(
            "list_to_binary",
            vec![Type::List(Box::new(int.clone()))],
            string.clone(),
        );
        register_builtin(
            "atom_to_list",
            vec![atom.clone()],
            Type::List(Box::new(int.clone())),
        );
        register_builtin(
            "list_to_atom",
            vec![Type::List(Box::new(int.clone()))],
            atom.clone(),
        );
        register_builtin(
            "integer_to_list",
            vec![int.clone()],
            Type::List(Box::new(int.clone())),
        );
        register_builtin(
            "list_to_integer",
            vec![Type::List(Box::new(int.clone()))],
            int.clone(),
        );
        register_builtin(
            "float_to_list",
            vec![float.clone()],
            Type::List(Box::new(int.clone())),
        );
        register_builtin(
            "list_to_float",
            vec![Type::List(Box::new(int.clone()))],
            float.clone(),
        );
        register_builtin("iolist_to_binary", vec![any.clone()], string.clone());
        register_builtin("term_to_binary", vec![any.clone()], string.clone());
        register_builtin("binary_to_term", vec![string.clone()], any.clone());
        register_builtin(
            "apply",
            vec![any.clone(), Type::List(Box::new(any.clone()))],
            any.clone(),
        );
        register_builtin("typeof", vec![any.clone()], atom.clone());
        register_builtin("assert", vec![bool_ty.clone()], unit.clone());

        if self.config.security_profile == SecurityProfile::Trusted {
            register_builtin("print", vec![any.clone()], atom.clone());
            register_builtin("println", vec![any.clone()], atom.clone());
            register_builtin("dbg", vec![any.clone()], any.clone());

            register_builtin("throw", vec![any.clone()], Type::Never);
            register_builtin("exit", vec![any.clone()], Type::Never);
            register_builtin("error", vec![any.clone()], Type::Never);

            register_builtin("sleep", vec![int.clone()], atom.clone());
            register_builtin("now", vec![], int.clone());
            register_builtin("monotonic_time", vec![], int.clone());
            register_builtin("random", vec![], float.clone());
            register_builtin("random_seed", vec![], any.clone());

            register_builtin(
                "spawn_link",
                vec![Type::Function(vec![], Box::new(any.clone()))],
                pid.clone(),
            );
            register_builtin("link", vec![pid.clone()], bool_ty.clone());
            register_builtin("unlink", vec![pid.clone()], bool_ty.clone());
            register_builtin("monitor", vec![pid.clone()], ref_ty.clone());
            register_builtin("demonitor", vec![ref_ty.clone()], bool_ty.clone());
            register_builtin("registered", vec![], Type::List(Box::new(atom.clone())));
            register_builtin("register", vec![atom.clone(), pid.clone()], bool_ty.clone());
            register_builtin("whereis", vec![atom.clone()], pid.clone());
            register_builtin("make_ref", vec![], ref_ty.clone());

            register_builtin("file_read", vec![string.clone()], any.clone());
            register_builtin(
                "file_write",
                vec![string.clone(), string.clone()],
                atom.clone(),
            );
            register_builtin("file_exists", vec![string.clone()], bool_ty.clone());
            register_builtin("file_delete", vec![string.clone()], atom.clone());
            register_builtin("dir_list", vec![string.clone()], any.clone());
            register_builtin("dir_make", vec![string.clone()], atom.clone());
            register_builtin("get_cwd", vec![], any.clone());

            register_builtin("argv", vec![], Type::List(Box::new(string.clone())));
            register_builtin("env", vec![string.clone()], string.clone());
            register_builtin(
                "set_env",
                vec![string.clone(), string.clone()],
                bool_ty.clone(),
            );
            register_builtin("exit_code", vec![int.clone()], Type::Never);
            register_builtin("os_cmd", vec![string.clone()], string.clone());

            register_builtin("get", vec![atom.clone()], any.clone());
            register_builtin("put", vec![atom.clone(), any.clone()], any.clone());
            register_builtin("erase", vec![atom.clone()], any.clone());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sandbox_rejects_extern_declaration() {
        let source = r#"
            extern "erlang" {
                fn lists::reverse(List<Int>) -> List<Int>
            }

            fn main() { 1 }
        "#;

        let mut session = Session::with_config(PathBuf::new(), SessionConfig::sandboxed_default());
        let err = session.compile_source(source).unwrap_err();

        match err {
            CompileError::Parse(parse_err) => {
                assert!(parse_err.message.contains("disabled in sandbox mode"));
            }
            other => panic!("expected parse error, got: {:?}", other),
        }
    }

    #[test]
    fn sandbox_rejects_non_whitelisted_import() {
        let source = r#"
            use sysutil
            fn main() { 1 }
        "#;

        let mut session = Session::with_config(PathBuf::new(), SessionConfig::sandboxed_default());
        let err = session.compile_source(source).unwrap_err();

        match err {
            CompileError::Security(SecurityError::ImportDisallowed { module, .. }) => {
                assert_eq!(module, "sysutil");
            }
            other => panic!("expected import security error, got: {:?}", other),
        }
    }

    #[test]
    fn sandbox_env_blocks_dangerous_builtin() {
        let source = r#"
            fn main() { whereis("host") }
        "#;

        let mut session = Session::with_config(PathBuf::new(), SessionConfig::sandboxed_default());
        let err = session.compile_source(source).unwrap_err();

        match err {
            CompileError::Type(TypeError::UnboundVariable(name, _)) => {
                assert_eq!(name, "whereis");
            }
            other => panic!("expected unbound variable error, got: {:?}", other),
        }
    }
}
