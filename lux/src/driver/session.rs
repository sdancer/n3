use std::path::PathBuf;

use crate::syntax::ast::Module;
use crate::syntax::lexer::Lexer;
use crate::syntax::parser::{Parser, ParseError};
use crate::types::infer::InferenceContext;
use crate::types::env::TypeEnv;
use crate::types::unify::TypeError;

pub struct Session {
    pub source_files: Vec<PathBuf>,
    pub output_dir: PathBuf,
    pub errors: Vec<CompileError>,
}

#[derive(Debug)]
pub enum CompileError {
    Parse(ParseError),
    Type(TypeError),
    Io(std::io::Error),
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

impl Session {
    pub fn new(output_dir: PathBuf) -> Self {
        Session {
            source_files: Vec::new(),
            output_dir,
            errors: Vec::new(),
        }
    }

    pub fn add_file(&mut self, path: PathBuf) {
        self.source_files.push(path);
    }

    pub fn compile_source(&mut self, source: &str) -> Result<Module, CompileError> {
        // Lex
        let tokens = Lexer::new(source).tokenize();

        // Parse
        let mut parser = Parser::new(tokens);
        let module = parser.parse_module()?;

        // Type check
        let mut ctx = InferenceContext::new();
        let mut env = TypeEnv::new();

        // Register type definitions from module (enums, structs)
        for item in &module.items {
            match item {
                crate::syntax::ast::Item::Enum(enum_def) => {
                    // Convert variants to type format
                    let mut variants = Vec::new();
                    for variant in &enum_def.variants {
                        // Convert variant field types
                        let mut field_types = Vec::new();
                        for field_ty in &variant.fields {
                            let ty = ctx.type_expr_to_type(field_ty, &std::collections::HashMap::new())?;
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
                crate::syntax::ast::Item::Struct(struct_def) => {
                    // Convert struct fields to type format
                    let mut fields = Vec::new();
                    for field in &struct_def.fields {
                        let ty = ctx.type_expr_to_type(&field.ty, &std::collections::HashMap::new())?;
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

        // Register extern function declarations
        for item in &module.items {
            if let crate::syntax::ast::Item::Extern(extern_block) = item {
                for extern_fn in &extern_block.decls {
                    // Build type parameter mapping
                    let mut type_params_map = std::collections::HashMap::new();
                    for (i, param) in extern_fn.type_params.iter().enumerate() {
                        type_params_map.insert(param.clone(), i as crate::types::types::TyVar);
                    }

                    // Convert parameter types
                    let mut param_types = Vec::new();
                    for param_ty in &extern_fn.params {
                        let ty = ctx.type_expr_to_type(param_ty, &type_params_map)?;
                        param_types.push(ty);
                    }

                    // Convert return type
                    let return_type = ctx.type_expr_to_type(&extern_fn.return_type, &type_params_map)?;

                    // Register the extern function
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

        // Register functions in environment so they can call each other
        for item in &module.items {
            if let crate::syntax::ast::Item::Function(func) = item {
                // Build function type from signature
                let mut param_types = Vec::new();
                for param in &func.params {
                    let ty = if let Some(ty_expr) = &param.ty {
                        ctx.type_expr_to_type(ty_expr, &std::collections::HashMap::new())?
                    } else {
                        ctx.fresh_var()
                    };
                    param_types.push(ty);
                }
                let ret_type = if let Some(ret) = &func.return_type {
                    ctx.type_expr_to_type(ret, &std::collections::HashMap::new())?
                } else {
                    ctx.fresh_var()
                };

                let fn_type = crate::types::types::Type::Function(param_types, Box::new(ret_type));
                env.insert(func.name.clone(), crate::types::types::Scheme::mono(fn_type));
            }
        }

        // Type check each function
        for item in &module.items {
            if let crate::syntax::ast::Item::Function(func) = item {
                // Create environment with function parameters
                let mut func_env = env.clone();
                for param in &func.params {
                    let param_type = if let Some(ty) = &param.ty {
                        ctx.type_expr_to_type(ty, &std::collections::HashMap::new())?
                    } else {
                        ctx.fresh_var()
                    };
                    func_env.insert(
                        param.name.clone(),
                        crate::types::types::Scheme::mono(param_type),
                    );
                }

                // Infer body type
                let body_type = ctx.infer_expr(&func_env, &func.body)?;

                // Check against declared return type
                if let Some(ret) = &func.return_type {
                    let ret_type = ctx.type_expr_to_type(ret, &std::collections::HashMap::new())?;
                    ctx.unify(&body_type, &ret_type, func.span)?;
                }
            }
        }

        Ok(module)
    }
}
