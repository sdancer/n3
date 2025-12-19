use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::Path;
use std::process::{self, Command};

use lux::codegen::emit::Emitter;
use lux::codegen::translate::Translator;
use lux::syntax::lexer::Lexer;
use lux::syntax::parser::Parser;
use lux::types::env::TypeEnv;
use lux::types::infer::InferenceContext;
use lux::types::types::Scheme;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: lux [options] <file.lux>");
        eprintln!("Options:");
        eprintln!("  --emit-core    Output Core Erlang only (don't compile to BEAM)");
        eprintln!("  --parse-only   Parse only (don't generate code)");
        process::exit(1);
    }

    let mut emit_core_only = false;
    let mut parse_only = false;
    let mut filename = None;

    for arg in &args[1..] {
        match arg.as_str() {
            "--emit-core" => emit_core_only = true,
            "--parse-only" => parse_only = true,
            _ if !arg.starts_with('-') => filename = Some(arg.clone()),
            _ => {
                eprintln!("Unknown option: {}", arg);
                process::exit(1);
            }
        }
    }

    let filename = filename.unwrap_or_else(|| {
        eprintln!("No input file specified");
        process::exit(1);
    });

    let source = match fs::read_to_string(&filename) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {}: {}", filename, e);
            process::exit(1);
        }
    };

    // Lexing
    let tokens = Lexer::new(&source).tokenize();

    // Check for lexer errors
    for token in &tokens {
        if let lux::syntax::token::TokenKind::Error(msg) = &token.kind {
            eprintln!("Lexer error at {:?}: {}", token.span, msg);
            process::exit(1);
        }
    }

    // Parsing
    let mut parser = Parser::new(tokens);
    let module = match parser.parse_module() {
        Ok(m) => m,
        Err(e) => {
            eprintln!("Parse error at {:?}: {}", e.span, e.message);
            process::exit(1);
        }
    };

    let module_name = module.name.clone().unwrap_or_else(|| {
        Path::new(&filename)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("main")
            .to_string()
    });

    if parse_only {
        println!("Parsed module: {}", module_name);
        println!("Items: {}", module.items.len());
        for item in &module.items {
            match item {
                lux::syntax::ast::Item::Function(f) => {
                    println!("  fn {}/{}", f.name, f.params.len());
                }
                lux::syntax::ast::Item::Enum(e) => {
                    println!("  enum {} ({} variants)", e.name, e.variants.len());
                }
                lux::syntax::ast::Item::Struct(s) => {
                    println!("  struct {} ({} fields)", s.name, s.fields.len());
                }
                lux::syntax::ast::Item::TypeAlias(t) => {
                    println!("  type {}", t.name);
                }
                lux::syntax::ast::Item::Extern(e) => {
                    println!("  extern \"{}\" ({} decls)", e.abi, e.decls.len());
                }
                lux::syntax::ast::Item::Use(u) => {
                    match &u.items {
                        Some(items) => println!("  use {}::{{{}}}", u.module, items.join(", ")),
                        None => println!("  use {}", u.module),
                    }
                }
            }
        }
        return;
    }

    // Type checking
    let mut ctx = InferenceContext::new();
    let mut env = TypeEnv::new();

    // Register built-in functions
    use lux::types::types::Type;
    let any = Type::Any;
    let int = Type::Int;
    let float = Type::Float;
    let bool_ty = Type::Bool;
    let string = Type::String;
    let atom = Type::Atom;
    let unit = Type::Unit;
    let pid = Type::Pid;
    let ref_ty = Type::Ref;

    // Helper to register a built-in function
    let mut register_builtin = |name: &str, params: Vec<Type>, ret: Type| {
        let fn_type = Type::Function(params, Box::new(ret));
        env.insert(name.to_string(), Scheme::mono(fn_type));
    };

    // I/O functions
    register_builtin("print", vec![any.clone()], atom.clone());
    register_builtin("println", vec![any.clone()], atom.clone());
    register_builtin("dbg", vec![any.clone()], any.clone());

    // List functions
    register_builtin("length", vec![Type::List(Box::new(any.clone()))], int.clone());
    register_builtin("hd", vec![Type::List(Box::new(any.clone()))], any.clone());
    register_builtin("tl", vec![Type::List(Box::new(any.clone()))], Type::List(Box::new(any.clone())));
    register_builtin("reverse", vec![Type::List(Box::new(any.clone()))], Type::List(Box::new(any.clone())));
    register_builtin("sort", vec![Type::List(Box::new(any.clone()))], Type::List(Box::new(any.clone())));
    register_builtin("append", vec![Type::List(Box::new(any.clone())), Type::List(Box::new(any.clone()))], Type::List(Box::new(any.clone())));
    register_builtin("flatten", vec![Type::List(Box::new(any.clone()))], Type::List(Box::new(any.clone())));
    register_builtin("take", vec![Type::List(Box::new(any.clone())), int.clone()], Type::List(Box::new(any.clone())));
    register_builtin("drop", vec![Type::List(Box::new(any.clone())), int.clone()], Type::List(Box::new(any.clone())));
    register_builtin("nth", vec![Type::List(Box::new(any.clone())), int.clone()], any.clone());
    register_builtin("member", vec![any.clone(), Type::List(Box::new(any.clone()))], bool_ty.clone());
    register_builtin("unique", vec![Type::List(Box::new(any.clone()))], Type::List(Box::new(any.clone())));
    register_builtin("zip", vec![Type::List(Box::new(any.clone())), Type::List(Box::new(any.clone()))], Type::List(Box::new(any.clone())));
    register_builtin("unzip", vec![Type::List(Box::new(any.clone()))], Type::Tuple(vec![Type::List(Box::new(any.clone())), Type::List(Box::new(any.clone()))]));
    register_builtin("enumerate", vec![Type::List(Box::new(any.clone()))], Type::List(Box::new(any.clone())));

    // Math functions
    register_builtin("abs", vec![int.clone()], int.clone());
    register_builtin("max", vec![int.clone(), int.clone()], int.clone());
    register_builtin("min", vec![int.clone(), int.clone()], int.clone());
    register_builtin("rem", vec![int.clone(), int.clone()], int.clone());
    register_builtin("div", vec![int.clone(), int.clone()], int.clone());

    // Bitwise functions
    register_builtin("band", vec![int.clone(), int.clone()], int.clone());
    register_builtin("bor", vec![int.clone(), int.clone()], int.clone());
    register_builtin("bxor", vec![int.clone(), int.clone()], int.clone());
    register_builtin("bnot", vec![int.clone()], int.clone());
    register_builtin("bsl", vec![int.clone(), int.clone()], int.clone());
    register_builtin("bsr", vec![int.clone(), int.clone()], int.clone());

    // Type conversion functions
    register_builtin("to_string", vec![any.clone()], string.clone());
    register_builtin("to_int", vec![any.clone()], int.clone());
    register_builtin("to_float", vec![any.clone()], float.clone());
    register_builtin("to_atom", vec![string.clone()], atom.clone());

    // Tuple functions
    register_builtin("fst", vec![Type::Tuple(vec![any.clone(), any.clone()])], any.clone());
    register_builtin("snd", vec![Type::Tuple(vec![any.clone(), any.clone()])], any.clone());
    register_builtin("elem", vec![any.clone(), int.clone()], any.clone());
    register_builtin("set_elem", vec![any.clone(), int.clone(), any.clone()], any.clone());
    register_builtin("tuple_to_list", vec![any.clone()], Type::List(Box::new(any.clone())));
    register_builtin("list_to_tuple", vec![Type::List(Box::new(any.clone()))], any.clone());

    // Type checking functions
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

    // String functions
    register_builtin("str_length", vec![string.clone()], int.clone());
    register_builtin("str_concat", vec![string.clone(), string.clone()], string.clone());
    register_builtin("str_split", vec![string.clone(), string.clone()], Type::List(Box::new(string.clone())));
    register_builtin("str_join", vec![Type::List(Box::new(string.clone())), string.clone()], string.clone());
    register_builtin("str_trim", vec![string.clone()], string.clone());
    register_builtin("str_upper", vec![string.clone()], string.clone());
    register_builtin("str_lower", vec![string.clone()], string.clone());
    register_builtin("str_replace", vec![string.clone(), string.clone(), string.clone()], string.clone());
    register_builtin("str_contains", vec![string.clone(), string.clone()], bool_ty.clone());
    register_builtin("str_starts_with", vec![string.clone(), string.clone()], bool_ty.clone());
    register_builtin("str_ends_with", vec![string.clone(), string.clone()], bool_ty.clone());
    register_builtin("str_slice", vec![string.clone(), int.clone(), int.clone()], string.clone());
    register_builtin("str_char_at", vec![string.clone(), int.clone()], int.clone());
    register_builtin("chars", vec![string.clone()], Type::List(Box::new(int.clone())));
    register_builtin("str_from_chars", vec![Type::List(Box::new(int.clone()))], string.clone());

    // Map functions
    register_builtin("map_put", vec![Type::Map(Box::new(any.clone()), Box::new(any.clone())), any.clone(), any.clone()], Type::Map(Box::new(any.clone()), Box::new(any.clone())));
    register_builtin("map_get", vec![Type::Map(Box::new(any.clone()), Box::new(any.clone())), any.clone()], any.clone());
    register_builtin("map_remove", vec![Type::Map(Box::new(any.clone()), Box::new(any.clone())), any.clone()], Type::Map(Box::new(any.clone()), Box::new(any.clone())));
    register_builtin("map_keys", vec![Type::Map(Box::new(any.clone()), Box::new(any.clone()))], Type::List(Box::new(any.clone())));
    register_builtin("map_values", vec![Type::Map(Box::new(any.clone()), Box::new(any.clone()))], Type::List(Box::new(any.clone())));
    register_builtin("map_has_key", vec![Type::Map(Box::new(any.clone()), Box::new(any.clone())), any.clone()], bool_ty.clone());
    register_builtin("map_merge", vec![Type::Map(Box::new(any.clone()), Box::new(any.clone())), Type::Map(Box::new(any.clone()), Box::new(any.clone()))], Type::Map(Box::new(any.clone()), Box::new(any.clone())));
    register_builtin("map_size", vec![Type::Map(Box::new(any.clone()), Box::new(any.clone()))], int.clone());
    register_builtin("map_to_list", vec![Type::Map(Box::new(any.clone()), Box::new(any.clone()))], Type::List(Box::new(any.clone())));
    register_builtin("list_to_map", vec![Type::List(Box::new(any.clone()))], Type::Map(Box::new(any.clone()), Box::new(any.clone())));
    register_builtin("size", vec![any.clone()], int.clone());

    // Error handling
    register_builtin("throw", vec![any.clone()], Type::Never);
    register_builtin("exit", vec![any.clone()], Type::Never);
    register_builtin("error", vec![any.clone()], Type::Never);
    register_builtin("assert", vec![bool_ty.clone()], unit.clone());

    // Time functions
    register_builtin("sleep", vec![int.clone()], atom.clone());
    register_builtin("now", vec![], int.clone());
    register_builtin("monotonic_time", vec![], int.clone());

    // Random functions
    register_builtin("random", vec![], float.clone());
    register_builtin("random_seed", vec![], any.clone());

    // Process functions
    register_builtin("spawn_link", vec![Type::Function(vec![], Box::new(any.clone()))], pid.clone());
    register_builtin("link", vec![pid.clone()], bool_ty.clone());
    register_builtin("unlink", vec![pid.clone()], bool_ty.clone());
    register_builtin("monitor", vec![pid.clone()], ref_ty.clone());
    register_builtin("demonitor", vec![ref_ty.clone()], bool_ty.clone());
    register_builtin("registered", vec![], Type::List(Box::new(atom.clone())));
    register_builtin("register", vec![atom.clone(), pid.clone()], bool_ty.clone());
    register_builtin("whereis", vec![atom.clone()], pid.clone());
    register_builtin("make_ref", vec![], ref_ty.clone());

    // File I/O
    register_builtin("file_read", vec![string.clone()], any.clone());
    register_builtin("file_write", vec![string.clone(), string.clone()], atom.clone());
    register_builtin("file_exists", vec![string.clone()], bool_ty.clone());
    register_builtin("file_delete", vec![string.clone()], atom.clone());
    register_builtin("dir_list", vec![string.clone()], any.clone());
    register_builtin("dir_make", vec![string.clone()], atom.clone());
    register_builtin("get_cwd", vec![], any.clone());

    // OS functions
    register_builtin("argv", vec![], Type::List(Box::new(string.clone())));
    register_builtin("env", vec![string.clone()], string.clone());
    register_builtin("set_env", vec![string.clone(), string.clone()], bool_ty.clone());
    register_builtin("exit_code", vec![int.clone()], Type::Never);
    register_builtin("os_cmd", vec![string.clone()], string.clone());

    // Process dictionary
    register_builtin("get", vec![atom.clone()], any.clone());
    register_builtin("put", vec![atom.clone(), any.clone()], any.clone());
    register_builtin("erase", vec![atom.clone()], any.clone());

    // Binary/string conversion
    register_builtin("byte_size", vec![string.clone()], int.clone());
    register_builtin("bit_size", vec![string.clone()], int.clone());
    register_builtin("binary_to_list", vec![string.clone()], Type::List(Box::new(int.clone())));
    register_builtin("list_to_binary", vec![Type::List(Box::new(int.clone()))], string.clone());
    register_builtin("atom_to_list", vec![atom.clone()], Type::List(Box::new(int.clone())));
    register_builtin("list_to_atom", vec![Type::List(Box::new(int.clone()))], atom.clone());
    register_builtin("integer_to_list", vec![int.clone()], Type::List(Box::new(int.clone())));
    register_builtin("list_to_integer", vec![Type::List(Box::new(int.clone()))], int.clone());
    register_builtin("float_to_list", vec![float.clone()], Type::List(Box::new(int.clone())));
    register_builtin("list_to_float", vec![Type::List(Box::new(int.clone()))], float.clone());
    register_builtin("iolist_to_binary", vec![any.clone()], string.clone());
    register_builtin("term_to_binary", vec![any.clone()], string.clone());
    register_builtin("binary_to_term", vec![string.clone()], any.clone());

    // Apply function
    register_builtin("apply", vec![any.clone(), Type::List(Box::new(any.clone()))], any.clone());

    // Type introspection
    register_builtin("typeof", vec![any.clone()], atom.clone());

    // Register type definitions from module (enums, structs)
    // First pass: register all type names with empty fields (so types can reference each other)
    for item in &module.items {
        match item {
            lux::syntax::ast::Item::Enum(enum_def) => {
                // Register with empty variants first
                ctx.register_type(
                    enum_def.name.clone(),
                    enum_def.type_params.clone(),
                    vec![],
                );
            }
            lux::syntax::ast::Item::Struct(struct_def) => {
                // Register with empty fields first
                ctx.register_struct(
                    struct_def.name.clone(),
                    struct_def.type_params.clone(),
                    vec![],
                );
            }
            _ => {}
        }
    }

    // Second pass: fill in the actual field/variant types
    for item in &module.items {
        match item {
            lux::syntax::ast::Item::Enum(enum_def) => {
                // Build type parameter mapping
                let mut type_params_map = HashMap::new();
                for (i, param) in enum_def.type_params.iter().enumerate() {
                    type_params_map.insert(param.clone(), i as lux::types::types::TyVar);
                }

                let mut variants = Vec::new();
                for variant in &enum_def.variants {
                    let mut field_types = Vec::new();
                    for field_ty in &variant.fields {
                        match ctx.type_expr_to_type(field_ty, &type_params_map) {
                            Ok(ty) => field_types.push(ty),
                            Err(e) => {
                                eprintln!("Type error in enum {}: {:?}", enum_def.name, e);
                                process::exit(1);
                            }
                        }
                    }
                    variants.push((variant.name.clone(), field_types));
                }
                // Update the registered type with actual variants
                ctx.register_type(
                    enum_def.name.clone(),
                    enum_def.type_params.clone(),
                    variants,
                );
            }
            lux::syntax::ast::Item::Struct(struct_def) => {
                // Build type parameter mapping
                let mut type_params_map = HashMap::new();
                for (i, param) in struct_def.type_params.iter().enumerate() {
                    type_params_map.insert(param.clone(), i as lux::types::types::TyVar);
                }

                let mut fields = Vec::new();
                for field in &struct_def.fields {
                    match ctx.type_expr_to_type(&field.ty, &type_params_map) {
                        Ok(ty) => fields.push((field.name.clone(), ty)),
                        Err(e) => {
                            eprintln!("Type error in struct {}: {:?}", struct_def.name, e);
                            process::exit(1);
                        }
                    }
                }
                // Update the registered struct with actual fields
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
        if let lux::syntax::ast::Item::Extern(extern_block) = item {
            for extern_fn in &extern_block.decls {
                let mut type_params_map = HashMap::new();
                for (i, param) in extern_fn.type_params.iter().enumerate() {
                    type_params_map.insert(param.clone(), i as lux::types::types::TyVar);
                }

                let mut param_types = Vec::new();
                for param_ty in &extern_fn.params {
                    match ctx.type_expr_to_type(param_ty, &type_params_map) {
                        Ok(ty) => param_types.push(ty),
                        Err(e) => {
                            eprintln!("Type error in extern {}::{}: {:?}", extern_fn.module, extern_fn.name, e);
                            process::exit(1);
                        }
                    }
                }

                match ctx.type_expr_to_type(&extern_fn.return_type, &type_params_map) {
                    Ok(return_type) => {
                        ctx.register_extern_fn(
                            &extern_fn.module,
                            &extern_fn.name,
                            &extern_fn.type_params,
                            param_types,
                            return_type,
                        );
                    }
                    Err(e) => {
                        eprintln!("Type error in extern {}::{}: {:?}", extern_fn.module, extern_fn.name, e);
                        process::exit(1);
                    }
                }
            }
        }
    }

    // Register functions in environment
    for item in &module.items {
        if let lux::syntax::ast::Item::Function(func) = item {
            let mut param_types = Vec::new();
            for param in &func.params {
                let ty = if let Some(ty_expr) = &param.ty {
                    match ctx.type_expr_to_type(ty_expr, &HashMap::new()) {
                        Ok(ty) => ty,
                        Err(e) => {
                            eprintln!("Type error in function {}: {:?}", func.name, e);
                            process::exit(1);
                        }
                    }
                } else {
                    ctx.fresh_var()
                };
                param_types.push(ty);
            }
            let ret_type = if let Some(ret) = &func.return_type {
                match ctx.type_expr_to_type(ret, &HashMap::new()) {
                    Ok(ty) => ty,
                    Err(e) => {
                        eprintln!("Type error in function {}: {:?}", func.name, e);
                        process::exit(1);
                    }
                }
            } else {
                ctx.fresh_var()
            };

            let fn_type = lux::types::types::Type::Function(param_types, Box::new(ret_type));
            env.insert(func.name.clone(), Scheme::mono(fn_type));
        }
    }

    // Type check each function body
    for item in &module.items {
        if let lux::syntax::ast::Item::Function(func) = item {
            let mut func_env = env.clone();
            for param in &func.params {
                let param_type = if let Some(ty) = &param.ty {
                    match ctx.type_expr_to_type(ty, &HashMap::new()) {
                        Ok(ty) => ty,
                        Err(e) => {
                            eprintln!("Type error in function {}: {:?}", func.name, e);
                            process::exit(1);
                        }
                    }
                } else {
                    ctx.fresh_var()
                };
                func_env.insert(param.name.clone(), Scheme::mono(param_type));
            }

            match ctx.infer_expr(&func_env, &func.body) {
                Ok(body_type) => {
                    if let Some(ret) = &func.return_type {
                        match ctx.type_expr_to_type(ret, &HashMap::new()) {
                            Ok(ret_type) => {
                                if let Err(e) = ctx.unify(&body_type, &ret_type, func.span) {
                                    eprintln!("Type error in function {}: {:?}", func.name, e);
                                    process::exit(1);
                                }
                            }
                            Err(e) => {
                                eprintln!("Type error in function {}: {:?}", func.name, e);
                                process::exit(1);
                            }
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Type error in function {}: {:?}", func.name, e);
                    process::exit(1);
                }
            }
        }
    }

    // Translate to Core Erlang
    let mut translator = Translator::new();
    let core_module = translator.translate_module(&module);

    // Emit Core Erlang source
    let mut emitter = Emitter::new();
    let core_source = emitter.emit_module(&core_module);

    // Write .core file
    let core_filename = format!("{}.core", module_name);
    if let Err(e) = fs::write(&core_filename, &core_source) {
        eprintln!("Error writing {}: {}", core_filename, e);
        process::exit(1);
    }

    println!("Generated: {}", core_filename);

    if emit_core_only {
        // Print the Core Erlang source
        println!("\n{}", core_source);
        return;
    }

    // Compile with erlc
    println!("Compiling with erlc...");
    let status = Command::new("erlc")
        .arg("+from_core")
        .arg(&core_filename)
        .status();

    match status {
        Ok(s) if s.success() => {
            println!("Generated: {}.beam", module_name);
        }
        Ok(s) => {
            eprintln!("erlc failed with exit code: {:?}", s.code());
            process::exit(1);
        }
        Err(e) => {
            if e.kind() == std::io::ErrorKind::NotFound {
                eprintln!("erlc not found. Install Erlang/OTP to compile to BEAM.");
                eprintln!("Core Erlang output is in: {}", core_filename);
            } else {
                eprintln!("Error running erlc: {}", e);
            }
            process::exit(1);
        }
    }
}
