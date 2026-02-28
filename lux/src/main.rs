use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{self, Command};

use lux::codegen::emit::Emitter;
use lux::codegen::translate::Translator;
use lux::driver::session::{CompileError, SecurityError, Session, SessionConfig};
use lux::syntax::lexer::Lexer;
use lux::syntax::parser::{Parser, ParserOptions};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: lux [options] <file.lux>");
        eprintln!("Options:");
        eprintln!("  --emit-core    Output Core Erlang only (don't compile to BEAM)");
        eprintln!("  --parse-only   Parse only (don't generate code)");
        eprintln!("  --sandbox      Compile with sandbox restrictions");
        process::exit(1);
    }

    let mut emit_core_only = false;
    let mut parse_only = false;
    let mut sandbox = false;
    let mut filename = None;

    for arg in &args[1..] {
        match arg.as_str() {
            "--emit-core" => emit_core_only = true,
            "--parse-only" => parse_only = true,
            "--sandbox" => sandbox = true,
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

    if parse_only {
        let tokens = Lexer::new(&source).tokenize();
        for token in &tokens {
            if let lux::syntax::token::TokenKind::Error(msg) = &token.kind {
                eprintln!("Lexer error at {:?}: {}", token.span, msg);
                process::exit(1);
            }
        }

        let mut parser = Parser::new_with_options(
            tokens,
            ParserOptions {
                allow_extern: !sandbox,
            },
        );
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
                lux::syntax::ast::Item::Use(u) => match &u.items {
                    Some(items) => println!("  use {}::{{{}}}", u.module, items.join(", ")),
                    None => println!("  use {}", u.module),
                },
            }
        }
        return;
    }

    let config = if sandbox {
        SessionConfig::sandboxed_default()
    } else {
        SessionConfig::trusted()
    };
    let mut session = Session::with_config(PathBuf::new(), config);

    let module = match session.compile_source(&source) {
        Ok(m) => m,
        Err(err) => {
            print_compile_error(err);
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

    let mut translator = Translator::new();
    let core_module = translator.translate_module(&module);

    let mut emitter = Emitter::new();
    let core_source = emitter.emit_module(&core_module);

    let core_filename = format!("{}.core", module_name);
    if let Err(e) = fs::write(&core_filename, &core_source) {
        eprintln!("Error writing {}: {}", core_filename, e);
        process::exit(1);
    }

    println!("Generated: {}", core_filename);

    if emit_core_only {
        println!("\n{}", core_source);
        return;
    }

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

fn print_compile_error(err: CompileError) {
    match err {
        CompileError::Parse(e) => {
            eprintln!("Parse error at {:?}: {}", e.span, e.message);
        }
        CompileError::Type(e) => {
            eprintln!("Type error: {}", e);
        }
        CompileError::Io(e) => {
            eprintln!("I/O error: {}", e);
        }
        CompileError::Security(SecurityError::ExternDisallowed(span)) => {
            eprintln!(
                "Security error at {:?}: external declarations are disabled in sandbox mode",
                span
            );
        }
        CompileError::Security(SecurityError::ImportDisallowed { module, span }) => {
            eprintln!(
                "Security error at {:?}: import '{}' is not allowed in sandbox mode",
                span, module
            );
        }
    }
}
