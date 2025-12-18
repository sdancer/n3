use std::env;
use std::fs;
use std::path::Path;
use std::process::{self, Command};

use lux::codegen::emit::Emitter;
use lux::codegen::translate::Translator;
use lux::syntax::lexer::Lexer;
use lux::syntax::parser::Parser;

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
