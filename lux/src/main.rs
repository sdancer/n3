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
    let translated = translator.translate_function_modules(&module);
    let mut core_outputs: Vec<(String, String)> = Vec::new();
    for core_module in &translated.modules {
        let mut emitter = Emitter::new();
        let core_source = emitter.emit_module(core_module);
        let core_filename = format!("{}.core", core_module.name);
        if let Err(e) = fs::write(&core_filename, &core_source) {
            eprintln!("Error writing {}: {}", core_filename, e);
            process::exit(1);
        }
        println!("Generated: {}", core_filename);
        core_outputs.push((core_filename, core_source));
    }
    if core_outputs.is_empty() {
        eprintln!("No functions to compile");
        process::exit(1);
    }

    let metadata_filename = format!("{}.meta.json", module_name);
    let metadata_json = build_metadata_json(&module_name, &translated);
    if let Err(e) = fs::write(&metadata_filename, metadata_json) {
        eprintln!("Error writing {}: {}", metadata_filename, e);
        process::exit(1);
    }
    println!("Generated: {}", metadata_filename);

    if emit_core_only {
        for (core_filename, core_source) in &core_outputs {
            println!("\n// {}\n{}", core_filename, core_source);
        }
        return;
    }

    println!("Compiling with erlc...");
    let mut command = Command::new("erlc");
    command.arg("+from_core");
    for (core_filename, _) in &core_outputs {
        command.arg(core_filename);
    }
    let status = command.status();

    match status {
        Ok(s) if s.success() => {
            for (core_filename, _) in &core_outputs {
                let beam_filename = core_filename.replace(".core", ".beam");
                println!("Generated: {}", beam_filename);
            }
        }
        Ok(s) => {
            eprintln!("erlc failed with exit code: {:?}", s.code());
            process::exit(1);
        }
        Err(e) => {
            if e.kind() == std::io::ErrorKind::NotFound {
                eprintln!("erlc not found. Install Erlang/OTP to compile to BEAM.");
                eprintln!("Core Erlang outputs:");
                for (core_filename, _) in &core_outputs {
                    eprintln!("  {}", core_filename);
                }
            } else {
                eprintln!("Error running erlc: {}", e);
            }
            process::exit(1);
        }
    }
}

fn json_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(c),
        }
    }
    out
}

fn build_metadata_json(
    source_module: &str,
    translated: &lux::codegen::translate::TranslatedFunctionModules,
) -> String {
    let mut json = String::new();
    json.push_str("{\n");
    json.push_str(&format!(
        "  \"source_module\": \"{}\",\n",
        json_escape(source_module)
    ));
    match (&translated.entry_module, translated.entry_arity) {
        (Some(module), Some(arity)) => {
            json.push_str("  \"entry\": {\n");
            json.push_str(&format!("    \"module\": \"{}\",\n", json_escape(module)));
            json.push_str("    \"function\": \"apply\",\n");
            json.push_str(&format!("    \"arity\": {}\n", arity));
            json.push_str("  },\n");
        }
        _ => {
            json.push_str("  \"entry\": null,\n");
        }
    }
    json.push_str("  \"functions\": [\n");
    for (i, item) in translated.metadata.iter().enumerate() {
        json.push_str("    {\n");
        json.push_str(&format!(
            "      \"source_name\": \"{}\",\n",
            json_escape(&item.source_name)
        ));
        json.push_str(&format!(
            "      \"module\": \"{}\",\n",
            json_escape(&item.module_name)
        ));
        json.push_str("      \"function\": \"apply\",\n");
        json.push_str(&format!("      \"arity\": {},\n", item.arity));
        json.push_str(&format!(
            "      \"hash\": \"{}\"\n",
            json_escape(&item.module_hash)
        ));
        json.push_str("    }");
        if i + 1 < translated.metadata.len() {
            json.push(',');
        }
        json.push('\n');
    }
    json.push_str("  ]\n");
    json.push('}');
    json.push('\n');
    json
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
