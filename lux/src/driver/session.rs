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
        let env = TypeEnv::new();

        // TODO: Register type definitions from module

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
