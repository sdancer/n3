use crate::codegen::erlang::*;
use crate::syntax::ast::{self, Expr, Item, Module, Pattern, Stmt};

/// Convert PascalCase to snake_case
fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            if i > 0 {
                result.push('_');
            }
            result.push(c.to_ascii_lowercase());
        } else {
            result.push(c);
        }
    }
    result
}

pub struct Translator {
    var_counter: u32,
    module_name: String,
    /// Track function names and arities for local calls
    functions: Vec<(String, usize)>,
}

impl Translator {
    pub fn new() -> Self {
        Translator {
            var_counter: 0,
            module_name: String::new(),
            functions: Vec::new(),
        }
    }

    fn fresh_var(&mut self) -> String {
        let v = self.var_counter;
        self.var_counter += 1;
        format!("_v{}", v)
    }

    pub fn translate_module(&mut self, module: &Module) -> CoreModule {
        self.module_name = module.name.clone().unwrap_or_else(|| "main".to_string());

        // First pass: collect all function names and arities
        self.functions.clear();
        for item in &module.items {
            if let Item::Function(func) = item {
                self.functions.push((func.name.clone(), func.params.len()));
            }
        }

        let mut functions = Vec::new();
        let mut exports = Vec::new();

        for item in &module.items {
            match item {
                Item::Function(func) => {
                    let core_func = self.translate_function(func);
                    exports.push((func.name.clone(), func.params.len()));
                    functions.push(core_func);
                }
                Item::Enum(_) => {
                    // Enums don't generate code directly - they're just type info
                }
                Item::TypeAlias(_) => {
                    // Type aliases don't generate code
                }
                Item::Extern(_) => {
                    // External declarations don't generate code
                }
            }
        }

        CoreModule {
            name: self.module_name.clone(),
            exports,
            functions,
        }
    }

    /// Check if a name is a local function and return its arity
    fn lookup_function(&self, name: &str) -> Option<usize> {
        self.functions.iter()
            .find(|(n, _)| n == name)
            .map(|(_, arity)| *arity)
    }

    fn translate_function(&mut self, func: &ast::Function) -> CoreFunDef {
        // Generate parameter names
        let params: Vec<String> = func
            .params
            .iter()
            .map(|p| self.to_core_var(&p.name))
            .collect();

        let body = self.translate_expr(&func.body);

        CoreFunDef {
            name: func.name.clone(),
            arity: func.params.len(),
            params,
            body,
        }
    }

    fn translate_expr(&mut self, expr: &Expr) -> CoreExpr {
        match expr {
            Expr::Int(n, _) => CoreExpr::Lit(CoreLit::Int(*n)),
            Expr::Float(f, _) => CoreExpr::Lit(CoreLit::Float(*f)),
            Expr::String(s, _) => CoreExpr::Lit(CoreLit::String(s.clone())),
            Expr::Bool(b, _) => CoreExpr::Lit(CoreLit::Atom(if *b { "true" } else { "false" }.into())),
            Expr::Atom(a, _) => CoreExpr::Lit(CoreLit::Atom(a.clone())),
            Expr::Unit(_) => CoreExpr::Lit(CoreLit::Atom("ok".into())),

            Expr::Var(name, _) => CoreExpr::Var(self.to_core_var(name)),

            Expr::Tuple(exprs, _) => {
                let elems: Vec<CoreExpr> = exprs.iter().map(|e| self.translate_expr(e)).collect();
                CoreExpr::Tuple(elems)
            }

            Expr::List(exprs, tail, _) => {
                let elems: Vec<CoreExpr> = exprs.iter().map(|e| self.translate_expr(e)).collect();
                let tail_expr = tail
                    .as_ref()
                    .map(|t| self.translate_expr(t))
                    .unwrap_or(CoreExpr::Lit(CoreLit::Nil));

                // Build list from end
                elems.into_iter().rev().fold(tail_expr, |acc, elem| {
                    CoreExpr::Cons(Box::new(elem), Box::new(acc))
                })
            }

            Expr::Binary(left, op, right, _) => {
                let l = self.translate_expr(left);
                let r = self.translate_expr(right);

                let (module, func) = match op {
                    ast::BinOp::Add => ("erlang", "+"),
                    ast::BinOp::Sub => ("erlang", "-"),
                    ast::BinOp::Mul => ("erlang", "*"),
                    ast::BinOp::Div => ("erlang", "div"),
                    ast::BinOp::Mod => ("erlang", "rem"),
                    ast::BinOp::Eq => ("erlang", "=:="),
                    ast::BinOp::NotEq => ("erlang", "=/="),
                    ast::BinOp::Lt => ("erlang", "<"),
                    ast::BinOp::LtEq => ("erlang", "=<"),
                    ast::BinOp::Gt => ("erlang", ">"),
                    ast::BinOp::GtEq => ("erlang", ">="),
                    ast::BinOp::And => ("erlang", "and"),
                    ast::BinOp::Or => ("erlang", "or"),
                    ast::BinOp::Pipe => {
                        // x |> f becomes f(x)
                        return CoreExpr::Apply(Box::new(r), vec![l]);
                    }
                };

                CoreExpr::Call(module.into(), func.into(), vec![l, r])
            }

            Expr::Unary(op, inner, _) => {
                let e = self.translate_expr(inner);
                match op {
                    ast::UnaryOp::Neg => CoreExpr::Call("erlang".into(), "-".into(), vec![e]),
                    ast::UnaryOp::Not => CoreExpr::Call("erlang".into(), "not".into(), vec![e]),
                }
            }

            Expr::If(cond, then_branch, else_branch, _) => {
                let cond_expr = self.translate_expr(cond);
                let then_expr = self.translate_expr(then_branch);
                let else_expr = else_branch
                    .as_ref()
                    .map(|e| self.translate_expr(e))
                    .unwrap_or(CoreExpr::Lit(CoreLit::Atom("ok".into())));

                CoreExpr::Case(
                    Box::new(cond_expr),
                    vec![
                        CoreClause {
                            patterns: vec![CorePattern::Lit(CoreLit::Atom("true".into()))],
                            guard: CoreExpr::Lit(CoreLit::Atom("true".into())),
                            body: then_expr,
                        },
                        CoreClause {
                            patterns: vec![CorePattern::Lit(CoreLit::Atom("false".into()))],
                            guard: CoreExpr::Lit(CoreLit::Atom("true".into())),
                            body: else_expr,
                        },
                    ],
                )
            }

            Expr::Match(scrutinee, arms, _) => {
                let scrut = self.translate_expr(scrutinee);
                let clauses: Vec<CoreClause> = arms
                    .iter()
                    .map(|arm| {
                        let pattern = self.translate_pattern(&arm.pattern);
                        let guard = arm
                            .guard
                            .as_ref()
                            .map(|g| self.translate_expr(g))
                            .unwrap_or(CoreExpr::Lit(CoreLit::Atom("true".into())));
                        let body = self.translate_expr(&arm.body);
                        CoreClause {
                            patterns: vec![pattern],
                            guard,
                            body,
                        }
                    })
                    .collect();

                CoreExpr::Case(Box::new(scrut), clauses)
            }

            Expr::Block(stmts, final_expr, _) => {
                self.translate_block(stmts, final_expr.as_deref())
            }

            Expr::Call(func, args, _) => {
                let arg_exprs: Vec<CoreExpr> = args.iter().map(|a| self.translate_expr(a)).collect();

                // Check if it's a direct function call (Var) or path
                match func.as_ref() {
                    Expr::Var(name, _) => {
                        // Check for built-in functions first
                        if name == "print" {
                            // print(x) -> io:format("~p~n", [x])
                            let format_str = CoreExpr::Lit(CoreLit::String("~p~n".to_string()));
                            let args_list = arg_exprs.into_iter().rev().fold(
                                CoreExpr::Lit(CoreLit::Nil),
                                |acc, elem| CoreExpr::Cons(Box::new(elem), Box::new(acc))
                            );
                            CoreExpr::Call("io".to_string(), "format".to_string(), vec![format_str, args_list])
                        } else if name == "println" {
                            // println(x) -> io:format("~p~n", [x]) (same as print for now)
                            let format_str = CoreExpr::Lit(CoreLit::String("~p~n".to_string()));
                            let args_list = arg_exprs.into_iter().rev().fold(
                                CoreExpr::Lit(CoreLit::Nil),
                                |acc, elem| CoreExpr::Cons(Box::new(elem), Box::new(acc))
                            );
                            CoreExpr::Call("io".to_string(), "format".to_string(), vec![format_str, args_list])
                        } else if self.lookup_function(name).is_some() {
                            // Local function call - use apply with local fun reference
                            CoreExpr::Apply(
                                Box::new(CoreExpr::LocalFunRef(name.clone(), args.len())),
                                arg_exprs
                            )
                        } else {
                            // Could be a variable holding a function
                            let func_expr = self.translate_expr(func);
                            CoreExpr::Apply(Box::new(func_expr), arg_exprs)
                        }
                    }
                    Expr::Path(parts, _) if parts.len() >= 2 => {
                        // Check if this looks like an enum constructor (Type::Variant)
                        // vs a module call (module:function)
                        let first = &parts[0];
                        let last = parts.last().unwrap();

                        // If the first part starts with uppercase, treat as enum constructor
                        if first.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                            // Enum constructor with args: Message::Get(pid) -> {get, pid}
                            let tag = to_snake_case(last);
                            let mut tuple_elems = vec![CoreExpr::Lit(CoreLit::Atom(tag))];
                            tuple_elems.extend(arg_exprs);
                            CoreExpr::Tuple(tuple_elems)
                        } else {
                            // Module:function call
                            CoreExpr::Call(parts[0].clone(), last.clone(), arg_exprs)
                        }
                    }
                    _ => {
                        let func_expr = self.translate_expr(func);
                        CoreExpr::Apply(Box::new(func_expr), arg_exprs)
                    }
                }
            }

            Expr::Lambda(params, _, body, _) => {
                let param_names: Vec<String> = params
                    .iter()
                    .map(|p| self.to_core_var(&p.name))
                    .collect();
                let body_expr = self.translate_expr(body);
                CoreExpr::Fun(param_names, Box::new(body_expr))
            }

            Expr::Spawn(thunk, _) => {
                let thunk_expr = self.translate_expr(thunk);
                // spawn/1 takes a fun() -> any()
                CoreExpr::Call("erlang".into(), "spawn".into(), vec![thunk_expr])
            }

            Expr::Send(pid, msg, _) => {
                let pid_expr = self.translate_expr(pid);
                let msg_expr = self.translate_expr(msg);
                // Use erlang:send/2 which is equivalent to Pid ! Msg
                CoreExpr::Call("erlang".into(), "send".into(), vec![pid_expr, msg_expr])
            }

            Expr::Receive { arms, timeout, .. } => {
                let clauses: Vec<CoreClause> = arms
                    .iter()
                    .map(|arm| {
                        let pattern = self.translate_pattern(&arm.pattern);
                        let guard = arm
                            .guard
                            .as_ref()
                            .map(|g| self.translate_expr(g))
                            .unwrap_or(CoreExpr::Lit(CoreLit::Atom("true".into())));
                        let body = self.translate_expr(&arm.body);
                        CoreClause {
                            patterns: vec![pattern],
                            guard,
                            body,
                        }
                    })
                    .collect();

                let timeout_expr = timeout.as_ref().map(|(ms, body)| {
                    (self.translate_expr(ms), self.translate_expr(body))
                });

                CoreExpr::Receive {
                    clauses,
                    timeout: timeout_expr.map(|(ms, body)| (Box::new(ms), Box::new(body))),
                }
            }

            Expr::SelfPid(_) => {
                CoreExpr::Call("erlang".into(), "self".into(), vec![])
            }

            Expr::Return(expr, _) => {
                expr.as_ref()
                    .map(|e| self.translate_expr(e))
                    .unwrap_or(CoreExpr::Lit(CoreLit::Atom("ok".into())))
            }

            Expr::Field(obj, field, _) => {
                // Record field access - translate to element/2 for tuples
                // or maps:get for maps. For now, assume tuple-based records
                let obj_expr = self.translate_expr(obj);
                // This is a simplification - real implementation needs type info
                CoreExpr::Call(
                    "erlang".into(),
                    "element".into(),
                    vec![
                        CoreExpr::Lit(CoreLit::Int(2)), // placeholder index
                        obj_expr,
                    ],
                )
            }

            Expr::Path(parts, _) => {
                // Path like Foo::Bar - could be a constructor or module path
                if parts.len() == 1 {
                    CoreExpr::Var(self.to_core_var(&parts[0]))
                } else {
                    // Enum constructor - translate to tuple {tag}
                    // e.g., Message::Inc becomes {inc}
                    let tag = parts.last().unwrap();
                    CoreExpr::Tuple(vec![CoreExpr::Lit(CoreLit::Atom(to_snake_case(tag)))])
                }
            }

            Expr::MethodCall(obj, method, args, _) => {
                // Method calls become function calls with obj as first arg
                let obj_expr = self.translate_expr(obj);
                let mut all_args = vec![obj_expr];
                all_args.extend(args.iter().map(|a| self.translate_expr(a)));

                CoreExpr::Apply(
                    Box::new(CoreExpr::Var(format!("'{}'/{}", method, all_args.len()))),
                    all_args,
                )
            }

            Expr::Record(fields, _) => {
                // Translate record to map
                let entries: Vec<CoreExpr> = fields
                    .iter()
                    .flat_map(|(name, expr)| {
                        vec![
                            CoreExpr::Lit(CoreLit::Atom(name.clone())),
                            self.translate_expr(expr),
                        ]
                    })
                    .collect();

                CoreExpr::Call("maps".into(), "from_list".into(), vec![
                    self.build_list(entries)
                ])
            }
        }
    }

    fn translate_block(&mut self, stmts: &[Stmt], final_expr: Option<&Expr>) -> CoreExpr {
        let mut bindings = Vec::new();

        for stmt in stmts {
            match stmt {
                Stmt::Let(name, _, init, _) => {
                    let init_expr = self.translate_expr(init);
                    bindings.push((self.to_core_var(name), init_expr));
                }
                Stmt::Expr(expr) => {
                    // Expression statement - bind to throwaway var
                    let var = self.fresh_var();
                    let expr = self.translate_expr(expr);
                    bindings.push((var, expr));
                }
            }
        }

        let body = final_expr
            .map(|e| self.translate_expr(e))
            .unwrap_or(CoreExpr::Lit(CoreLit::Atom("ok".into())));

        if bindings.is_empty() {
            body
        } else {
            // Build nested lets from inside out
            bindings.into_iter().rev().fold(body, |acc, (name, expr)| {
                CoreExpr::Let(vec![(name, expr)], Box::new(acc))
            })
        }
    }

    fn translate_pattern(&mut self, pattern: &Pattern) -> CorePattern {
        match pattern {
            Pattern::Wildcard(_) => CorePattern::Var("_".into()),
            Pattern::Var(name, _) => CorePattern::Var(self.to_core_var(name)),
            Pattern::Int(n, _) => CorePattern::Lit(CoreLit::Int(*n)),
            Pattern::Float(f, _) => CorePattern::Lit(CoreLit::Float(*f)),
            Pattern::String(s, _) => CorePattern::Lit(CoreLit::String(s.clone())),
            Pattern::Bool(b, _) => {
                CorePattern::Lit(CoreLit::Atom(if *b { "true" } else { "false" }.into()))
            }
            Pattern::Atom(a, _) => CorePattern::Lit(CoreLit::Atom(a.clone())),

            Pattern::Tuple(pats, _) => {
                let patterns: Vec<CorePattern> = pats
                    .iter()
                    .map(|p| self.translate_pattern(p))
                    .collect();
                CorePattern::Tuple(patterns)
            }

            Pattern::List(pats, tail, _) => {
                let tail_pat = tail
                    .as_ref()
                    .map(|t| self.translate_pattern(t))
                    .unwrap_or(CorePattern::Nil);

                pats.iter().rev().fold(tail_pat, |acc, pat| {
                    CorePattern::Cons(
                        Box::new(self.translate_pattern(pat)),
                        Box::new(acc),
                    )
                })
            }

            Pattern::Constructor(path, fields, _) => {
                // Constructor pattern - translate to tuple {tag, field1, field2, ...}
                // e.g., Message::Get(sender) -> {get, Sender}
                let tag = path.last().unwrap();
                let mut patterns = vec![CorePattern::Lit(CoreLit::Atom(to_snake_case(tag)))];
                patterns.extend(fields.iter().map(|p| self.translate_pattern(p)));
                CorePattern::Tuple(patterns)
            }

            Pattern::Record(fields, _) => {
                // Record pattern - for now just match as map
                // This is simplified - real impl needs proper map patterns
                let mut patterns = Vec::new();
                for (name, pat) in fields {
                    patterns.push(CorePattern::Tuple(vec![
                        CorePattern::Lit(CoreLit::Atom(name.clone())),
                        self.translate_pattern(pat),
                    ]));
                }
                CorePattern::Tuple(patterns)
            }

            Pattern::Or(left, right, _) => {
                // Or-patterns need special handling in Core Erlang
                // For now, just use the left pattern (simplified)
                self.translate_pattern(left)
            }
        }
    }

    fn to_core_var(&self, name: &str) -> String {
        // Core Erlang variables must start with uppercase
        let mut chars: Vec<char> = name.chars().collect();
        if let Some(first) = chars.first_mut() {
            *first = first.to_ascii_uppercase();
        }
        chars.into_iter().collect()
    }

    fn build_list(&self, exprs: Vec<CoreExpr>) -> CoreExpr {
        exprs.into_iter().rev().fold(
            CoreExpr::Lit(CoreLit::Nil),
            |acc, e| CoreExpr::Cons(Box::new(e), Box::new(acc)),
        )
    }
}

impl Default for Translator {
    fn default() -> Self {
        Self::new()
    }
}
