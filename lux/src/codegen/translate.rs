use crate::codegen::erlang::*;
use crate::syntax::ast::{self, Expr, InterpolatedPart, Item, Module, Pattern, Stmt};

/// Helper enum for let binding types
#[derive(Debug)]
enum LetPart {
    Simple(String),           // Simple variable binding
    Pattern(CorePattern),     // Pattern destructuring
}

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
            Expr::InterpolatedString(parts, _) => {
                // Build format string and arguments for io_lib:format
                let mut format_str = String::new();
                let mut args = Vec::new();

                for part in parts {
                    match part {
                        InterpolatedPart::Literal(s) => {
                            // Escape ~ characters in literals for io_lib:format
                            format_str.push_str(&s.replace('~', "~~"));
                        }
                        InterpolatedPart::Expr(e) => {
                            format_str.push_str("~p");
                            args.push(self.translate_expr(e));
                        }
                    }
                }

                // io_lib:format returns an iolist, wrap with lists:flatten to get string
                let format_expr = CoreExpr::Lit(CoreLit::String(format_str));
                let args_list = self.build_list(args);
                let io_list = CoreExpr::Call("io_lib".into(), "format".into(), vec![format_expr, args_list]);
                CoreExpr::Call("lists".into(), "flatten".into(), vec![io_list])
            }
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

            Expr::Map(entries, _) => {
                let core_entries: Vec<(CoreExpr, CoreExpr)> = entries
                    .iter()
                    .map(|(k, v)| (self.translate_expr(k), self.translate_expr(v)))
                    .collect();
                CoreExpr::Map(core_entries)
            }

            Expr::Range(start, end, inclusive, _) => {
                let start_expr = self.translate_expr(start);
                let end_expr = if *inclusive {
                    self.translate_expr(end)
                } else {
                    // For exclusive range, end - 1
                    CoreExpr::Call(
                        "erlang".into(),
                        "-".into(),
                        vec![
                            self.translate_expr(end),
                            CoreExpr::Lit(CoreLit::Int(1)),
                        ],
                    )
                };
                CoreExpr::Call("lists".into(), "seq".into(), vec![start_expr, end_expr])
            }

            Expr::ListComp { expr, generators, filters, .. } => {
                self.translate_list_comp(expr, generators, filters)
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
                    ast::BinOp::Concat => ("erlang", "++"),
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

            Expr::Try { body, catch_arms, .. } => {
                let translated_body = self.translate_expr(body);
                let success_var = self.fresh_var();
                let class_var = self.fresh_var();
                let reason_var = self.fresh_var();
                let stack_var = self.fresh_var();

                // Build catch body: case on {class, reason} to match catch arms
                let catch_body = if catch_arms.is_empty() {
                    // No catch arms - re-raise
                    CoreExpr::Primop(
                        "raise".into(),
                        vec![
                            CoreExpr::Var(stack_var.clone()),
                            CoreExpr::Var(reason_var.clone()),
                        ],
                    )
                } else {
                    // Build case expression for catch arms
                    let scrutinee = CoreExpr::Tuple(vec![
                        CoreExpr::Var(class_var.clone()),
                        CoreExpr::Var(reason_var.clone()),
                    ]);

                    let mut clauses: Vec<CoreClause> = catch_arms
                        .iter()
                        .map(|arm| {
                            let class_pattern = if let Some(ref class) = arm.class {
                                CorePattern::Lit(CoreLit::Atom(class.clone()))
                            } else {
                                CorePattern::Var("_".into())
                            };
                            let reason_pattern = self.translate_pattern(&arm.pattern);
                            CoreClause {
                                patterns: vec![CorePattern::Tuple(vec![
                                    class_pattern,
                                    reason_pattern,
                                ])],
                                guard: CoreExpr::Lit(CoreLit::Atom("true".into())),
                                body: self.translate_expr(&arm.body),
                            }
                        })
                        .collect();

                    // Add fallback clause that re-raises
                    clauses.push(CoreClause {
                        patterns: vec![CorePattern::Tuple(vec![
                            CorePattern::Var("_Class".into()),
                            CorePattern::Var("_Reason".into()),
                        ])],
                        guard: CoreExpr::Lit(CoreLit::Atom("true".into())),
                        body: CoreExpr::Primop(
                            "raise".into(),
                            vec![
                                CoreExpr::Var(stack_var.clone()),
                                CoreExpr::Var(reason_var.clone()),
                            ],
                        ),
                    });

                    CoreExpr::Case(Box::new(scrutinee), clauses)
                };

                CoreExpr::Try {
                    body: Box::new(translated_body),
                    vars: vec![success_var.clone()],
                    handler: Box::new(CoreExpr::Var(success_var)),
                    evars: vec![class_var, reason_var, stack_var],
                    catch: Box::new(catch_body),
                }
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
        let mut result_parts: Vec<(LetPart, CoreExpr)> = Vec::new();

        for stmt in stmts {
            match stmt {
                Stmt::Let(pattern, _, init, _) => {
                    let init_expr = self.translate_expr(init);
                    match pattern {
                        Pattern::Var(name, _) => {
                            // Simple variable binding
                            result_parts.push((LetPart::Simple(self.to_core_var(name)), init_expr));
                        }
                        Pattern::Wildcard(_) => {
                            // Wildcard - just evaluate for side effects
                            let var = self.fresh_var();
                            result_parts.push((LetPart::Simple(var), init_expr));
                        }
                        _ => {
                            // Pattern destructuring - use case expression
                            let core_pattern = self.translate_pattern(pattern);
                            result_parts.push((LetPart::Pattern(core_pattern), init_expr));
                        }
                    }
                }
                Stmt::Expr(expr) => {
                    // Expression statement - bind to throwaway var
                    let var = self.fresh_var();
                    let expr = self.translate_expr(expr);
                    result_parts.push((LetPart::Simple(var), expr));
                }
            }
        }

        let body = final_expr
            .map(|e| self.translate_expr(e))
            .unwrap_or(CoreExpr::Lit(CoreLit::Atom("ok".into())));

        if result_parts.is_empty() {
            body
        } else {
            // Build nested lets/cases from inside out
            result_parts.into_iter().rev().fold(body, |acc, (part, expr)| {
                match part {
                    LetPart::Simple(name) => {
                        CoreExpr::Let(vec![(name, expr)], Box::new(acc))
                    }
                    LetPart::Pattern(pattern) => {
                        // Use case expression for pattern destructuring
                        CoreExpr::Case(
                            Box::new(expr),
                            vec![CoreClause {
                                patterns: vec![pattern],
                                guard: CoreExpr::Lit(CoreLit::Atom("true".into())),
                                body: acc,
                            }],
                        )
                    }
                }
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

    /// Translate a list comprehension to Core Erlang
    /// [expr for x in list if cond] becomes:
    /// lists:filtermap(fun(X) -> case Cond of true -> {true, Expr}; false -> false end end, List)
    fn translate_list_comp(
        &mut self,
        expr: &Expr,
        generators: &[crate::syntax::ast::Generator],
        filters: &[Expr],
    ) -> CoreExpr {
        // For simplicity, handle single generator case with lists:filtermap
        // Multiple generators use nested flatmap

        if generators.is_empty() {
            // No generators - just return a list with the expr
            return CoreExpr::Cons(
                Box::new(self.translate_expr(expr)),
                Box::new(CoreExpr::Lit(CoreLit::Nil)),
            );
        }

        // Build from innermost generator outward
        self.translate_list_comp_inner(expr, generators, filters, 0, false)
    }

    fn translate_list_comp_inner(
        &mut self,
        expr: &Expr,
        generators: &[crate::syntax::ast::Generator],
        filters: &[Expr],
        gen_idx: usize,
        use_map: bool, // true if we can use map (single generator, no filters)
    ) -> CoreExpr {
        if gen_idx >= generators.len() {
            // All generators processed - apply filters and return expr
            let translated_expr = self.translate_expr(expr);

            if filters.is_empty() {
                if use_map {
                    // Just return the expression directly (used with lists:map)
                    translated_expr
                } else {
                    // Return expression in a singleton list (used with lists:flatmap)
                    CoreExpr::Cons(
                        Box::new(translated_expr),
                        Box::new(CoreExpr::Lit(CoreLit::Nil)),
                    )
                }
            } else {
                // Build combined filter condition
                let mut combined_filter = self.translate_expr(&filters[0]);
                for filter in &filters[1..] {
                    let f = self.translate_expr(filter);
                    combined_filter = CoreExpr::Call(
                        "erlang".into(),
                        "and".into(),
                        vec![combined_filter, f],
                    );
                }

                // case Filter of true -> [Expr]; false -> [] end
                CoreExpr::Case(
                    Box::new(combined_filter),
                    vec![
                        CoreClause {
                            patterns: vec![CorePattern::Lit(CoreLit::Atom("true".into()))],
                            guard: CoreExpr::Lit(CoreLit::Atom("true".into())),
                            body: CoreExpr::Cons(
                                Box::new(translated_expr),
                                Box::new(CoreExpr::Lit(CoreLit::Nil)),
                            ),
                        },
                        CoreClause {
                            patterns: vec![CorePattern::Lit(CoreLit::Atom("false".into()))],
                            guard: CoreExpr::Lit(CoreLit::Atom("true".into())),
                            body: CoreExpr::Lit(CoreLit::Nil),
                        },
                    ],
                )
            }
        } else {
            // Process this generator
            let generator = &generators[gen_idx];
            let source = self.translate_expr(&generator.source);
            let pattern = self.translate_pattern(&generator.pattern);

            // Check if this is the simple case: single generator, single variable pattern, no filters
            let is_simple_map = generators.len() == 1
                && filters.is_empty()
                && matches!(&generator.pattern, Pattern::Var(_, _));

            // Recurse for inner generators
            let inner = self.translate_list_comp_inner(expr, generators, filters, gen_idx + 1, is_simple_map);

            if is_simple_map {
                // Simple case: [expr for x in list] -> lists:map(fun(X) -> Expr end, List)
                let param_var = self.to_core_var(match &generator.pattern {
                    Pattern::Var(name, _) => name,
                    _ => unreachable!(),
                });
                let lambda = CoreExpr::Fun(vec![param_var], Box::new(inner));
                CoreExpr::Call("lists".into(), "map".into(), vec![lambda, source])
            } else {
                // Complex case: use flatmap with pattern matching
                let param_var = self.fresh_var();
                let lambda_body = CoreExpr::Case(
                    Box::new(CoreExpr::Var(param_var.clone())),
                    vec![
                        CoreClause {
                            patterns: vec![pattern],
                            guard: CoreExpr::Lit(CoreLit::Atom("true".into())),
                            body: inner,
                        },
                        // Default case for non-matching patterns (skip)
                        CoreClause {
                            patterns: vec![CorePattern::Var("_".into())],
                            guard: CoreExpr::Lit(CoreLit::Atom("true".into())),
                            body: CoreExpr::Lit(CoreLit::Nil),
                        },
                    ],
                );
                let lambda = CoreExpr::Fun(vec![param_var], Box::new(lambda_body));
                CoreExpr::Call("lists".into(), "flatmap".into(), vec![lambda, source])
            }
        }
    }
}

impl Default for Translator {
    fn default() -> Self {
        Self::new()
    }
}
