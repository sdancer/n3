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
                Item::Struct(_) => {
                    // Structs don't generate code directly - they're just type info
                }
                Item::TypeAlias(_) => {
                    // Type aliases don't generate code
                }
                Item::Extern(_) => {
                    // External declarations don't generate code
                }
                Item::Use(_) => {
                    // Use declarations are resolved at compile time
                    // They inform function resolution but don't generate code
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
                        // If right side is a function name, use LocalFunRef or Call
                        if let Expr::Var(name, _) = right.as_ref() {
                            // Check if it's a built-in function
                            if matches!(name.as_str(), "length" | "hd" | "tl" | "reverse" | "sort" | "flatten" | "abs") {
                                let (module, func) = match name.as_str() {
                                    "length" | "hd" | "tl" | "abs" => ("erlang", name.as_str()),
                                    _ => ("lists", name.as_str()),
                                };
                                return CoreExpr::Call(module.into(), func.into(), vec![l]);
                            } else if self.lookup_function(name).is_some() {
                                return CoreExpr::Apply(
                                    Box::new(CoreExpr::LocalFunRef(name.clone(), 1)),
                                    vec![l]
                                );
                            }
                        }
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

            Expr::Cond(arms, _) => {
                // Translate cond to nested if-else
                // cond { c1 => b1, c2 => b2, _ => b3 }
                // becomes: case c1 of true -> b1; false -> case c2 of true -> b2; false -> b3
                if arms.is_empty() {
                    return CoreExpr::Lit(CoreLit::Atom("ok".into()));
                }

                // Build from the end
                let mut result = CoreExpr::Lit(CoreLit::Atom("nil".into())); // fallback
                for (cond, body) in arms.iter().rev() {
                    let cond_expr = self.translate_expr(cond);
                    let body_expr = self.translate_expr(body);

                    // Check if condition is a wildcard pattern (variable named "_" or literal true)
                    let is_else = match cond {
                        Expr::Var(name, _) if name == "_" => true,
                        Expr::Bool(true, _) => true,
                        _ => false,
                    };

                    if is_else {
                        result = body_expr;
                    } else {
                        result = CoreExpr::Case(
                            Box::new(cond_expr),
                            vec![
                                CoreClause {
                                    patterns: vec![CorePattern::Lit(CoreLit::Atom("true".into()))],
                                    guard: CoreExpr::Lit(CoreLit::Atom("true".into())),
                                    body: body_expr,
                                },
                                CoreClause {
                                    patterns: vec![CorePattern::Lit(CoreLit::Atom("false".into()))],
                                    guard: CoreExpr::Lit(CoreLit::Atom("true".into())),
                                    body: result,
                                },
                            ],
                        );
                    }
                }
                result
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
                        } else if name == "length" && arg_exprs.len() == 1 {
                            // length(list) -> erlang:length(list)
                            CoreExpr::Call("erlang".to_string(), "length".to_string(), arg_exprs)
                        } else if name == "hd" && arg_exprs.len() == 1 {
                            // hd(list) -> erlang:hd(list)
                            CoreExpr::Call("erlang".to_string(), "hd".to_string(), arg_exprs)
                        } else if name == "tl" && arg_exprs.len() == 1 {
                            // tl(list) -> erlang:tl(list)
                            CoreExpr::Call("erlang".to_string(), "tl".to_string(), arg_exprs)
                        } else if name == "abs" && arg_exprs.len() == 1 {
                            // abs(n) -> erlang:abs(n)
                            CoreExpr::Call("erlang".to_string(), "abs".to_string(), arg_exprs)
                        } else if name == "max" && arg_exprs.len() == 2 {
                            // max(a, b) -> erlang:max(a, b)
                            CoreExpr::Call("erlang".to_string(), "max".to_string(), arg_exprs)
                        } else if name == "min" && arg_exprs.len() == 2 {
                            // min(a, b) -> erlang:min(a, b)
                            CoreExpr::Call("erlang".to_string(), "min".to_string(), arg_exprs)
                        } else if name == "reverse" && arg_exprs.len() == 1 {
                            // reverse(list) -> lists:reverse(list)
                            CoreExpr::Call("lists".to_string(), "reverse".to_string(), arg_exprs)
                        } else if name == "sort" && arg_exprs.len() == 1 {
                            // sort(list) -> lists:sort(list)
                            CoreExpr::Call("lists".to_string(), "sort".to_string(), arg_exprs)
                        } else if name == "append" && arg_exprs.len() == 2 {
                            // append(a, b) -> lists:append(a, b)
                            CoreExpr::Call("lists".to_string(), "append".to_string(), arg_exprs)
                        } else if name == "flatten" && arg_exprs.len() == 1 {
                            // flatten(list) -> lists:flatten(list)
                            CoreExpr::Call("lists".to_string(), "flatten".to_string(), arg_exprs)
                        } else if name == "to_string" && arg_exprs.len() == 1 {
                            // to_string(x) -> io_lib:format("~p", [x]) |> lists:flatten
                            let format_str = CoreExpr::Lit(CoreLit::String("~p".to_string()));
                            let args_list = CoreExpr::Cons(
                                Box::new(arg_exprs.into_iter().next().unwrap()),
                                Box::new(CoreExpr::Lit(CoreLit::Nil))
                            );
                            let io_list = CoreExpr::Call("io_lib".to_string(), "format".to_string(), vec![format_str, args_list]);
                            CoreExpr::Call("lists".to_string(), "flatten".to_string(), vec![io_list])
                        } else if name == "to_int" && arg_exprs.len() == 1 {
                            // to_int(string) -> list_to_integer(string)
                            CoreExpr::Call("erlang".to_string(), "list_to_integer".to_string(), arg_exprs)
                        } else if name == "to_float" && arg_exprs.len() == 1 {
                            // to_float(string) -> list_to_float(string)
                            CoreExpr::Call("erlang".to_string(), "list_to_float".to_string(), arg_exprs)
                        } else if name == "to_atom" && arg_exprs.len() == 1 {
                            // to_atom(string) -> list_to_atom(string)
                            CoreExpr::Call("erlang".to_string(), "list_to_atom".to_string(), arg_exprs)
                        } else if name == "fst" && arg_exprs.len() == 1 {
                            // fst(tuple) -> element(1, tuple)
                            CoreExpr::Call("erlang".to_string(), "element".to_string(),
                                vec![CoreExpr::Lit(CoreLit::Int(1)), arg_exprs.into_iter().next().unwrap()])
                        } else if name == "snd" && arg_exprs.len() == 1 {
                            // snd(tuple) -> element(2, tuple)
                            CoreExpr::Call("erlang".to_string(), "element".to_string(),
                                vec![CoreExpr::Lit(CoreLit::Int(2)), arg_exprs.into_iter().next().unwrap()])
                        } else if name == "size" && arg_exprs.len() == 1 {
                            // size(tuple) -> tuple_size(tuple)
                            CoreExpr::Call("erlang".to_string(), "tuple_size".to_string(), arg_exprs)
                        } else if name == "throw" && arg_exprs.len() == 1 {
                            // throw(term) -> erlang:throw(term)
                            CoreExpr::Call("erlang".to_string(), "throw".to_string(), arg_exprs)
                        } else if name == "exit" && arg_exprs.len() == 1 {
                            // exit(reason) -> erlang:exit(reason)
                            CoreExpr::Call("erlang".to_string(), "exit".to_string(), arg_exprs)
                        } else if name == "error" && arg_exprs.len() == 1 {
                            // error(reason) -> erlang:error(reason)
                            CoreExpr::Call("erlang".to_string(), "error".to_string(), arg_exprs)
                        } else if name == "band" && arg_exprs.len() == 2 {
                            // band(a, b) -> erlang:band(a, b)
                            CoreExpr::Call("erlang".to_string(), "band".to_string(), arg_exprs)
                        } else if name == "bor" && arg_exprs.len() == 2 {
                            // bor(a, b) -> erlang:bor(a, b)
                            CoreExpr::Call("erlang".to_string(), "bor".to_string(), arg_exprs)
                        } else if name == "bxor" && arg_exprs.len() == 2 {
                            // bxor(a, b) -> erlang:bxor(a, b)
                            CoreExpr::Call("erlang".to_string(), "bxor".to_string(), arg_exprs)
                        } else if name == "bnot" && arg_exprs.len() == 1 {
                            // bnot(x) -> erlang:bnot(x)
                            CoreExpr::Call("erlang".to_string(), "bnot".to_string(), arg_exprs)
                        } else if name == "bsl" && arg_exprs.len() == 2 {
                            // bsl(n, shift) -> erlang:bsl(n, shift)
                            CoreExpr::Call("erlang".to_string(), "bsl".to_string(), arg_exprs)
                        } else if name == "bsr" && arg_exprs.len() == 2 {
                            // bsr(n, shift) -> erlang:bsr(n, shift)
                            CoreExpr::Call("erlang".to_string(), "bsr".to_string(), arg_exprs)
                        } else if name == "rem" && arg_exprs.len() == 2 {
                            // rem(a, b) -> erlang:rem(a, b) (alternative to %)
                            CoreExpr::Call("erlang".to_string(), "rem".to_string(), arg_exprs)
                        } else if name == "div" && arg_exprs.len() == 2 {
                            // div(a, b) -> erlang:div(a, b) (integer division)
                            CoreExpr::Call("erlang".to_string(), "div".to_string(), arg_exprs)
                        } else if name == "is_int" && arg_exprs.len() == 1 {
                            CoreExpr::Call("erlang".to_string(), "is_integer".to_string(), arg_exprs)
                        } else if name == "is_float" && arg_exprs.len() == 1 {
                            CoreExpr::Call("erlang".to_string(), "is_float".to_string(), arg_exprs)
                        } else if name == "is_number" && arg_exprs.len() == 1 {
                            CoreExpr::Call("erlang".to_string(), "is_number".to_string(), arg_exprs)
                        } else if name == "is_atom" && arg_exprs.len() == 1 {
                            CoreExpr::Call("erlang".to_string(), "is_atom".to_string(), arg_exprs)
                        } else if name == "is_list" && arg_exprs.len() == 1 {
                            CoreExpr::Call("erlang".to_string(), "is_list".to_string(), arg_exprs)
                        } else if name == "is_tuple" && arg_exprs.len() == 1 {
                            CoreExpr::Call("erlang".to_string(), "is_tuple".to_string(), arg_exprs)
                        } else if name == "is_map" && arg_exprs.len() == 1 {
                            CoreExpr::Call("erlang".to_string(), "is_map".to_string(), arg_exprs)
                        } else if name == "is_bool" && arg_exprs.len() == 1 {
                            CoreExpr::Call("erlang".to_string(), "is_boolean".to_string(), arg_exprs)
                        } else if name == "is_string" && arg_exprs.len() == 1 {
                            // In Erlang, strings are lists of integers
                            CoreExpr::Call("erlang".to_string(), "is_list".to_string(), arg_exprs)
                        } else if name == "is_pid" && arg_exprs.len() == 1 {
                            CoreExpr::Call("erlang".to_string(), "is_pid".to_string(), arg_exprs)
                        } else if name == "is_function" && arg_exprs.len() == 1 {
                            CoreExpr::Call("erlang".to_string(), "is_function".to_string(), arg_exprs)
                        } else if name == "assert" && arg_exprs.len() == 1 {
                            // assert(cond) -> case cond of true -> ok; false -> error(assertion_failed)
                            let cond = arg_exprs.into_iter().next().unwrap();
                            CoreExpr::Case(
                                Box::new(cond),
                                vec![
                                    CoreClause {
                                        patterns: vec![CorePattern::Lit(CoreLit::Atom("true".into()))],
                                        guard: CoreExpr::Lit(CoreLit::Atom("true".into())),
                                        body: CoreExpr::Lit(CoreLit::Atom("ok".into())),
                                    },
                                    CoreClause {
                                        patterns: vec![CorePattern::Lit(CoreLit::Atom("false".into()))],
                                        guard: CoreExpr::Lit(CoreLit::Atom("true".into())),
                                        body: CoreExpr::Call("erlang".into(), "error".into(),
                                            vec![CoreExpr::Lit(CoreLit::Atom("assertion_failed".into()))]),
                                    },
                                ],
                            )
                        } else if name == "dbg" && arg_exprs.len() == 1 {
                            // dbg(x) -> io:format("DBG: ~p~n", [x]), x
                            let x = arg_exprs.into_iter().next().unwrap();
                            let tmp = self.fresh_var();
                            let format_str = CoreExpr::Lit(CoreLit::String("DBG: ~p~n".to_string()));
                            let args_list = CoreExpr::Cons(
                                Box::new(CoreExpr::Var(tmp.clone())),
                                Box::new(CoreExpr::Lit(CoreLit::Nil))
                            );
                            let print_call = CoreExpr::Call("io".into(), "format".into(), vec![format_str, args_list]);
                            CoreExpr::Let(
                                vec![(tmp.clone(), x)],
                                Box::new(CoreExpr::Seq(
                                    Box::new(print_call),
                                    Box::new(CoreExpr::Var(tmp))
                                ))
                            )
                        } else if name == "sleep" && arg_exprs.len() == 1 {
                            // sleep(ms) -> timer:sleep(ms)
                            CoreExpr::Call("timer".to_string(), "sleep".to_string(), arg_exprs)
                        } else if name == "now" && arg_exprs.is_empty() {
                            // now() -> erlang:system_time(millisecond)
                            CoreExpr::Call("erlang".to_string(), "system_time".to_string(),
                                vec![CoreExpr::Lit(CoreLit::Atom("millisecond".into()))])
                        } else if name == "monotonic_time" && arg_exprs.is_empty() {
                            // monotonic_time() -> erlang:monotonic_time(millisecond)
                            CoreExpr::Call("erlang".to_string(), "monotonic_time".to_string(),
                                vec![CoreExpr::Lit(CoreLit::Atom("millisecond".into()))])
                        } else if name == "random" && arg_exprs.is_empty() {
                            // random() -> rand:uniform()
                            CoreExpr::Call("rand".to_string(), "uniform".to_string(), vec![])
                        } else if name == "random" && arg_exprs.len() == 1 {
                            // random(n) -> rand:uniform(n)
                            CoreExpr::Call("rand".to_string(), "uniform".to_string(), arg_exprs)
                        } else if name == "random_seed" && arg_exprs.is_empty() {
                            // random_seed() -> rand:seed(exsss)
                            CoreExpr::Call("rand".to_string(), "seed".to_string(),
                                vec![CoreExpr::Lit(CoreLit::Atom("exsss".into()))])
                        } else if name == "spawn_link" && arg_exprs.len() == 1 {
                            // spawn_link(fun) -> erlang:spawn_link(fun)
                            CoreExpr::Call("erlang".to_string(), "spawn_link".to_string(), arg_exprs)
                        } else if name == "link" && arg_exprs.len() == 1 {
                            // link(pid) -> erlang:link(pid)
                            CoreExpr::Call("erlang".to_string(), "link".to_string(), arg_exprs)
                        } else if name == "unlink" && arg_exprs.len() == 1 {
                            // unlink(pid) -> erlang:unlink(pid)
                            CoreExpr::Call("erlang".to_string(), "unlink".to_string(), arg_exprs)
                        } else if name == "monitor" && arg_exprs.len() == 1 {
                            // monitor(pid) -> erlang:monitor(process, pid)
                            CoreExpr::Call("erlang".to_string(), "monitor".to_string(),
                                vec![CoreExpr::Lit(CoreLit::Atom("process".into())), arg_exprs.into_iter().next().unwrap()])
                        } else if name == "demonitor" && arg_exprs.len() == 1 {
                            // demonitor(ref) -> erlang:demonitor(ref)
                            CoreExpr::Call("erlang".to_string(), "demonitor".to_string(), arg_exprs)
                        } else if name == "registered" && arg_exprs.is_empty() {
                            // registered() -> erlang:registered()
                            CoreExpr::Call("erlang".to_string(), "registered".to_string(), vec![])
                        } else if name == "register" && arg_exprs.len() == 2 {
                            // register(name, pid) -> erlang:register(name, pid)
                            CoreExpr::Call("erlang".to_string(), "register".to_string(), arg_exprs)
                        } else if name == "whereis" && arg_exprs.len() == 1 {
                            // whereis(name) -> erlang:whereis(name)
                            CoreExpr::Call("erlang".to_string(), "whereis".to_string(), arg_exprs)
                        } else if name == "make_ref" && arg_exprs.is_empty() {
                            // make_ref() -> erlang:make_ref()
                            CoreExpr::Call("erlang".to_string(), "make_ref".to_string(), vec![])
                        } else if name == "str_length" && arg_exprs.len() == 1 {
                            // str_length(s) -> string:length(s)
                            CoreExpr::Call("string".to_string(), "length".to_string(), arg_exprs)
                        } else if name == "str_concat" && arg_exprs.len() == 2 {
                            // str_concat(a, b) -> string:concat(a, b)
                            CoreExpr::Call("string".to_string(), "concat".to_string(), arg_exprs)
                        } else if name == "str_split" && arg_exprs.len() == 2 {
                            // str_split(s, sep) -> string:split(s, sep, all)
                            let mut args = arg_exprs;
                            args.push(CoreExpr::Lit(CoreLit::Atom("all".into())));
                            CoreExpr::Call("string".to_string(), "split".to_string(), args)
                        } else if name == "str_join" && arg_exprs.len() == 2 {
                            // str_join(list, sep) -> lists:join(sep, list)
                            let mut args: Vec<_> = arg_exprs.into_iter().collect();
                            args.reverse(); // swap order for lists:join
                            CoreExpr::Call("lists".to_string(), "join".to_string(), args)
                        } else if name == "str_trim" && arg_exprs.len() == 1 {
                            // str_trim(s) -> string:trim(s)
                            CoreExpr::Call("string".to_string(), "trim".to_string(), arg_exprs)
                        } else if name == "str_upper" && arg_exprs.len() == 1 {
                            // str_upper(s) -> string:uppercase(s)
                            CoreExpr::Call("string".to_string(), "uppercase".to_string(), arg_exprs)
                        } else if name == "str_lower" && arg_exprs.len() == 1 {
                            // str_lower(s) -> string:lowercase(s)
                            CoreExpr::Call("string".to_string(), "lowercase".to_string(), arg_exprs)
                        } else if name == "str_replace" && arg_exprs.len() == 3 {
                            // str_replace(s, from, to) -> string:replace(s, from, to, all)
                            let mut args = arg_exprs;
                            args.push(CoreExpr::Lit(CoreLit::Atom("all".into())));
                            CoreExpr::Call("string".to_string(), "replace".to_string(), args)
                        } else if name == "str_contains" && arg_exprs.len() == 2 {
                            // str_contains(s, sub) -> string:find(s, sub) != nomatch
                            let find_call = CoreExpr::Call("string".to_string(), "find".to_string(), arg_exprs);
                            CoreExpr::Call("erlang".to_string(), "/=".to_string(),
                                vec![find_call, CoreExpr::Lit(CoreLit::Atom("nomatch".into()))])
                        } else if name == "str_starts_with" && arg_exprs.len() == 2 {
                            // str_starts_with(s, prefix) -> string:prefix(s, prefix)
                            CoreExpr::Call("string".to_string(), "prefix".to_string(), arg_exprs)
                        } else if name == "chars" && arg_exprs.len() == 1 {
                            // chars(s) -> string:to_graphemes(s)
                            CoreExpr::Call("string".to_string(), "to_graphemes".to_string(), arg_exprs)
                        } else if name == "take" && arg_exprs.len() == 2 {
                            // take(n, list) -> lists:sublist(list, n)
                            let args: Vec<_> = arg_exprs.into_iter().collect();
                            CoreExpr::Call("lists".to_string(), "sublist".to_string(), vec![args[1].clone(), args[0].clone()])
                        } else if name == "drop" && arg_exprs.len() == 2 {
                            // drop(n, list) -> lists:nthtail(n, list)
                            CoreExpr::Call("lists".to_string(), "nthtail".to_string(), arg_exprs)
                        } else if name == "nth" && arg_exprs.len() == 2 {
                            // nth(n, list) -> lists:nth(n, list)
                            CoreExpr::Call("lists".to_string(), "nth".to_string(), arg_exprs)
                        } else if name == "zip" && arg_exprs.len() == 2 {
                            // zip(a, b) -> lists:zip(a, b)
                            CoreExpr::Call("lists".to_string(), "zip".to_string(), arg_exprs)
                        } else if name == "unzip" && arg_exprs.len() == 1 {
                            // unzip(list) -> lists:unzip(list)
                            CoreExpr::Call("lists".to_string(), "unzip".to_string(), arg_exprs)
                        } else if name == "enumerate" && arg_exprs.len() == 1 {
                            // enumerate(list) -> lists:enumerate(list)
                            CoreExpr::Call("lists".to_string(), "enumerate".to_string(), arg_exprs)
                        } else if name == "member" && arg_exprs.len() == 2 {
                            // member(elem, list) -> lists:member(elem, list)
                            CoreExpr::Call("lists".to_string(), "member".to_string(), arg_exprs)
                        } else if name == "unique" && arg_exprs.len() == 1 {
                            // unique(list) -> lists:usort(list)
                            CoreExpr::Call("lists".to_string(), "usort".to_string(), arg_exprs)
                        // Map functions
                        } else if name == "map_put" && arg_exprs.len() == 3 {
                            // map_put(map, key, value) -> maps:put(key, value, map)
                            let args: Vec<_> = arg_exprs.into_iter().collect();
                            CoreExpr::Call("maps".to_string(), "put".to_string(),
                                vec![args[1].clone(), args[2].clone(), args[0].clone()])
                        } else if name == "map_get" && arg_exprs.len() == 2 {
                            // map_get(map, key) -> maps:get(key, map)
                            let args: Vec<_> = arg_exprs.into_iter().collect();
                            CoreExpr::Call("maps".to_string(), "get".to_string(),
                                vec![args[1].clone(), args[0].clone()])
                        } else if name == "map_get" && arg_exprs.len() == 3 {
                            // map_get(map, key, default) -> maps:get(key, map, default)
                            let args: Vec<_> = arg_exprs.into_iter().collect();
                            CoreExpr::Call("maps".to_string(), "get".to_string(),
                                vec![args[1].clone(), args[0].clone(), args[2].clone()])
                        } else if name == "map_remove" && arg_exprs.len() == 2 {
                            // map_remove(map, key) -> maps:remove(key, map)
                            let args: Vec<_> = arg_exprs.into_iter().collect();
                            CoreExpr::Call("maps".to_string(), "remove".to_string(),
                                vec![args[1].clone(), args[0].clone()])
                        } else if name == "map_keys" && arg_exprs.len() == 1 {
                            // map_keys(map) -> maps:keys(map)
                            CoreExpr::Call("maps".to_string(), "keys".to_string(), arg_exprs)
                        } else if name == "map_values" && arg_exprs.len() == 1 {
                            // map_values(map) -> maps:values(map)
                            CoreExpr::Call("maps".to_string(), "values".to_string(), arg_exprs)
                        } else if name == "map_has_key" && arg_exprs.len() == 2 {
                            // map_has_key(map, key) -> maps:is_key(key, map)
                            let args: Vec<_> = arg_exprs.into_iter().collect();
                            CoreExpr::Call("maps".to_string(), "is_key".to_string(),
                                vec![args[1].clone(), args[0].clone()])
                        } else if name == "map_merge" && arg_exprs.len() == 2 {
                            // map_merge(map1, map2) -> maps:merge(map1, map2)
                            CoreExpr::Call("maps".to_string(), "merge".to_string(), arg_exprs)
                        } else if name == "map_size" && arg_exprs.len() == 1 {
                            // map_size(map) -> maps:size(map)
                            CoreExpr::Call("maps".to_string(), "size".to_string(), arg_exprs)
                        } else if name == "map_to_list" && arg_exprs.len() == 1 {
                            // map_to_list(map) -> maps:to_list(map)
                            CoreExpr::Call("maps".to_string(), "to_list".to_string(), arg_exprs)
                        } else if name == "list_to_map" && arg_exprs.len() == 1 {
                            // list_to_map(list) -> maps:from_list(list)
                            CoreExpr::Call("maps".to_string(), "from_list".to_string(), arg_exprs)
                        // File I/O functions
                        } else if name == "file_read" && arg_exprs.len() == 1 {
                            // file_read(path) -> file:read_file(path)
                            CoreExpr::Call("file".to_string(), "read_file".to_string(), arg_exprs)
                        } else if name == "file_write" && arg_exprs.len() == 2 {
                            // file_write(path, content) -> file:write_file(path, content)
                            CoreExpr::Call("file".to_string(), "write_file".to_string(), arg_exprs)
                        } else if name == "file_exists" && arg_exprs.len() == 1 {
                            // file_exists(path) -> filelib:is_file(path)
                            CoreExpr::Call("filelib".to_string(), "is_file".to_string(), arg_exprs)
                        } else if name == "file_delete" && arg_exprs.len() == 1 {
                            // file_delete(path) -> file:delete(path)
                            CoreExpr::Call("file".to_string(), "delete".to_string(), arg_exprs)
                        } else if name == "dir_list" && arg_exprs.len() == 1 {
                            // dir_list(path) -> file:list_dir(path)
                            CoreExpr::Call("file".to_string(), "list_dir".to_string(), arg_exprs)
                        } else if name == "dir_make" && arg_exprs.len() == 1 {
                            // dir_make(path) -> file:make_dir(path)
                            CoreExpr::Call("file".to_string(), "make_dir".to_string(), arg_exprs)
                        } else if name == "get_cwd" && arg_exprs.is_empty() {
                            // get_cwd() -> file:get_cwd()
                            CoreExpr::Call("file".to_string(), "get_cwd".to_string(), vec![])
                        } else if name == "typeof" && arg_exprs.len() == 1 {
                            // typeof(x) -> returns atom describing type
                            let x = arg_exprs.into_iter().next().unwrap();
                            let tmp = self.fresh_var();
                            CoreExpr::Let(
                                vec![(tmp.clone(), x)],
                                Box::new(CoreExpr::Case(
                                    Box::new(CoreExpr::Lit(CoreLit::Atom("true".into()))),
                                    vec![
                                        CoreClause {
                                            patterns: vec![CorePattern::Var("_".into())],
                                            guard: CoreExpr::Call("erlang".into(), "is_integer".into(),
                                                vec![CoreExpr::Var(tmp.clone())]),
                                            body: CoreExpr::Lit(CoreLit::Atom("integer".into())),
                                        },
                                        CoreClause {
                                            patterns: vec![CorePattern::Var("_".into())],
                                            guard: CoreExpr::Call("erlang".into(), "is_float".into(),
                                                vec![CoreExpr::Var(tmp.clone())]),
                                            body: CoreExpr::Lit(CoreLit::Atom("float".into())),
                                        },
                                        CoreClause {
                                            patterns: vec![CorePattern::Var("_".into())],
                                            guard: CoreExpr::Call("erlang".into(), "is_atom".into(),
                                                vec![CoreExpr::Var(tmp.clone())]),
                                            body: CoreExpr::Lit(CoreLit::Atom("atom".into())),
                                        },
                                        CoreClause {
                                            patterns: vec![CorePattern::Var("_".into())],
                                            guard: CoreExpr::Call("erlang".into(), "is_list".into(),
                                                vec![CoreExpr::Var(tmp.clone())]),
                                            body: CoreExpr::Lit(CoreLit::Atom("list".into())),
                                        },
                                        CoreClause {
                                            patterns: vec![CorePattern::Var("_".into())],
                                            guard: CoreExpr::Call("erlang".into(), "is_tuple".into(),
                                                vec![CoreExpr::Var(tmp.clone())]),
                                            body: CoreExpr::Lit(CoreLit::Atom("tuple".into())),
                                        },
                                        CoreClause {
                                            patterns: vec![CorePattern::Var("_".into())],
                                            guard: CoreExpr::Call("erlang".into(), "is_map".into(),
                                                vec![CoreExpr::Var(tmp.clone())]),
                                            body: CoreExpr::Lit(CoreLit::Atom("map".into())),
                                        },
                                        CoreClause {
                                            patterns: vec![CorePattern::Var("_".into())],
                                            guard: CoreExpr::Call("erlang".into(), "is_pid".into(),
                                                vec![CoreExpr::Var(tmp.clone())]),
                                            body: CoreExpr::Lit(CoreLit::Atom("pid".into())),
                                        },
                                        CoreClause {
                                            patterns: vec![CorePattern::Var("_".into())],
                                            guard: CoreExpr::Call("erlang".into(), "is_function".into(),
                                                vec![CoreExpr::Var(tmp)]),
                                            body: CoreExpr::Lit(CoreLit::Atom("function".into())),
                                        },
                                    ],
                                ))
                            )
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

            Expr::Index(container, key, _) => {
                let container_expr = self.translate_expr(container);
                let key_expr = self.translate_expr(key);
                // Use maps:get for map access (also works for lists with integer keys via lists:nth)
                CoreExpr::Call("maps".into(), "get".into(), vec![key_expr, container_expr])
            }

            Expr::Field(obj, field, _) => {
                // Field access - translate to maps:get(field, obj)
                // Structs and records are translated to maps
                let obj_expr = self.translate_expr(obj);
                CoreExpr::Call(
                    "maps".into(),
                    "get".into(),
                    vec![
                        CoreExpr::Lit(CoreLit::Atom(field.clone())),
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

            Expr::StructInit(_name, fields, _) => {
                // Translate struct to map with atom keys
                let entries: Vec<(CoreExpr, CoreExpr)> = fields
                    .iter()
                    .map(|(name, expr)| {
                        (
                            CoreExpr::Lit(CoreLit::Atom(name.clone())),
                            self.translate_expr(expr),
                        )
                    })
                    .collect();
                CoreExpr::Map(entries)
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
