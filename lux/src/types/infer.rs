use std::collections::HashMap;

use crate::syntax::ast::*;
use crate::syntax::span::Span;
use crate::types::env::{free_vars_in_type, TypeEnv};
use crate::types::types::{Scheme, Substitution, TyVar, Type, TypeId};
use crate::types::unify::{unify, TypeError};

pub struct InferenceContext {
    next_var: TyVar,
    next_type_id: TypeId,
    substitution: Substitution,
    type_defs: HashMap<String, TypeDef>,
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub id: TypeId,
    pub params: Vec<String>,
    pub variants: Vec<(String, Vec<Type>)>, // for enums
}

impl InferenceContext {
    pub fn new() -> Self {
        InferenceContext {
            next_var: 0,
            next_type_id: 0,
            substitution: Substitution::new(),
            type_defs: HashMap::new(),
        }
    }

    /// Generate a fresh type variable
    pub fn fresh_var(&mut self) -> Type {
        let var = self.next_var;
        self.next_var += 1;
        Type::Var(var)
    }

    /// Generate a fresh type ID
    pub fn fresh_type_id(&mut self) -> TypeId {
        let id = self.next_type_id;
        self.next_type_id += 1;
        id
    }

    /// Register a type definition
    pub fn register_type(&mut self, name: String, params: Vec<String>, variants: Vec<(String, Vec<Type>)>) -> TypeId {
        let id = self.fresh_type_id();
        self.type_defs.insert(name, TypeDef { id, params, variants });
        id
    }

    /// Instantiate a type scheme with fresh variables
    pub fn instantiate(&mut self, scheme: &Scheme) -> Type {
        let mapping: HashMap<TyVar, Type> = scheme
            .vars
            .iter()
            .map(|&v| (v, self.fresh_var()))
            .collect();
        self.substitute_vars(&scheme.ty, &mapping)
    }

    fn substitute_vars(&self, ty: &Type, mapping: &HashMap<TyVar, Type>) -> Type {
        match ty {
            Type::Var(v) => mapping.get(v).cloned().unwrap_or_else(|| ty.clone()),
            Type::Function(params, ret) => Type::Function(
                params.iter().map(|p| self.substitute_vars(p, mapping)).collect(),
                Box::new(self.substitute_vars(ret, mapping)),
            ),
            Type::Tuple(ts) => Type::Tuple(ts.iter().map(|t| self.substitute_vars(t, mapping)).collect()),
            Type::List(elem) => Type::List(Box::new(self.substitute_vars(elem, mapping))),
            Type::Record(fields) => Type::Record(
                fields.iter().map(|(n, t)| (n.clone(), self.substitute_vars(t, mapping))).collect(),
            ),
            Type::Named(id, name, args) => Type::Named(
                *id,
                name.clone(),
                args.iter().map(|a| self.substitute_vars(a, mapping)).collect(),
            ),
            _ => ty.clone(),
        }
    }

    /// Generalize a type to a scheme (quantify free vars not in env)
    pub fn generalize(&self, env: &TypeEnv, ty: &Type) -> Scheme {
        let ty = self.substitution.apply(ty);
        let ty_vars = free_vars_in_type(&ty);
        let env_vars = env.free_vars();
        let vars: Vec<TyVar> = ty_vars.difference(&env_vars).copied().collect();
        Scheme { vars, ty }
    }

    /// Apply current substitution
    pub fn apply(&self, ty: &Type) -> Type {
        self.substitution.apply(ty)
    }

    /// Unify two types and update substitution
    pub fn unify(&mut self, t1: &Type, t2: &Type, span: Span) -> Result<(), TypeError> {
        let t1 = self.apply(t1);
        let t2 = self.apply(t2);
        let subst = unify(&t1, &t2, span)?;
        self.substitution = self.substitution.compose(&subst);
        Ok(())
    }

    /// Convert a type expression to a type
    pub fn type_expr_to_type(&mut self, expr: &TypeExpr, type_params: &HashMap<String, TyVar>) -> Result<Type, TypeError> {
        match expr {
            TypeExpr::Named(name, args, span) => {
                // Check if it's a type parameter
                if args.is_empty() {
                    if let Some(&var) = type_params.get(name) {
                        return Ok(Type::Var(var));
                    }
                }

                // Check built-in types
                let ty = match name.as_str() {
                    "Int" => Type::Int,
                    "Float" => Type::Float,
                    "Bool" => Type::Bool,
                    "String" => Type::String,
                    "Atom" => Type::Atom,
                    "Pid" => Type::Pid,
                    "Ref" => Type::Ref,
                    "Never" => Type::Never,
                    "Any" => Type::Any,
                    _ => {
                        // Look up user-defined type
                        let def_id = self.type_defs.get(name).map(|d| d.id);
                        if let Some(id) = def_id {
                            let mut type_args = Vec::new();
                            for a in args {
                                type_args.push(self.type_expr_to_type(a, type_params)?);
                            }
                            Type::Named(id, name.clone(), type_args)
                        } else {
                            return Err(TypeError::UnboundType(name.clone(), *span));
                        }
                    }
                };
                Ok(ty)
            }
            TypeExpr::Tuple(types, _) => {
                let tys: Vec<Type> = types
                    .iter()
                    .map(|t| self.type_expr_to_type(t, type_params))
                    .collect::<Result<_, _>>()?;
                Ok(Type::Tuple(tys))
            }
            TypeExpr::Function(params, ret, _) => {
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|t| self.type_expr_to_type(t, type_params))
                    .collect::<Result<_, _>>()?;
                let ret_type = self.type_expr_to_type(ret, type_params)?;
                Ok(Type::Function(param_types, Box::new(ret_type)))
            }
            TypeExpr::Record(fields, _) => {
                let field_types: Vec<(String, Type)> = fields
                    .iter()
                    .map(|(n, t)| Ok((n.clone(), self.type_expr_to_type(t, type_params)?)))
                    .collect::<Result<_, TypeError>>()?;
                Ok(Type::Record(field_types))
            }
            TypeExpr::Unit(_) => Ok(Type::Unit),
        }
    }

    /// Infer the type of an expression
    pub fn infer_expr(&mut self, env: &TypeEnv, expr: &Expr) -> Result<Type, TypeError> {
        match expr {
            Expr::Int(_, _) => Ok(Type::Int),
            Expr::Float(_, _) => Ok(Type::Float),
            Expr::String(_, _) => Ok(Type::String),
            Expr::InterpolatedString(parts, _) => {
                // Check that all interpolated expressions are valid
                for part in parts {
                    if let crate::syntax::ast::InterpolatedPart::Expr(e) = part {
                        self.infer_expr(env, e)?;
                    }
                }
                Ok(Type::String)
            }
            Expr::Bool(_, _) => Ok(Type::Bool),
            Expr::Atom(_, _) => Ok(Type::Atom),
            Expr::Unit(_) => Ok(Type::Unit),

            Expr::Var(name, span) => {
                match env.lookup(name) {
                    Some(scheme) => Ok(self.instantiate(scheme)),
                    None => Err(TypeError::UnboundVariable(name.clone(), *span)),
                }
            }

            Expr::Tuple(exprs, _) => {
                let types: Vec<Type> = exprs
                    .iter()
                    .map(|e| self.infer_expr(env, e))
                    .collect::<Result<_, _>>()?;
                Ok(Type::Tuple(types))
            }

            Expr::List(exprs, tail, span) => {
                let elem_type = self.fresh_var();
                for e in exprs {
                    let t = self.infer_expr(env, e)?;
                    self.unify(&elem_type, &t, *span)?;
                }
                if let Some(t) = tail {
                    let tail_type = self.infer_expr(env, t)?;
                    self.unify(&Type::List(Box::new(elem_type.clone())), &tail_type, *span)?;
                }
                Ok(Type::List(Box::new(self.apply(&elem_type))))
            }

            Expr::Map(entries, span) => {
                let key_type = self.fresh_var();
                let value_type = self.fresh_var();
                for (k, v) in entries {
                    let kt = self.infer_expr(env, k)?;
                    let vt = self.infer_expr(env, v)?;
                    self.unify(&key_type, &kt, *span)?;
                    self.unify(&value_type, &vt, *span)?;
                }
                Ok(Type::Map(
                    Box::new(self.apply(&key_type)),
                    Box::new(self.apply(&value_type)),
                ))
            }

            Expr::Range(start, end, _inclusive, span) => {
                let start_type = self.infer_expr(env, start)?;
                let end_type = self.infer_expr(env, end)?;
                self.unify(&start_type, &Type::Int, *span)?;
                self.unify(&end_type, &Type::Int, *span)?;
                Ok(Type::List(Box::new(Type::Int)))
            }

            Expr::ListComp { expr, generators, filters, span } => {
                let mut local_env = env.clone();

                // Process generators - each binds variables
                for generator in generators {
                    let source_type = self.infer_expr(&local_env, &generator.source)?;
                    // Expect source to be a list
                    let elem_type = self.fresh_var();
                    self.unify(&source_type, &Type::List(Box::new(elem_type.clone())), *span)?;
                    // Bind pattern variables with the element type
                    self.bind_pattern_vars(&mut local_env, &generator.pattern, &self.apply(&elem_type));
                }

                // Check filters are boolean
                for filter in filters {
                    let filter_type = self.infer_expr(&local_env, filter)?;
                    self.unify(&filter_type, &Type::Bool, *span)?;
                }

                // Infer the expression type
                let expr_type = self.infer_expr(&local_env, expr)?;
                Ok(Type::List(Box::new(self.apply(&expr_type))))
            }

            Expr::Binary(left, op, right, span) => {
                let left_type = self.infer_expr(env, left)?;
                let right_type = self.infer_expr(env, right)?;

                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                        self.unify(&left_type, &Type::Int, *span)?;
                        self.unify(&right_type, &Type::Int, *span)?;
                        Ok(Type::Int)
                    }
                    BinOp::Eq | BinOp::NotEq => {
                        self.unify(&left_type, &right_type, *span)?;
                        Ok(Type::Bool)
                    }
                    BinOp::Lt | BinOp::LtEq | BinOp::Gt | BinOp::GtEq => {
                        self.unify(&left_type, &Type::Int, *span)?;
                        self.unify(&right_type, &Type::Int, *span)?;
                        Ok(Type::Bool)
                    }
                    BinOp::And | BinOp::Or => {
                        self.unify(&left_type, &Type::Bool, *span)?;
                        self.unify(&right_type, &Type::Bool, *span)?;
                        Ok(Type::Bool)
                    }
                    BinOp::Concat => {
                        // ++ concatenates lists or strings
                        // Both sides must be the same type
                        self.unify(&left_type, &right_type, *span)?;
                        Ok(self.apply(&left_type))
                    }
                    BinOp::Pipe => {
                        // Pipe operator: x |> f  means f(x)
                        let ret_type = self.fresh_var();
                        let fn_type = Type::Function(vec![left_type], Box::new(ret_type.clone()));
                        self.unify(&right_type, &fn_type, *span)?;
                        Ok(self.apply(&ret_type))
                    }
                }
            }

            Expr::Unary(op, inner, span) => {
                let inner_type = self.infer_expr(env, inner)?;
                match op {
                    UnaryOp::Neg => {
                        self.unify(&inner_type, &Type::Int, *span)?;
                        Ok(Type::Int)
                    }
                    UnaryOp::Not => {
                        self.unify(&inner_type, &Type::Bool, *span)?;
                        Ok(Type::Bool)
                    }
                }
            }

            Expr::If(cond, then_branch, else_branch, span) => {
                let cond_type = self.infer_expr(env, cond)?;
                self.unify(&cond_type, &Type::Bool, *span)?;

                let then_type = self.infer_expr(env, then_branch)?;

                if let Some(else_br) = else_branch {
                    let else_type = self.infer_expr(env, else_br)?;
                    self.unify(&then_type, &else_type, *span)?;
                    Ok(self.apply(&then_type))
                } else {
                    self.unify(&then_type, &Type::Unit, *span)?;
                    Ok(Type::Unit)
                }
            }

            Expr::Block(stmts, final_expr, _) => {
                let mut local_env = env.clone();

                for stmt in stmts {
                    match stmt {
                        Stmt::Let(pattern, ty_ann, init, span) => {
                            let init_type = self.infer_expr(&local_env, init)?;

                            if let Some(ann) = ty_ann {
                                let ann_type = self.type_expr_to_type(ann, &HashMap::new())?;
                                self.unify(&init_type, &ann_type, *span)?;
                            }

                            // Extract variables from pattern and add to environment
                            self.bind_pattern_vars(&mut local_env, pattern, &self.apply(&init_type));
                        }
                        Stmt::Expr(e) => {
                            self.infer_expr(&local_env, e)?;
                        }
                    }
                }

                if let Some(e) = final_expr {
                    self.infer_expr(&local_env, e)
                } else {
                    Ok(Type::Unit)
                }
            }

            Expr::Call(func, args, span) => {
                let func_type = self.infer_expr(env, func)?;
                let arg_types: Vec<Type> = args
                    .iter()
                    .map(|a| self.infer_expr(env, a))
                    .collect::<Result<_, _>>()?;

                let return_type = self.fresh_var();
                let expected = Type::Function(arg_types, Box::new(return_type.clone()));

                self.unify(&func_type, &expected, *span)?;
                Ok(self.apply(&return_type))
            }

            Expr::Lambda(params, ret_ann, body, _) => {
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|p| {
                        if let Some(ty) = &p.ty {
                            self.type_expr_to_type(ty, &HashMap::new())
                        } else {
                            Ok(self.fresh_var())
                        }
                    })
                    .collect::<Result<_, _>>()?;

                let mut body_env = env.clone();
                for (param, ty) in params.iter().zip(&param_types) {
                    body_env.insert(param.name.clone(), Scheme::mono(ty.clone()));
                }

                let body_type = self.infer_expr(&body_env, body)?;

                if let Some(ret) = ret_ann {
                    let ret_type = self.type_expr_to_type(ret, &HashMap::new())?;
                    self.unify(&body_type, &ret_type, body.span())?;
                }

                Ok(Type::Function(
                    param_types.into_iter().map(|t| self.apply(&t)).collect(),
                    Box::new(self.apply(&body_type)),
                ))
            }

            // Process primitives
            Expr::Spawn(thunk, span) => {
                let thunk_type = self.infer_expr(env, thunk)?;
                let ret = self.fresh_var();
                self.unify(&thunk_type, &Type::Function(vec![], Box::new(ret)), *span)?;
                Ok(Type::Pid)
            }

            Expr::Send(pid, msg, span) => {
                let pid_type = self.infer_expr(env, pid)?;
                self.unify(&pid_type, &Type::Pid, *span)?;
                let msg_type = self.infer_expr(env, msg)?;
                Ok(msg_type)
            }

            Expr::Receive { arms, timeout, span } => {
                let result_type = self.fresh_var();
                for arm in arms {
                    // TODO: proper pattern typing
                    let arm_type = self.infer_expr(env, &arm.body)?;
                    self.unify(&result_type, &arm_type, *span)?;
                }
                // Check timeout body type if present
                if let Some((_, timeout_body)) = timeout {
                    let timeout_type = self.infer_expr(env, timeout_body)?;
                    self.unify(&result_type, &timeout_type, *span)?;
                }
                Ok(self.apply(&result_type))
            }

            Expr::SelfPid(_) => Ok(Type::Pid),

            Expr::Return(expr, _) => {
                if let Some(e) = expr {
                    self.infer_expr(env, e)
                } else {
                    Ok(Type::Unit)
                }
            }

            Expr::Index(container, key, span) => {
                let container_type = self.infer_expr(env, container)?;
                let key_type = self.infer_expr(env, key)?;

                // Check if it's a map or list access
                let value_type = self.fresh_var();

                // Try to unify with Map(key_type, value_type)
                let map_type = Type::Map(Box::new(key_type.clone()), Box::new(value_type.clone()));
                if self.unify(&container_type, &map_type, *span).is_ok() {
                    return Ok(self.apply(&value_type));
                }

                // Try to unify with List(value_type) where key is Int
                let list_type = Type::List(Box::new(value_type.clone()));
                if self.unify(&container_type, &list_type, *span).is_ok() {
                    self.unify(&key_type, &Type::Int, *span)?;
                    return Ok(self.apply(&value_type));
                }

                // Default: return Any
                Ok(Type::Any)
            }

            Expr::Try { body, catch_arms, span } => {
                let body_type = self.infer_expr(env, body)?;

                // Each catch arm should return the same type as body
                for arm in catch_arms {
                    // Bind the pattern variable
                    let mut local_env = env.clone();
                    let pattern_type = self.fresh_var();
                    self.bind_pattern_vars(&mut local_env, &arm.pattern, &pattern_type);

                    let arm_type = self.infer_expr(&local_env, &arm.body)?;
                    self.unify(&body_type, &arm_type, *span)?;
                }

                Ok(self.apply(&body_type))
            }

            // TODO: implement remaining expression types
            _ => Ok(self.fresh_var()),
        }
    }

    /// Bind variables from a pattern to the environment with appropriate types
    fn bind_pattern_vars(&mut self, env: &mut TypeEnv, pattern: &crate::syntax::ast::Pattern, ty: &Type) {
        use crate::syntax::ast::Pattern;
        match pattern {
            Pattern::Var(name, _) => {
                let scheme = self.generalize(env, ty);
                env.insert(name.clone(), scheme);
            }
            Pattern::Wildcard(_) => {
                // No binding needed
            }
            Pattern::Tuple(pats, _) => {
                // For tuples, try to destructure the type
                if let Type::Tuple(elem_types) = ty {
                    for (pat, elem_ty) in pats.iter().zip(elem_types.iter()) {
                        self.bind_pattern_vars(env, pat, elem_ty);
                    }
                } else {
                    // Type mismatch - bind all vars as fresh type vars
                    for pat in pats {
                        let fresh = self.fresh_var();
                        self.bind_pattern_vars(env, pat, &fresh);
                    }
                }
            }
            Pattern::List(pats, tail, _) => {
                if let Type::List(elem_ty) = ty {
                    for pat in pats {
                        self.bind_pattern_vars(env, pat, elem_ty);
                    }
                    if let Some(tail_pat) = tail {
                        self.bind_pattern_vars(env, tail_pat, ty);
                    }
                }
            }
            Pattern::Constructor(_, fields, _) => {
                // For constructors, we'd need type info - for now use fresh vars
                for pat in fields {
                    let fresh = self.fresh_var();
                    self.bind_pattern_vars(env, pat, &fresh);
                }
            }
            Pattern::Record(fields, _) => {
                for (_, pat) in fields {
                    let fresh = self.fresh_var();
                    self.bind_pattern_vars(env, pat, &fresh);
                }
            }
            _ => {
                // Literals don't bind variables
            }
        }
    }
}

impl Default for InferenceContext {
    fn default() -> Self {
        Self::new()
    }
}
