use std::collections::HashMap;
use crate::types::types::{Scheme, Type, TyVar, Substitution};

/// Type environment mapping names to type schemes
#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    bindings: HashMap<String, Scheme>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, name: String, scheme: Scheme) {
        self.bindings.insert(name, scheme);
    }

    pub fn lookup(&self, name: &str) -> Option<&Scheme> {
        self.bindings.get(name)
    }

    pub fn extend(&self, name: String, scheme: Scheme) -> Self {
        let mut new = self.clone();
        new.insert(name, scheme);
        new
    }

    pub fn remove(&mut self, name: &str) {
        self.bindings.remove(name);
    }

    /// Get all free type variables in the environment
    pub fn free_vars(&self) -> std::collections::HashSet<TyVar> {
        let mut vars = std::collections::HashSet::new();
        for scheme in self.bindings.values() {
            let scheme_vars = free_vars_in_type(&scheme.ty);
            for v in scheme_vars {
                if !scheme.vars.contains(&v) {
                    vars.insert(v);
                }
            }
        }
        vars
    }

    pub fn apply(&self, subst: &Substitution) -> Self {
        let mut new = TypeEnv::new();
        for (name, scheme) in &self.bindings {
            let new_ty = subst.apply(&scheme.ty);
            new.insert(name.clone(), Scheme { vars: scheme.vars.clone(), ty: new_ty });
        }
        new
    }
}

/// Get free type variables in a type
pub fn free_vars_in_type(ty: &Type) -> std::collections::HashSet<TyVar> {
    let mut vars = std::collections::HashSet::new();
    collect_free_vars(ty, &mut vars);
    vars
}

fn collect_free_vars(ty: &Type, vars: &mut std::collections::HashSet<TyVar>) {
    match ty {
        Type::Var(v) => {
            vars.insert(*v);
        }
        Type::Function(params, ret) => {
            for p in params {
                collect_free_vars(p, vars);
            }
            collect_free_vars(ret, vars);
        }
        Type::Tuple(ts) => {
            for t in ts {
                collect_free_vars(t, vars);
            }
        }
        Type::List(elem) => {
            collect_free_vars(elem, vars);
        }
        Type::Record(fields) => {
            for (_, t) in fields {
                collect_free_vars(t, vars);
            }
        }
        Type::Named(_, _, args) => {
            for a in args {
                collect_free_vars(a, vars);
            }
        }
        _ => {}
    }
}
