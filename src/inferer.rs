use std::collections::HashMap;

use crate::types::Type;
use crate::{
    ast::*,
    token::{Span, Token},
};

#[derive(Clone)]
pub enum Constraint {
    Eq(Type, Type, Span),
}

#[derive(Default)]
pub struct Inferer {
    env: Vec<(HashMap<String, Type>, bool)>,
    constraints: Vec<Constraint>,
    errors: Vec<(Span, String)>,
    i: usize,
}

impl Inferer {
    pub fn new() -> Self {
        Self {
            env: vec![(HashMap::new(), false)],
            ..Default::default()
        }
    }

    fn next(&mut self) -> usize {
        self.i += 1;
        self.i - 1
    }

    fn find_declared(&self, name: &str) -> Option<Type> {
        let mut idx = self.env.len() - 1;
        loop {
            if let Some(t) = self.env[idx].0.get(name) {
                return Some(t.clone());
            } else if self.env[idx].1 {
                idx -= 1;
            } else {
                return self.env[0].0.get(name).cloned();
            }
        }
    }

    pub fn infer(mut self, nodes: &mut Vec<Anotated<Ast>>) -> (Vec<Type>, Vec<(Span, String)>) {
        let declared: Vec<_> = nodes
            .iter_mut()
            .filter_map(|node| {
                node.2 = Some(Type::void());
                match &mut node.0 {
                    Ast::Declaration((name_tk, _), variant) => {
                        let t = Type::T(self.next());
                        self.env
                            .last_mut()
                            .unwrap()
                            .0
                            .insert(name_tk.to_string(), t.clone());
                        Some((t, variant.as_mut(), node.1.clone()))
                    }
                    Ast::Error | Ast::Coment(_) => None,
                    _ => {
                        self.errors.push((
                            node.1.clone(),
                            "Only declarations are suported at top level".to_owned(),
                        ));
                        None
                    }
                }
            })
            .collect();
        for (expected, variant, span) in declared {
            match variant {
                Declaration::Complete(_, value) => {
                    // TODO Don't ignore types
                    self.generate_constraints(value);
                    self.constraints
                        .push(Constraint::Eq(expected, value.2.clone().unwrap(), span))
                }
                Declaration::OnlyType(_) => {
                    // TODO Don't ignore types
                }
                Declaration::OnlyValue(value, _) => {
                    self.generate_constraints(value);
                    self.constraints
                        .push(Constraint::Eq(expected, value.2.clone().unwrap(), span))
                }
            }
        }

        let mut substitution_table = vec![None; self.i];

        for constraint in self.constraints.clone().into_iter() {
            self.unify_one(&mut substitution_table, &constraint)
        }

        let type_table = substitution_table
            .into_iter()
            .map(|e| e.unwrap_or(Type::Error("???")))
            .collect();

        (Inferer::finalize_type_table(type_table), self.errors)
    }

    // SPEED optimizar esto para no copiar tanto o exponer get_most_concrete_type en la api
    fn finalize_type_table(types: Vec<Type>) -> Vec<Type> {
        types
            .iter()
            .map(|t| Inferer::get_most_concrete_type(t, &types))
            .collect()
    }

    pub fn get_most_concrete_type(t: &Type, types: &[Type]) -> Type {
        match t {
            Type::T(next) => Inferer::get_most_concrete_type(&types[*next], types),
            Type::Fn(args, ret) => {
                let new_args = args
                    .iter()
                    .map(|t| Inferer::get_most_concrete_type(t, types))
                    .collect();
                let new_ret = Inferer::get_most_concrete_type(ret, types);
                Type::Fn(new_args, Box::new(new_ret))
            }
            Type::Tuple(args) => {
                let new_args = args
                    .iter()
                    .map(|t| Inferer::get_most_concrete_type(t, types))
                    .collect();
                Type::Tuple(new_args)
            }
            x => x.clone(),
        }
    }

    fn generate_constraints(&mut self, node: &mut Anotated<Ast>) {
        match &mut node.0 {
            Ast::Literal((l, _)) => match l {
                Token::Bool(_) => node.2 = Some(Type::Bool),
                Token::Text(_) => node.2 = Some(Type::Text),
                Token::Number(_) => node.2 = Some(Type::Number),
                _ => unreachable!(),
            },
            Ast::Variable((name_tk, _)) => {
                let name = name_tk.to_string();
                node.2 = if let Some(t) = self.find_declared(&name) {
                    Some(t)
                } else {
                    self.errors.push((
                        node.1.clone(),
                        format!("The variable '{}' is not defined", name),
                    ));
                    // TODO investigar que pase si quito esto
                    let t = Type::T(self.next());
                    self.env
                        .last_mut()
                        .unwrap()
                        .0
                        .insert(name.clone(), t.clone());
                    Some(t)
                }
            }
            Ast::Call(caller, args) => {
                self.generate_constraints(caller);
                let arg_types = args
                    .iter_mut()
                    .map(|arg| {
                        self.generate_constraints(arg);
                        arg.2.clone().unwrap()
                    })
                    .collect();

                let retur_type = Type::T(self.next());

                self.constraints.push(Constraint::Eq(
                    caller.2.clone().unwrap(),
                    Type::Fn(arg_types, Box::new(retur_type.clone())),
                    node.1.clone(),
                ));
                node.2 = Some(retur_type);
            }
            // Transform a.print into print(a), and a.add(b) into add(a,b)
            Ast::Binary(l, (Token::Op(op), _), r) if op == "." => {
                // TODO the spans are messed up
                let l = l.clone();
                let r = r.clone();
                match *r {
                    (Ast::Call(caller, args), _, _) => {
                        let new_args = vec![*l].into_iter().chain(args.into_iter()).collect();
                        *node = (Ast::Call(caller, new_args), node.1.clone(), None);
                    }
                    _ => *node = (Ast::Call(Box::new(*r), vec![*l]), node.1.clone(), None),
                }
                self.generate_constraints(node);
            }
            Ast::Binary(l, (op_tk, _), r) => {
                self.generate_constraints(l);
                self.generate_constraints(r);
                self.constraints.push(Constraint::Eq(
                    l.2.clone().unwrap(),
                    r.2.clone().unwrap(),
                    node.1.clone(),
                ));
                let t = Type::T(self.next());
                node.2 = Some(t);

                let res_type = match op_tk.to_string().as_str() {
                    "<" | "<=" | ">=" | ">" | "==" | "!=" | "and" | "or" => Type::Bool,
                    _ => l.2.clone().unwrap(),
                };
                self.constraints.push(Constraint::Eq(
                    node.2.clone().unwrap(),
                    res_type,
                    node.1.clone(),
                ));
            }
            Ast::While(_, cond, body) => {
                self.generate_constraints(cond);
                self.constraints.push(Constraint::Eq(
                    cond.2.clone().unwrap(),
                    Type::Bool,
                    node.1.clone(),
                ));
                self.generate_constraints(body);
                node.2 = Some(Type::void());
            }
            Ast::If(_, cond, if_body, _, else_body) => {
                self.generate_constraints(cond);
                self.constraints.push(Constraint::Eq(
                    cond.2.clone().unwrap(),
                    Type::Bool,
                    node.1.clone(),
                ));
                self.generate_constraints(if_body);
                self.generate_constraints(else_body);
                self.constraints.push(Constraint::Eq(
                    if_body.2.clone().unwrap(),
                    else_body.2.clone().unwrap(),
                    node.1.clone(),
                ));
                node.2 = Some(Type::T(self.next()));
                self.constraints.push(Constraint::Eq(
                    node.2.clone().unwrap(),
                    else_body.2.clone().unwrap(),
                    node.1.clone(),
                ));
            }
            Ast::Tuple(args) => {
                let arg_types = args
                    .iter_mut()
                    .map(|arg| {
                        self.generate_constraints(arg);
                        arg.2.clone().unwrap()
                    })
                    .collect();

                node.2 = Some(Type::Tuple(arg_types));
            }
            Ast::Block(args) => {
                self.env.push((HashMap::new(), true));
                for arg in args.iter_mut() {
                    self.generate_constraints(arg)
                }
                node.2 = if let Some((_, _, t)) = args.last() {
                    t.clone()
                } else {
                    Some(Type::void())
                };
                self.env.pop();
            }
            Ast::Lambda(args, _, body) => {
                self.env.push((HashMap::new(), false));
                let arg_types = args
                    .iter_mut()
                    .map(|arg| {
                        if let Ast::Variable((name_tk, _)) = &arg.0 {
                            let t = Type::T(self.next());
                            self.env
                                .last_mut()
                                .unwrap()
                                .0
                                .insert(name_tk.to_string(), t);
                        }
                        self.generate_constraints(arg);
                        arg.2.clone().unwrap()
                    })
                    .collect();
                self.generate_constraints(body);
                let return_type = body.2.clone().unwrap();

                node.2 = Some(Type::Fn(arg_types, Box::new(return_type)));
                self.env.pop();
            }
            Ast::Error => {
                node.2 = Some(Type::T(self.next()));
            }
            // TODO should declarations return the value?
            // TODO need auxiliary function Node to Type = (Ast) -> Option<Type>
            // TODO Should we convert al declarations to full declarations?
            Ast::Declaration((name_tk, _), variant) => {
                node.2 = Some(Type::void());
                let expected = Type::T(self.next());
                self.env
                    .last_mut()
                    .unwrap()
                    .0
                    .insert(name_tk.to_string(), expected.clone());
                match variant.as_mut() {
                    Declaration::Complete(_, value) => {
                        // TODO Don't ignore types
                        self.generate_constraints(value);
                        self.constraints.push(Constraint::Eq(
                            expected,
                            value.2.clone().unwrap(),
                            node.1.clone(),
                        ))
                    }
                    Declaration::OnlyType(_) => {
                        // TODO Don't ignore types
                    }
                    Declaration::OnlyValue(value, _) => {
                        self.generate_constraints(value);
                        self.constraints.push(Constraint::Eq(
                            expected,
                            value.2.clone().unwrap(),
                            node.1.clone(),
                        ))
                    }
                }
            }
            Ast::Coment(_) => (),
        }
    }

    fn unify_one(&mut self, substitution_map: &mut Vec<Option<Type>>, constraint: &Constraint) {
        match constraint {
            Constraint::Eq(tx, ty, span) => {
                let (tx, ty) = Inferer::unify_variable(substitution_map, tx.clone(), ty.clone());
                if tx != ty {
                    if let Type::T(n) = tx {
                        if Inferer::occurs_check(substitution_map, n, &ty) {
                            return self.errors.push((
                                span.clone(),
                                format!("Recursive types are not allowed, {} is in {}", tx, ty),
                            ));
                        }
                    }

                    match (&tx, &ty) {
                        (Type::T(n), _) => {
                            substitution_map[*n] = Some(ty);
                        }
                        (_, Type::T(n)) => {
                            substitution_map[*n] = Some(tx);
                        }
                        // Recursive types here
                        (Type::Fn(args1, ret1), Type::Fn(args2, ret2)) => {
                            if args1.len() != args2.len() {
                                return self.errors.push((
                                    span.clone(),
                                    format!(
                                    "Number of lambda arguments do not match, expected {}, got {}",
                                    args1.len(),
                                    args2.len()
                                ),
                                ));
                            }
                            for (a, b) in args1.iter().zip(args1.iter()) {
                                self.unify_one(
                                    substitution_map,
                                    &Constraint::Eq(a.clone(), b.clone(), span.clone()),
                                );
                            }
                            self.unify_one(
                                substitution_map,
                                &Constraint::Eq(*ret1.clone(), *ret2.clone(), span.clone()),
                            );
                        }
                        _ => {
                            return self
                                .errors
                                .push((span.clone(), format!("Expected {}, got {}", tx, ty)))
                        }
                    }
                }
            }
        }
    }

    fn unify_variable(
        substitution_map: &[Option<Type>],
        mut t1: Type,
        mut t2: Type,
    ) -> (Type, Type) {
        while let Type::T(n) = t1 {
            if let Some(t) = substitution_map[n].clone() {
                t1 = t;
            } else {
                break;
            }
        }

        while let Type::T(n) = t2 {
            if let Some(t) = substitution_map[n].clone() {
                t2 = t;
            } else {
                break;
            }
        }

        (t1, t2)
    }

    fn occurs_check(substitution_table: &[Option<Type>], v: usize, ty: &Type) -> bool {
        if let Type::T(t) = ty {
            if t == &v {
                return true;
            } else if let Some(t) = substitution_table[*t].clone() {
                return Inferer::occurs_check(substitution_table, v, &t);
            }
        }

        // TODO recursive, non pointer types must be cheked
        if let Type::Fn(args, ret) = ty {
            if Inferer::occurs_check(substitution_table, v, ret) {
                return true;
            }

            for a in args {
                if Inferer::occurs_check(substitution_table, v, a) {
                    return true;
                }
            }
        }

        false
    }
}
