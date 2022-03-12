use std::collections::HashMap;

use crate::ast::*;
use crate::tokenizer::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    String,
    Number,
    Bool,
    Tuple(Vec<Type>),
    T(usize),
    Fn(Vec<Type>, Box<Type>),
}

impl Type {
    pub fn void() -> Type {
        Type::Tuple(Vec::new())
    }
}

#[derive(Debug, Clone)]
pub enum Constraint {
    Eq(Type, Type, Span),
}

pub struct Inferer {
    env: HashMap<String, Type>,
    constraints: Vec<Constraint>,
    errors: Vec<(Span, String)>,
    i: usize,
}

impl Inferer {
    pub fn new() -> Self {
        Self {
            constraints: Vec::new(),
            errors: Vec::new(),
            i: 0,
            env: HashMap::new(),
        }
    }

    fn next(&mut self) -> usize {
        self.i += 1;
        self.i - 1
    }

	// TODO constraints for declarations, and add them to the env
    fn generate_constraints(&mut self, node: &mut Spanned<Ast>) {
        match &mut node.0 {
            Ast::Literal(l) => match l {
                Token::Bool(_) => node.2 = Some(Type::Bool),
                Token::Str(_) => node.2 = Some(Type::String),
                Token::Num(_) => node.2 = Some(Type::Number),
                _ => unreachable!(),
            },
            Ast::Variable(name) => {
                node.2 = if let Some(t) = self.env.get(name) {
                    Some(t.clone())
                } else {
                    let t = Type::T(self.next());
                    self.env.insert(name.clone(), t.clone());
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
            Ast::Binary(l, ".", r) => {
                // Transform a.print into print(a), and a.add(b) into add(a,b)
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
            Ast::Binary(l, op, r) => {
                self.generate_constraints(l);
                self.generate_constraints(r);
                self.constraints.push(Constraint::Eq(
                    l.2.clone().unwrap(),
                    r.2.clone().unwrap(),
                    node.1.clone(),
                ));
                let t = Type::T(self.next());
                node.2 = Some(t);
                let res_type = match op.as_ref() {
                    "<" | "<=" | ">=" | ">" | "==" | "!=" | "and" | "or" => Type::Bool,
                    _ => l.2.clone().unwrap(),
                };
                self.constraints.push(Constraint::Eq(
                    node.2.clone().unwrap(),
                    res_type,
                    node.1.clone(),
                ));
            }
            Ast::While(cond, body) => {
                self.generate_constraints(cond);
                self.constraints.push(Constraint::Eq(
                    cond.2.clone().unwrap(),
                    Type::Bool,
                    node.1.clone(),
                ));
                self.generate_constraints(body);
                node.2 = Some(Type::void());
            }
            Ast::If(cond, if_body, else_body) => {
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
                for arg in args.iter_mut() {
                    self.generate_constraints(arg)
                }
                node.2 = if let Some((_, _, t)) = args.last() {
                    t.clone()
                } else {
                    Some(Type::void())
                }
            }
            Ast::Lambda(args, body) => {
                let arg_types = args
                    .iter_mut()
                    .map(|arg| {
                        self.generate_constraints(arg);
                        arg.2.clone().unwrap()
                    })
                    .collect();
                self.generate_constraints(body);
                let return_type = body.2.clone().unwrap();

                node.2 = Some(Type::Fn(arg_types, Box::new(return_type)));
            }
            Ast::Error => unreachable!(),
        }
    }

    pub fn infer(mut self, nodes: &mut Vec<Spanned<Ast>>) -> Vec<Type> {
        for node in nodes {
            self.generate_constraints(node);
        }
        let mut substitution_table = vec![None; self.i];

        // TODO do unification here
        for constraint in self.constraints.clone().into_iter() {
            self.unify_one(&mut substitution_table, &constraint)
        }

        substitution_table.into_iter().map(|e| e.unwrap()).collect()
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
                                format!("Recursive types are not allowed, {:?} is in {:?}", tx, ty),
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
                                .push((span.clone(), format!("Expected {:?}, got {:?}", tx, ty)))
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

        /*// TODO recursive, non pointer types must be cheked
        if let Type::Lambda(args, ret) = ty {
            if self.occurs_check(v, ret) {
                return true;
            }

            for a in args {
                if self.occurs_check(v, a) {
                    return true;
                }
            }
        } */

        false
    }
}
