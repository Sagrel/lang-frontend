use crate::tokenizer::*;
use crate::types::Type;
use std::fmt::Display;

pub type Spanned<T> = (T, Span, Option<Type>);

#[derive(Debug, Clone)]
pub enum Declaration {
    Complete(Spanned<Ast> /*type */, Spanned<Ast> /*value */),
    OnlyType(Spanned<Ast> /*type */),
    OnlyValue(
        Spanned<Ast>, /*value */
        Span,         /* The := span for inlay_hints */
    ),
}

#[derive(Debug, Clone)]
pub enum Ast {
    Error,
    Literal(Token),
    Variable(String),
    Declaration(
        String,           /*name */
        Box<Declaration>, /*type and value */
    ),
    Call(
        Box<Spanned<Self>>, /* fn */
        Vec<Spanned<Self>>, /* args */
    ),
    Binary(
        Box<Spanned<Self>>, /* left */
        &'static str,       /* Operator */
        Box<Spanned<Self>>, /* right */
    ),
    While(
        Box<Spanned<Self>>, /* condition */
        Box<Spanned<Self>>, /* while body */
    ),
    If(
        Box<Spanned<Self>>, /* condition */
        Box<Spanned<Self>>, /* if body */
        Box<Spanned<Self>>, /* else body */
    ),
    Tuple(Vec<Spanned<Self>> /* elems */),
    Block(Vec<Spanned<Self>> /* elems */),
    Lambda(
        Vec<Spanned<Self>>, /* args */
        Box<Spanned<Self>>, /* body */
    ),
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ast::Error => write!(f, "¡Error!")?,
            Ast::Literal(l) => {
                match l {
                    Token::Bool(b) => {
                        write!(f, "{}", b)?;
                    }
                    Token::Number(n) => {
                        write!(f, "{}", n)?;
                    }
                    Token::Text(s) => {
                        write!(f, "\"{}\"", s)?;
                    }
                    _ => panic!("This should always be a literal"),
                };
            }
            Ast::Variable(name) => write!(f, "{}", name)?,
            Ast::Call(name, args) => {
                write!(f, "{}", &name.0)?;
                write!(f, "(")?;
                for arg in args {
                    write!(f, "{}", &arg.0)?;
                    write!(f, ",")?;
                }
                write!(f, ")")?;
            }
            Ast::Binary(l, op, r) => {
                write!(f, "({} {} {})", &l.0, op, &r.0)?;
            }
            Ast::While(cond, body) => {
                let body = format!("{}", body.0).replace('\n', "\n\t");
                write!(f, "while {} {{ {} \n}}", &cond.0, body)?;
            }
            Ast::If(cond, if_body, else_body) => {
                let if_body = format!("{}", if_body.0).replace('\n', "\n\t");
                let else_body = format!("{}", else_body.0).replace('\n', "\n\t");
                write!(
                    f,
                    "if {} {{ {} \n}} else {{ {} \n}}",
                    &cond.0, if_body, else_body
                )?;
            }
            Ast::Tuple(args) => {
                write!(f, "(")?;
                for arg in args {
                    write!(f, "{}", &arg.0)?;
                    write!(f, ",")?;
                }
                write!(f, ")")?;
            }
            Ast::Block(nodes) => {
                for node in nodes {
                    write!(f, "\n{}", &node.0)?;
                }
            }
            Ast::Lambda(args, body) => {
                let body = format!("{}", body.0).replace('\n', "\n\t");
                write!(f, "(")?;
                for arg in args {
                    write!(f, "{}", &arg.0)?;
                    write!(f, ",")?;
                }
                write!(f, ") => {{ {} \n}}", body)?;
            }
            Ast::Declaration(name, variant) => match variant.as_ref() {
                Declaration::Complete(ty, value) => {
                    write!(f, "{} : {} = {}", name, ty.0, value.0)?;
                }
                Declaration::OnlyType(ty) => {
                    write!(f, "{} : {}", name, ty.0)?;
                }
                Declaration::OnlyValue(value, _) => {
                    write!(f, "{} := {}", name, value.0)?;
                }
            },
        }
        Ok(())
    }
}
