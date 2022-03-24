use crate::{token::*, types::Type};
use std::fmt::Display;

pub type Anotated<T> = (T, Span, Option<Type>);

#[derive(Debug, Clone)]
pub enum Declaration {
    Complete(Anotated<Ast> /*type */, Anotated<Ast> /*value */),
    OnlyType(Anotated<Ast> /*type */),
    OnlyValue(
        Anotated<Ast>, /*value */
        Span,          /* The := span for inlay_hints */
    ),
}

#[derive(Debug, Clone)]
pub enum Ast {
    Error,
    Coment(Spanned<Token>),
    Literal(Spanned<Token>),
    Variable(Spanned<Token>),
    Declaration(
        Spanned<Token>,   /*name */
        Box<Declaration>, /*type and value */
    ),
    Call(
        Box<Anotated<Self>>, /* fn */
        Vec<Anotated<Self>>, /* args */
    ),
    Binary(
        Box<Anotated<Self>>, /* left */
        Spanned<Token>,      /* Operator Spanned<Token>*/
        Box<Anotated<Self>>, /* right */
    ),
    While(
        Spanned<Token>,      /* while Spanned<Token> */
        Box<Anotated<Self>>, /* condition */
        Box<Anotated<Self>>, /* while body */
    ),
    If(
        Spanned<Token>,         /* if Spanned<Token> */
        Box<Anotated<Self>>,    /* condition */
        Box<Anotated<Self>>,    /* if body */
        Option<Spanned<Token>>, /* else Spanned<Token> */
        Box<Anotated<Self>>,    /* else body */
    ),
    Tuple(Vec<Anotated<Self>> /* elems */),
    Block(Vec<Anotated<Self>> /* elems */),
    Lambda(
        Vec<Anotated<Self>>, /* args */
        Spanned<Token>,      /* Arrow token */
        Box<Anotated<Self>>, /* body */
    ),
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ast::Error => write!(f, "¡Error!")?,
            Ast::Literal((l, _)) => {
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
            Ast::Variable((name, _)) => write!(f, "{}", name)?,
            Ast::Call(name, args) => {
                write!(f, "{}", &name.0)?;
                write!(f, "(")?;
                for arg in args {
                    write!(f, "{}", &arg.0)?;
                    write!(f, ",")?;
                }
                write!(f, ")")?;
            }
            Ast::Binary(l, (op_tk, _), r) => {
                write!(f, "({} {} {})", &l.0, op_tk, &r.0)?;
            }
            Ast::While(_, cond, body) => {
                let body = format!("{}", body.0).replace('\n', "\n\t");
                write!(f, "while {} {{ {} \n}}", &cond.0, body)?;
            }
            Ast::If(_, cond, if_body, _, else_body) => {
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
            Ast::Lambda(args, _, body) => {
                let body = format!("{}", body.0).replace('\n', "\n\t");
                write!(f, "(")?;
                for arg in args {
                    write!(f, "{}", &arg.0)?;
                    write!(f, ",")?;
                }
                write!(f, ") => {{ {} \n}}", body)?;
            }
            Ast::Declaration((name, _), variant) => match variant.as_ref() {
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
            Ast::Coment((tk,_)) => {
                    write!(f, "{}", tk)?;
                }
        }
        Ok(())
    }
}
