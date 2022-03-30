use crate::{token::*, types::Type};
use std::fmt::Display;

// TODO remove the Option around Type and just give it a default void type or something
pub type Anotated<T> = (T, Span, Option<Type>);

#[derive(Debug, Clone)]
pub enum Ast {
    Error,
    Coment(Spanned<Token>),
    Literal(Spanned<Token>),
    Type(Type),
    Variable(Spanned<Token>),
    Declaration(
        Spanned<Token>,              /* name */
        Spanned<Token>,              /* : token or := token */
        Option<Box<Anotated<Self>>>, /* type */
        Option<Spanned<Token>>,      /* = token */
        Option<Box<Anotated<Self>>>, /* value */
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
        // TODO Make it so this are always declarations
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
            Ast::Declaration((name, _), (def_tk, _), ty, eq_tk, value) => {
                write!(f, "{} {} ", name, def_tk)?;

                if let Some(ty) = ty {
                    write!(f, "{}", ty.0)?;
                }
                if let Some((eq_tk, _)) = eq_tk {
                    write!(f, " {} ", eq_tk)?;
                }
                if let Some(value) = value {
                    write!(f, "{}", value.0)?;
                }
            }
            Ast::Coment((tk, _)) => {
                write!(f, "{}", tk)?;
            }
            Ast::Type(t) => {
                write!(f, "{}", t)?;
            },
        }
        Ok(())
    }
}
