use crate::tokenizer::*;
use std::fmt::Display;

pub type Spanned<T> = (T, Span);

#[derive(Debug)]
pub enum Ast {
    Error,
    Literal(Token),
    Variable(String),
    Call(
        Box<Spanned<Self>>,          /* fn */
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
                    Token::Num(n) => {
                        write!(f, "{}", n)?;
                    }
                    Token::Str(s) => {
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
            Ast::While(_, _) => todo!(),
            Ast::If(cond, if_body, else_body) => {
                let if_body = format!("{}",if_body.0).replace('\n', "\n\t");
                let else_body = format!("{}",else_body.0).replace('\n', "\n\t");
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
        }
        Ok(())
    }
}
