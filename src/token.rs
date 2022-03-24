use std::fmt::Display;

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Token {
    Bool(bool),
    Number(String),
    Text(String),
    Comment(String),
    Op(String), // SPEED make this into a &'static str NOTE: this will be easy when zero-copy branch lands
    Ctrl(char),
    Ident(String),
    While,
    If,
    Else,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Bool(b) => write!(f, "{}", b),
            Token::Number(n) => write!(f, "{}", n),
            Token::Text(t) => write!(f, "{}", t),
            Token::Comment(t) => write!(f, "//{}", t),
            Token::Op(o) => write!(f, "{}", o),
            Token::Ctrl(c) => write!(f, "{}", c),
            Token::Ident(i) => write!(f, "{}", i),
            Token::While => write!(f, "while"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
        }
    }
}

pub type Span = std::ops::Range<usize>;

pub type Spanned<T> = (T, Span);
