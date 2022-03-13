use chumsky::{
    prelude::{end, filter, just, one_of, skip_then_retry_until, take_until, Simple},
    text::{self, TextParser},
    Parser,
};

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Token {
    Bool(bool),
    Num(String),
    Str(String),
    Op(String), // SPEED make this into a &'static str NOTE: this will be easy when zero-copy branch hits
    Ctrl(char),
    Ident(String),
    While,
    If,
    Else,
}

pub fn tokenizer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    // A parser for numbers
    let num = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        //.map(|n| n.parse().unwrap())
        .map(Token::Num);

    // A parser for strings
    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str);

    // A parser for operators
    let op = just("=>").to("=>".to_string()).or(one_of("+-*/=!><:")
        .then(just('=').or_not())
        .map(|(o1, o2)| {
            if let Some(o) = o2 {
                format!("{}{}", o1, o)
            } else {
                o1.to_string()
            }
        }))
        .or(just(".").to(".".to_string()))
        .map(Token::Op);

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = one_of("()[]{},").map(Token::Ctrl);

    // A parser for identifiers and keywords
    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "while" => Token::While,
        "if" => Token::If,
        "else" => Token::Else,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "and" => Token::Op("and".to_string()),
        "or" => Token::Op("or".to_string()),
        _ => Token::Ident(ident),
    });

    let token = num
        .or(str_)
        .or(op)
        .or(ctrl)
        .or(ident)
        .recover_with(skip_then_retry_until([])); // THIS IS ALL THE ERROR RECOVERY CODE. AMAZING!!!

    // TODO This does not always work
    let comment = just("//").then(take_until(just('\n'))).padded();

    token
        .padded_by(comment.repeated())
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .repeated()
        .then_ignore(end().recover_with(skip_then_retry_until([])))  // This is to ensure we reach the EOF
}
