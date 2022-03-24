use chumsky::{
    prelude::{end, filter, just, one_of, skip_then_retry_until, take_until, Simple},
    text::{self, TextParser},
    Parser,
};

use crate::token::{Spanned, Token};

pub fn tokenizer() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    // A parser for numbers
    let num = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        //.map(|n| n.parse().unwrap())
        .map(Token::Number);

    // A parser for strings
    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Text);

    // A parser for operators
    let op = just("=>")
        .to("=>".to_string())
        .or(one_of("+-*/=!><:")
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

    let comment = just("//")
        .then(take_until(just('\n').ignored().or(end())))
        .map_with_span(|(_, (s, _)), span| (Token::Comment(s.iter().collect::<String>()), span))
        .padded();

    comment
        .padded()
        .or(token.map_with_span(|tok, span| (tok, span)).padded())
        .repeated()
        .then_ignore(end().recover_with(skip_then_retry_until([]))) // This is to ensure we reach the EOF
}
