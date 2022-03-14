use std::thread;

use ast::{Ast, Spanned};
use chumsky::{prelude::Simple, Parser, Stream};
use inferer::Type;
use tokenizer::{Span, Token};

use crate::inferer::Inferer;

pub mod ast;
pub mod inferer;
pub mod parser;
pub mod tokenizer;

type ParseResult = (
    Option<Vec<(Token, Span)>>,
    Option<(Vec<Spanned<Ast>>, Vec<Type>)>,
    Vec<Simple<String>>,
);

pub fn parse_file(src: &str) -> ParseResult {
    let builder = thread::Builder::new()
        .name("compilando con mucha recursion".into())
        .stack_size(32 * 1024 * 1024); // 32MB of stack space

    let src = src.to_string();

    let handler = builder.spawn(move || parse(&src)).unwrap();

    handler.join().unwrap()
}


fn parse(
    src: &str,
) -> ParseResult {
    

    let (tokens, token_errors) = tokenizer::tokenizer().parse_recovery(src);

    let mut res_tokens = None;
    let mut res_ast_and_types = None;

    let ast_errors = if let Some(tokens) = tokens {
 
        res_tokens = Some(tokens.clone());
        let len = src.chars().count();
        let (ast, ast_errors) = parser::ast_parser()
            .parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        if let Some(mut ast) = ast {
            let inferer = Inferer::default();

            let type_table = inferer.infer(&mut ast);
            res_ast_and_types =  Some((ast, type_table));
        }
        ast_errors
    } else {
        Vec::new()
    };

    let errors = token_errors
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(
            ast_errors
                .into_iter()
                .map(|e| e.map(|tk| format!("{:?}", tk))),
        ).collect();

        
    (res_tokens, res_ast_and_types, errors)
}
