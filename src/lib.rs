use std::thread;

use ast::{Anotated, Ast};
use chumsky::{prelude::Simple, Parser, Stream};
use token::*;
use types::Type;

use crate::inferer::Inferer;

pub mod ast;
pub mod inferer;
pub mod parser;
pub mod token;
pub mod tokenizer;
pub mod types;

type ParseResult = (
    Option<Vec<Spanned<Token>>>,
    Option<(Vec<Anotated<Ast>>, Vec<Type>)>,
    Vec<Simple<String>>,
);

// TODO make it so incorrect parsings do return a partial Ast and set of tokens
// TODO when the above is done, remove Option wrappers inside ParseResult
pub fn parse_file(src: &str) -> ParseResult {
    let builder = thread::Builder::new()
        .name("compilando con mucha recursion".into())
        .stack_size(32 * 1024 * 1024); // 32MB of stack space

    let src = src.to_string();

    let handler = builder.spawn(move || parse(&src)).unwrap();

    handler.join().unwrap()
}

fn parse(src: &str) -> ParseResult {
    let (tokens, token_errors) = tokenizer::tokenizer().parse_recovery(src);

    let mut res_tokens = None;
    let mut res_ast_and_types = None;

    let (ast_errors, inference_errors) = if let Some(tokens) = tokens {
        res_tokens = Some(tokens.clone());
        let len = src.chars().count();
        let (ast, ast_errors) = parser::ast_parser()
            .parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        let inference_error = if let Some(mut ast) = ast {
            let inferer = Inferer::new();

            let (type_table, errors) = inferer.infer(&mut ast);
            res_ast_and_types = Some((ast, type_table));
            errors
        } else {
            Vec::new()
        };
        (ast_errors, inference_error)
    } else {
        (Vec::new(), Vec::new())
    };

    let errors = token_errors
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(
            ast_errors
                .into_iter()
                .map(|e| e.map(|tk| format!("{:?}", tk))),
        )
        .chain(
            inference_errors
                .into_iter()
                .map(|(span, msg)| Simple::custom(span, msg)),
        )
        .collect();

    (res_tokens, res_ast_and_types, errors)
}
