use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{Parser, Stream};

mod parser;
mod tokenizer;

fn main() {
    let src = include_str!("../examples/simple");
    let (tokens, token_errors) = tokenizer::tokenizer().parse_recovery(src);

    let ast_errors = if let Some(tokens) = tokens {
        println!(
            "{:?}",
            tokens.iter().map(|(token, _)| token).collect::<Vec<_>>()
        );
        
        let len = src.chars().count();
        let (ast, ast_errors) = parser::ast_parser()
             .parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        if let Some(ast) = ast {            
            ast.iter().for_each(|(node, _)| println!("{:?}", node)
        );
        }
        ast_errors
    } else {
        Vec::new()
    };

    token_errors
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(ast_errors.into_iter().map(|e| e.map(|tk| format!("{:?}", tk))))
        .for_each(|e| {
            let report = Report::build(ReportKind::Error, (), e.span().start);

            let report = match e.reason() {
                chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                    .with_message(format!(
                        "Unclosed delimiter {}",
                        delimiter.fg(Color::Yellow)
                    ))
                    .with_label(
                        Label::new(span.clone())
                            .with_message(format!(
                                "Unclosed delimiter {}",
                                delimiter.fg(Color::Yellow)
                            ))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new(e.span())
                            .with_message(format!(
                                "Must be closed before this {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Unexpected => report
                    .with_message(format!(
                        "{}, expected one of this: {}",
                        if e.found().is_some() {
                            "Unexpected token in input"
                        } else {
                            "Unexpected end of input"
                        },
                        if e.expected().len() == 0 {
                            "something else".to_string()
                        } else {
                            e.expected()
                                .map(|expected| match expected {
                                    Some(expected) => expected.to_string(),
                                    None => "end of input".to_string(),
                                })
                                .collect::<Vec<_>>()
                                .join(", ")
                        }
                    ))
                    .with_label(
                        Label::new(e.span())
                            .with_message(format!(
                                "Unexpected token {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                    Label::new(e.span())
                        .with_message(format!("{}", msg.fg(Color::Red)))
                        .with_color(Color::Red),
                ),
            };

            report.finish().eprint(Source::from(&src)).unwrap();
        });
}
