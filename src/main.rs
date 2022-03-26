use std::{env, fs};

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use lang_frontend::*;

fn main() {
    let path = if let Some(path) = env::args().nth(1) {
        path
    } else {
        "./../lang-llvm/examples/main_arithmetic.lang".to_owned()
    };

    let src = fs::read_to_string(path).expect("Unable to read file");
    let (tokens, ast_and_type_table, errors) = parse_file(&src);

    if let Some(tokens) = tokens {
        println!(
            "{:?}",
            tokens.iter().map(|(token, _)| token).collect::<Vec<_>>()
        );
    }
    if let Some((ast, type_table)) = ast_and_type_table {
        ast.iter().for_each(|(node, _, _)| println!("{}", node));
        println!("{:?}", type_table);
    }

    errors.iter().for_each(|e| {
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
