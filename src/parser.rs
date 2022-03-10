use chumsky::prelude::*;

use crate::tokenizer::*;

pub type Spanned<T> = (T, Span);

#[derive(Debug)]
pub enum Ast {
    Error,
    Literal(Token),
    Variable(String),
    Call(
        Box<Spanned<Self>>,          /* fn */
        Spanned<Vec<Spanned<Self>>>, /* args */
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
}

pub fn ast_parser() -> impl Parser<Token, Vec<Spanned<Ast>>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let block = expr
            .clone()
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            // Attempt to recover anything that looks like a block but contains errors
            .recover_with(nested_delimiters(
                Token::Ctrl('{'),
                Token::Ctrl('}'),
                [
                    (Token::Ctrl('('), Token::Ctrl(')')),
                    (Token::Ctrl('['), Token::Ctrl(']')),
                ],
                |span| (Ast::Error, span),
            ));

        let if_ = just(Token::If)
            .ignore_then(expr.clone())
            .then(expr.clone())
            .then(
                just(Token::Else)
                    .ignore_then(expr.clone())
                    .or_not(),
            )
            .map_with_span(|((cond, a), b), span: Span| {
                (
                    Ast::If(
                        Box::new(cond),
                        Box::new(a),
                        Box::new(match b {
                            Some(b) => b,
                            // If an `if` expression has no trailing `else` block, we magic up one that just produces ()
                            None => (Ast::Tuple(Vec::new()), span.clone()),
                        }),
                    ),
                    span,
                )
            });

        let lit = filter_map(|span, token| match token {
            Token::Bool(_) | Token::Num(_) | Token::Str(_) => Ok(Ast::Literal(token)),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        })
        .labelled("literal");

        let identifier = filter_map(|span, token| match token {
            Token::Ident(ident) => Ok(ident.clone()),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        })
        .labelled("identifier");

        let comma_separated = expr
            .clone()
            .chain(just(Token::Ctrl(',')).ignore_then(expr.clone()).repeated())
            .then_ignore(just(Token::Ctrl(',')).or_not())
            .or_not()
            .map(|item| item.unwrap_or_else(Vec::new));

        // TODO asigment?

        let tuple = comma_separated
            .clone()
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
            .map(Ast::Tuple);

        let atom = lit
            .or(identifier.map(Ast::Variable))
            .or(tuple)
            .map_with_span(|expr, span| (expr, span))
            .or(block)
            .or(if_)
            .or(expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))) // Attempt to recover anything that looks like a parenthesised expression but contains errors
            .recover_with(nested_delimiters(
                Token::Ctrl('('),
                Token::Ctrl(')'),
                [
                    (Token::Ctrl('['), Token::Ctrl(']')),
                    (Token::Ctrl('{'), Token::Ctrl('}')),
                ],
                |span| (Ast::Error, span),
            ))
            // Attempt to recover anything that looks like a list but contains errors
            .recover_with(nested_delimiters(
                Token::Ctrl('['),
                Token::Ctrl(']'),
                [
                    (Token::Ctrl('('), Token::Ctrl(')')),
                    (Token::Ctrl('{'), Token::Ctrl('}')),
                ],
                |span| (Ast::Error, span),
            ));

        // Function calls have very high precedence so we prioritise them
        let call = atom
            .then(
                comma_separated
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                    .map_with_span(|args, span| (args, span))
                    .repeated(),
            )
            .foldl(|f, args| {
                let span = f.1.start..args.1.end;
                (Ast::Call(Box::new(f), args), span)
            });

        // TODO make the following patterns into something better

        // The dot operator has the highest precedence after function call: a.add(b) =>  a . add(b) The add(b) has higher priority
        let op = just(Token::Op(".".to_string())).to(".");
        let dot = call
            .clone()
            .then(op.then(call).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Ast::Binary(Box::new(a), op, Box::new(b)), span)
            });

        // Product ops (multiply and divide) have equal precedence
        let op = just(Token::Op("*".to_string()))
            .to("*")
            .or(just(Token::Op("/".to_string())).to("/"));
        let product = dot
            .clone()
            .then(op.then(dot).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Ast::Binary(Box::new(a), op, Box::new(b)), span)
            });

        // Sum ops (add and subtract) have equal precedence
        let op = just(Token::Op("+".to_string()))
            .to("+")
            .or(just(Token::Op("-".to_string())).to("-"));
        let sum = product
            .clone()
            .then(op.then(product).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Ast::Binary(Box::new(a), op, Box::new(b)), span)
            });

        // Comparison ops (equal, not-equal) have equal precedence
        let op = just(Token::Op("==".to_string()))
            .to("==")
            .or(just(Token::Op("!=".to_string())).to("!="))
            .or(just(Token::Op("<".to_string())).to("<"))
            .or(just(Token::Op("<=".to_string())).to("<="))
            .or(just(Token::Op(">".to_string())).to(">"))
            .or(just(Token::Op(">=".to_string())).to(">="));
        let compare = sum
            .clone()
            .then(op.then(sum).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Ast::Binary(Box::new(a), op, Box::new(b)), span)
            });

        // Logic ops ("and" and "or") have equal precedence
        let op = just(Token::Op("and".to_string()))
            .to("and")
            .or(just(Token::Op("or".to_string())).to("or"));
        let logic = compare
            .clone()
            .then(op.then(compare).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Ast::Binary(Box::new(a), op, Box::new(b)), span)
            });

        // TODO terminar las definiciones
        let op = just(Token::Op(":=".to_string())).to(":=");
        let definition = logic
            .clone()
            .then(op.then(logic).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Ast::Binary(Box::new(a), op, Box::new(b)), span)
            });

        definition
    })
    .repeated()
}
