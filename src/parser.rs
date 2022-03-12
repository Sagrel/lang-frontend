use chumsky::prelude::*;

use crate::ast::*;
use crate::tokenizer::*;

macro_rules! operators {
    // Base case just one element
    ($last:expr) => {
        just(Token::Op($last.to_string())).to($last)
    };
    // $x is the head and ($($y),+) is the tail, that must contain at least 1 elemet (the $y)
    ($x:expr, $($y:expr),+) => (
        // Call `find_min!` on the tail `$y`
        operators!($x).or(operators!($($y),+))
    )
}

pub fn parse_with_less_precedence(
    op: impl Parser<Token, &'static str, Error = Simple<Token>> + Clone,
    prev: impl Parser<Token, Spanned<Ast>, Error = Simple<Token>> + Clone,
) -> impl Parser<Token, Spanned<Ast>, Error = Simple<Token>> + Clone {
    prev.clone()
        .then(op.then(prev).repeated())
        .foldl(|a, (op, b)| {
            let span = a.1.start..b.1.end;
            (Ast::Binary(Box::new(a), op, Box::new(b)), span)
        })
}

pub fn expresion_parser() -> impl Parser<Token, Spanned<Ast>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let lit = filter_map(|span, token| match token {
            Token::Bool(_) | Token::Num(_) | Token::Str(_) => Ok(Ast::Literal(token)),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        })
        .labelled("literal");

        let identifier = filter_map(|span, token| match token {
            Token::Ident(ident) => Ok(ident),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        })
        .labelled("identifier");

        let tuple = expr
            .clone()
            .separated_by(just(Token::Ctrl(',')))
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
            .map(Ast::Tuple);

        let block = expr
            .clone()
            .repeated()
            .map_with_span(|nodes, span| (Ast::Block(nodes), span))
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

        let while_ = just(Token::While)
            .ignore_then(expr.clone())
            .then(block.clone())
            .map(|(cond, body)| Ast::While(Box::new(cond), Box::new(body)));

        let if_ = recursive(|if_| {
            just(Token::If)
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(
                    just(Token::Else)
                        .ignore_then(block.clone().or(if_))
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
                                None => (
                                    Ast::Block(vec![(Ast::Tuple(Vec::new()), span.clone())]),
                                    span.clone(),
                                ),
                            }),
                        ),
                        span,
                    )
                })
        });

        let lambda = tuple
            .clone()
            .then_ignore(just(Token::Op("=>".to_string())))
            .then(block.clone())
            .map(|(args, body)| {
                if let Ast::Tuple(args) = args {
                    Ast::Lambda(args, Box::new(body))
                } else {
                    Ast::Error
                }
            });

        // ATOMS ARE NOT AMBIGUOUS
        // { HELLO } IS AN ATOM BECAUSE IT IS UNAMBIGUOUSLY A BLOCK
        // 2 + 3 * 3 IS NOT AN ATOM BECAUSE OF OPERATOR PRECEDENCE

        let atom = lit
            .or(identifier.map(Ast::Variable))
            .or(lambda)
            .or(tuple.clone())
            .or(while_)
            .map_with_span(|expr, span| (expr, span))
            .or(block)
            .or(if_)
            // this is for the case we find 2 * (1 + 3), it should not afect function calls like print("hello world")
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
            .clone()
            .then(tuple.clone())
            .map_with_span(|(caller, args), span| {
                if let Ast::Tuple(args) = args {
                    (Ast::Call(Box::new(caller), args), span)
                } else {
                    (Ast::Error, span)
                }
            })
            .or(atom);

        // The dot operator has the highest precedence after function call: a.add(b) =>  a . add(b) The add(b) has higher priorityst(Token::Op(".".to_string())).to(".");

        let dot = parse_with_less_precedence(operators!("."), call);
        let product = parse_with_less_precedence(operators!("*", "/"), dot);
        let sum = parse_with_less_precedence(operators!("+", "-"), product);
        let compare = parse_with_less_precedence(operators!("==", "!=", "<", "<=", ">", ">="), sum);
        let logic = parse_with_less_precedence(operators!("and", "or"), compare);
        let definition = parse_with_less_precedence(operators!(":="), logic);

        definition
    })
}

pub fn ast_parser() -> impl Parser<Token, Vec<Spanned<Ast>>, Error = Simple<Token>> + Clone {
    expresion_parser().repeated().then_ignore(end())
}
