use chumsky::prelude::*;

use crate::{
    ast::*,
    token::{Span, Spanned, Token},
    types::Type,
};

// SPEED remove all of the ".boxed()"  they just make compile times more berable

macro_rules! operators {
    // Base case just one element
    ($last:expr) => {
        just(Token::Op($last.to_string())).map_with_span(|tk, span| (tk,span))
    };
    // $x is the head and ($($y),+) is the tail, that must contain at least 1 elemet (the $y)
    ($x:expr, $($y:expr),+) => (
        operators!($x).or(operators!($($y),+))
    )
}

fn parse_with_less_precedence(
    op: impl Parser<Token, Spanned<Token>, Error = Simple<Token>> + Clone,
    prev: impl Parser<Token, Anotated<Ast>, Error = Simple<Token>> + Clone,
) -> impl Parser<Token, Anotated<Ast>, Error = Simple<Token>> + Clone {
    prev.clone()
        .then(op.then(prev).repeated())
        .foldl(|a, (op, b)| {
            let span = a.1.start..b.1.end;
            (Ast::Binary(Box::new(a), op, Box::new(b)), span, None)
        })
}

fn type_parser() -> impl Parser<Token, Type, Error = Simple<Token>> + Clone {
    recursive(|ty| {
        let tuple = ty
            .clone()
            .separated_by(just(Token::Ctrl(',')))
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

        let lambda = tuple
            .clone()
            .then_ignore(just(Token::Op("=>".to_string())))
            .then(ty)
            .map(|(args, ret)| Type::Fn(args, Box::new(ret)));

        just(Token::Ident("Number".to_string()))
            .to(Type::Number)
            .or(just(Token::Ident("Text".to_string())).to(Type::Text))
            .or(just(Token::Ident("Bool".to_string())).to(Type::Bool))
            .or(lambda)
            .or(tuple.map(Type::Tuple))
    })
}

fn identifier_parser() -> impl Parser<Token, Spanned<Token>, Error = Simple<Token>> + Clone {
    filter_map(|span, tk| match &tk {
        Token::Ident(_) => Ok((tk, span)),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tk))),
    })
    .labelled("identifier")
}

fn pattern_parser() -> impl Parser<Token, Anotated<Pattern>, Error = Simple<Token>> {
    recursive(|pattern| {
        let var_pattern = identifier_parser().map(Pattern::Var);
        let tuple_pattern = pattern
            .clone()
            .separated_by(just(Token::Ctrl(',')))
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
            .map(Pattern::Tuple);
        var_pattern
            .or(tuple_pattern)
            .map_with_span(|p, span| (p, span, None))
    })
}

fn declaration_parser(
    expresion: impl Parser<Token, Anotated<Ast>, Error = Simple<Token>> + Clone,
) -> impl Parser<Token, Anotated<Ast>, Error = Simple<Token>> {
    let only_type = pattern_parser()
        .then(just(Token::Op(":".to_string())).map_with_span(|tk, span| (tk, span)))
        .then(type_parser().map_with_span(|ty, span| (Ast::Type(ty), span, None)))
        .map(|((name, def_tk), ty)| Ast::Declaration(name, def_tk, Some(Box::new(ty)), None, None));
    let only_value = pattern_parser()
        .then(just(Token::Op(":=".to_string())).map_with_span(|tk, span| (tk, span)))
        .then(expresion.clone())
        .map(|((name, def_tk), value)| {
            Ast::Declaration(name, def_tk, None, None, Some(Box::new(value)))
        });
    let complete = pattern_parser()
        .then(just(Token::Op(":".to_string())).map_with_span(|tk, span| (tk, span)))
        .then(type_parser().map_with_span(|ty, span| (Ast::Type(ty), span, None)))
        .then(just(Token::Op("=".to_string())).map_with_span(|tk, span| (tk, span)))
        .then(expresion)
        .map(|((((name, def_tk), ty), eq_tk), value)| {
            Ast::Declaration(
                name,
                def_tk,
                Some(Box::new(ty)),
                Some(eq_tk),
                Some(Box::new(value)),
            )
        });

    complete
        .or(only_type)
        .or(only_value)
        .map_with_span(|node, span| (node, span, None))
}

// TODO Make this more error resistant. Unclosed { fucks everything up
pub fn expresion_parser() -> impl Parser<Token, Anotated<Ast>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let lit = filter_map(|span, token| match token {
            Token::Bool(_) | Token::Number(_) | Token::Text(_) => Ok(Ast::Literal((token, span))),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        })
        .labelled("literal")
        .boxed();

        let identifier = identifier_parser();

        let tuple = expr
            .clone()
            .separated_by(just(Token::Ctrl(',')))
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
            .map(Ast::Tuple)
            .boxed();

        let block = expr
            .clone()
            .repeated()
            .map_with_span(|nodes, span| (Ast::Block(nodes), span, None))
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            // Attempt to recover anything that looks like a block but contains errors
            .recover_with(nested_delimiters(
                Token::Ctrl('{'),
                Token::Ctrl('}'),
                [
                    (Token::Ctrl('('), Token::Ctrl(')')),
                    (Token::Ctrl('['), Token::Ctrl(']')),
                ],
                |span| (Ast::Error, span, None),
            ))
            .boxed();

        let while_ = just(Token::While)
            .map_with_span(|tk, span| (tk, span))
            .then(expr.clone())
            .then(block.clone())
            .map(|((while_tk, cond), body)| Ast::While(while_tk, Box::new(cond), Box::new(body)))
            .boxed();

        let if_ = recursive(|if_| {
            just(Token::If)
                .map_with_span(|tk, span| (tk, span))
                .then(expr.clone())
                .then(block.clone())
                .then(
                    just(Token::Else)
                        .map_with_span(|tk, span| (tk, span))
                        .then(block.clone().or(if_))
                        .or_not(),
                )
                .map_with_span(|(((if_tk, cond), if_body), else_branch), span: Span| {
                    let (else_tk, else_body) = match else_branch {
                        Some((else_tk, else_body)) => (Some(else_tk), else_body),
                        // If an `if` expression has no trailing `else` block, we magic up one that just produces ()
                        None => (
                            None,
                            (
                                Ast::Block(vec![(Ast::Tuple(Vec::new()), span.clone(), None)]),
                                span.clone(),
                                None,
                            ),
                        ),
                    };
                    (
                        Ast::If(
                            if_tk,
                            Box::new(cond),
                            Box::new(if_body),
                            else_tk,
                            Box::new(else_body),
                        ),
                        span,
                        None,
                    )
                })
        })
        .boxed();

        // TODO change this so it only parses declarations or identifiers as args
        let lambda = declaration_parser(expr.clone())
            .or(pattern_parser().map(|pattern| {
                let span = pattern.1.clone();
                (
                    // HACK We are generating an imaginary token that does not exist, that might be bad
                    Ast::Declaration(pattern, (Token::Ctrl(':'), span.clone()), None, None, None),
                    span,
                    None,
                )
            }))
            .separated_by(just(Token::Ctrl(',')))
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
            .then(just(Token::Op("=>".to_string())).map_with_span(|tk, span| (tk, span)))
            .then(block.clone())
            .map(|((args, arrow_tk), body)| Ast::Lambda(args, arrow_tk, Box::new(body)))
            .boxed();

        // ATOMS ARE NOT AMBIGUOUS
        // { HELLO } IS AN ATOM BECAUSE IT IS UNAMBIGUOUSLY A BLOCK
        // 2 + 3 * 3 IS NOT AN ATOM BECAUSE OF OPERATOR PRECEDENCE

        let atom = lit
            .or(identifier.map(Ast::Variable))
            .or(lambda)
            .or(while_)
            .map_with_span(|expr, span| (expr, span, None))
            .or(block)
            .or(if_)
            // this is for the case we find 2 * (1 + 3), it should not afect function calls like print("hello world")
            .or(expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
            // HACK we move tuples down here to prevent a clash with just a parenthesised expresion
            .or(tuple.clone().map_with_span(|expr, span| (expr, span, None)))
            .recover_with(nested_delimiters(
                Token::Ctrl('('),
                Token::Ctrl(')'),
                [
                    (Token::Ctrl('['), Token::Ctrl(']')),
                    (Token::Ctrl('{'), Token::Ctrl('}')),
                ],
                |span| (Ast::Error, span, None),
            ))
            // Attempt to recover anything that looks like a list but contains errors
            .recover_with(nested_delimiters(
                Token::Ctrl('['),
                Token::Ctrl(']'),
                [
                    (Token::Ctrl('('), Token::Ctrl(')')),
                    (Token::Ctrl('{'), Token::Ctrl('}')),
                ],
                |span| (Ast::Error, span, None),
            ))
            .recover_with(nested_delimiters(
                Token::Ctrl('{'),
                Token::Ctrl('}'),
                [
                    (Token::Ctrl('('), Token::Ctrl(')')),
                    (Token::Ctrl('['), Token::Ctrl(']')),
                ],
                |span| (Ast::Error, span, None),
            ))
            .recover_with(skip_then_retry_until([
                Token::Ctrl(')'),
                Token::Ctrl(']'),
                Token::Ctrl('}'),
            ]))
            .boxed();

        // Function calls have very high precedence so we prioritise them
        let call = atom
            .clone()
            .then(tuple.clone())
            .map_with_span(|(caller, args), span| {
                if let Ast::Tuple(args) = args {
                    (Ast::Call(Box::new(caller), args), span, None)
                } else {
                    (Ast::Error, span, None)
                }
            })
            // atom is later because otherwhise add(1,2) would be parsed as add:identifier, (1,2):tuple
            .or(atom)
            .boxed();

        // The dot operator has the highest precedence after function call: a.add(b) =>  a . add(b) The add(b) has higher priorityst(Token::Op(".".to_string())).to(".");

        let dot = parse_with_less_precedence(operators!("."), call);
        let product = parse_with_less_precedence(operators!("*", "/"), dot);
        let sum = parse_with_less_precedence(operators!("+", "-"), product);
        let compare = parse_with_less_precedence(operators!("==", "!=", "<", "<=", ">", ">="), sum);
        let logic = parse_with_less_precedence(operators!("and", "or"), compare);

        let definition = declaration_parser(expr);

        let comment = filter_map(|span, tk| match &tk {
            Token::Comment(_) => Ok(Ast::Coment((tk, span))),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tk))),
        })
        .labelled("identifier")
        .boxed()
        .map_with_span(|c, span| (c, span, None));

        definition.or(logic).or(comment)
    })
}

pub fn ast_parser() -> impl Parser<Token, Vec<Anotated<Ast>>, Error = Simple<Token>> + Clone {
    expresion_parser().repeated().then_ignore(end())
}
