mod parsers;
pub mod token;

use crate::{
    common::{Operator, Span},
    diagnostic::Diagnostic,
    lexer::{
        parsers::{parse_float, parse_int},
        token::{Token, TokenType},
    },
};

const MULTICHAR_SYMBOLS: &[&str] = &[
    "+=", "-=", "*=", "/=", "%=", "==", "!=", ">=", "<=", "||", "&&", "..", "..=", "<<", ">>",
    "++", "->",
];

fn classify(lex: &str, span: Span) -> Option<Token<'_>> {
    let ty = match lex {
        "let" => TokenType::Let,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "while" => TokenType::While,
        "for" => TokenType::For,
        "in" => TokenType::In,
        "return" => TokenType::Return,
        "extern" => TokenType::Extern,
        "export" => TokenType::Export,
        "do" => TokenType::Do,
        "then" => TokenType::Then,
        "with" => TokenType::With,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "(" => TokenType::LParen,
        ")" => TokenType::RParen,
        "[" => TokenType::LSquare,
        "]" => TokenType::RSquare,
        "{" => TokenType::LCurly,
        "}" => TokenType::RCurly,
        ";" => TokenType::Semicolon,
        ":" => TokenType::Colon,
        "," => TokenType::Comma,
        "." => TokenType::Dot,
        "?" => TokenType::QuestionMark,
        "#" => TokenType::Hashtag,
        "@" => TokenType::At,
        "->" => TokenType::Arrow,
        "+" => TokenType::Operator(Operator::Plus),
        "-" => TokenType::Operator(Operator::Minus),
        "*" => TokenType::Operator(Operator::Star),
        "/" => TokenType::Operator(Operator::Slash),
        "%" => TokenType::Operator(Operator::Modulo),
        "=" => TokenType::Operator(Operator::Assign),
        "+=" => TokenType::Operator(Operator::PlusAssign),
        "-=" => TokenType::Operator(Operator::MinusAssign),
        "*=" => TokenType::Operator(Operator::StarAssign),
        "/=" => TokenType::Operator(Operator::SlashAssign),
        "%=" => TokenType::Operator(Operator::ModuloAssign),
        "==" => TokenType::Operator(Operator::Eq),
        "!=" => TokenType::Operator(Operator::Ne),
        ">" => TokenType::Operator(Operator::Gt),
        "<" => TokenType::Operator(Operator::Lt),
        ">=" => TokenType::Operator(Operator::Ge),
        "<=" => TokenType::Operator(Operator::Le),
        "|" => TokenType::Operator(Operator::Pipe),
        "||" => TokenType::Operator(Operator::LogOr),
        "&" => TokenType::Operator(Operator::Ampersand),
        "&&" => TokenType::Operator(Operator::LogAnd),
        "^" => TokenType::Operator(Operator::Caret),
        "~" => TokenType::Operator(Operator::Tilde),
        "!" => TokenType::Operator(Operator::Bang),
        ".." => TokenType::Operator(Operator::Range),
        "..=" => TokenType::Operator(Operator::RangeInclus),
        "<<" => TokenType::Operator(Operator::Lsh),
        ">>" => TokenType::Operator(Operator::Rsh),
        "++" => TokenType::Operator(Operator::Concat),
        _ => {
            if let Some(n) = parse_int(lex) {
                TokenType::IntLit(n)
            } else if let Some(n) = parse_float(lex) {
                TokenType::FloatLit(n)
            } else if lex.chars().all(|c| c == '_' || c.is_alphanumeric()) {
                TokenType::Identifier
            } else {
                return None;
            }
        }
    };

    Some(Token { ty, lex, span })
}

pub fn lex<'lex>(path: &str, source: &'lex str) -> Result<Vec<Token<'lex>>, Diagnostic> {
    let mut chars = source.char_indices().peekable();
    let len = source.len();

    let mut tokens = Vec::new();
    let mut tok_start = 0usize;
    let mut in_string = false;
    let mut skip = 0usize;

    while let Some((pos, ch)) = chars.next() {
        if skip > 0 {
            skip -= 1;
            continue;
        }

        if in_string {
            if ch == '"' {
                tokens.push(Token {
                    ty: TokenType::StringLit,
                    lex: &source[(tok_start + 1)..pos],
                    span: Span::from(tok_start..(pos + 1)),
                });
                tok_start = pos + 1;
                in_string = false;
            }
            continue;
        }

        match ch {
            ' ' | '\r' | '\n' | '\t' => {
                if tok_start < pos {
                    let tok = classify(&source[tok_start..pos], Span::from(tok_start..pos));
                    if let Some(tok) = tok {
                        tokens.push(tok);
                    } else {
                        return Err(Diagnostic::new(
                            &path,
                            format!(
                                "Unrecognized sequence of characters: `{:?}`",
                                &source[tok_start..pos]
                            ),
                            Span::from(tok_start..pos),
                        ));
                    }
                }
                tok_start = pos + 1;
            }
            ch if (ch == '.'
                && source[tok_start..pos]
                    .chars()
                    .all(|c| c.is_ascii_digit() || c == '_'))
                || ch.is_alphanumeric()
                || ch == '_' => {}
            '"' => {
                in_string = true;
                if tok_start < pos {
                    let tok = classify(&source[tok_start..pos], Span::from(tok_start..pos));
                    if let Some(tok) = tok {
                        tokens.push(tok);
                    } else {
                        return Err(Diagnostic::new(
                            &path,
                            format!(
                                "Unrecognized sequence of characters: `{:?}`",
                                &source[tok_start..pos]
                            ),
                            Span::from(tok_start..pos),
                        ));
                    }
                }
                tok_start = pos;
            }
            _ => {
                if tok_start < pos {
                    let tok = classify(&source[tok_start..pos], Span::from(tok_start..pos));
                    if let Some(tok) = tok {
                        tokens.push(tok);
                    } else {
                        return Err(Diagnostic::new(
                            &path,
                            format!(
                                "Unrecognized sequence of characters: `{:?}`",
                                &source[tok_start..pos]
                            ),
                            Span::from(tok_start..pos),
                        ));
                    }
                }

                tok_start = pos;

                if ch == '-'
                    && let Some((_, '-')) = chars.peek()
                {
                    chars.next();
                    let mut pos = 0;
                    for (p, ch) in chars.by_ref() {
                        if ch == '\n' {
                            pos = p;
                            break;
                        }
                    }
                    tok_start = pos + 1;
                    continue;
                }

                if ch == '{'
                    && let Some((_, '-')) = chars.peek()
                {
                    chars.next();
                    let mut terminated = false;
                    let mut pos = 0;
                    while let Some((p, ch)) = chars.next() {
                        if ch == '-'
                            && let Some((_, '}')) = chars.peek()
                        {
                            terminated = true;
                            chars.next();
                            pos = p;
                            break;
                        }
                    }
                    if !terminated {
                        return Err(Diagnostic::new(
                            &path,
                            "Unterminated block comment",
                            Span::from(tok_start..pos + 1),
                        ));
                    }
                    tok_start = pos + 2;
                    continue;
                }

                let mut continue_main_loop = false;
                for symbol in MULTICHAR_SYMBOLS {
                    if pos + symbol.len() > len {
                        continue;
                    }

                    let built_symbol = &source[pos..pos + symbol.len()];

                    if *symbol == built_symbol {
                        tokens.push(
                            classify(
                                &source[tok_start..(pos + symbol.len())],
                                Span::from(tok_start..(pos + symbol.len())),
                            )
                            .unwrap(),
                        );
                        skip = symbol.len() - 1;
                        tok_start = pos + symbol.len();
                        continue_main_loop = true;
                        break;
                    }
                }
                if continue_main_loop {
                    continue;
                }

                let tok = classify(&source[pos..=pos], Span::from(pos..(pos + 1)));
                if let Some(tok) = tok {
                    tokens.push(tok);
                } else {
                    return Err(Diagnostic::new(
                        &path,
                        format!(
                            "Unrecognized sequence of characters: `{:?}`",
                            &source[tok_start..pos]
                        ),
                        Span::from(tok_start..pos),
                    ));
                }

                tok_start = pos + 1;
            }
        }
    }

    if in_string {
        return Err(Diagnostic::new(
            &path,
            format!(
                "Unterminated string literal: `{:?}`",
                &source[(tok_start + 1)..len]
            ),
            Span::from(tok_start..len),
        ));
    }

    if tok_start < len {
        let tok = classify(&source[tok_start..len], Span::from(tok_start..len));
        if let Some(tok) = tok {
            tokens.push(tok);
        } else {
            return Err(Diagnostic::new(
                &path,
                format!(
                    "Unrecognized sequence of characters: `{:?}`",
                    &source[tok_start..len]
                ),
                Span::from(tok_start..len),
            ));
        }
    }

    Ok(tokens)
}
