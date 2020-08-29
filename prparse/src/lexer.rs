use crate::token::{Token, TokenKind, Literal, LitKind, IntLit, FloatLit};
use TokenKind::*;
use std::collections::HashMap;
use crate::CResult;
use crate::span::Span;

pub struct Lexer<'a> {
    source: &'a [u8],
    pos: usize,
    keywords: HashMap<String, TokenKind<'a>>,
}

macro_rules! keywords_map {
    ($($lit:literal => $val:expr),+ $(,)?) => {{
        let mut keywords = HashMap::new();
        $(
            keywords.insert($lit.to_string(), $val);
        )+
        keywords
    }}
}

macro_rules! glue {
    ($s:expr, $l:expr, $v1:expr; $($g2:literal => $v2:expr $(; $g:literal => $v:expr)*),+) => {
        match $s.peek() {
            $(
            Some($g2) => {
                $s.advance();
                glue! {$s, $l + 1, $v2 $(; $g => $v)*}
            }),+
            _ => $v1.token(Span::new($s.pos - $l, $s.pos - 1))
        }
    };
    ($s:expr, $l:expr, $v:expr) => {
        $v.token(Span::new($s.pos - $l, $s.pos - 1))
    }
}

fn is_whitespace(c: char) -> bool {
    [' ', '\t', '\r', '\n'].contains(&c)
}

fn is_hex_digit(c: char) -> bool {
    matches!(c, '0'..='9' | 'a'..='f' | 'A'..='F')
}

fn is_digit(c: char) -> bool {
    matches!(c, '0'..='9')
}

fn is_alpha(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '_')
}

fn is_alphanumeric(c: char) -> bool {
    is_alpha(c) || is_digit(c)
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let keywords = keywords_map! {
            "fun" => Fun,
            "defer" => Defer,
            "let" => Let,
            "while" => While,
            "for" => For,
            "do" => Do,
            "guard" => Guard,
            "return" => Return,
            "enum" => Enum,
            "union" => Union,
            "named" => Named,
            "as" => As,
            "break" => Break,
            "in" => In,
            "true" => Lit(Literal {
                kind: LitKind::Bool(true),
                span: Span::EMPTY
            }),
            "false" => Lit(Literal {
                kind: LitKind::Bool(false),
                span: Span::EMPTY
            })
        };
        println!("tokenizing: {:?}", source);
        let source = source.as_bytes();
        Lexer { source, pos: 0, keywords }
    }

    pub fn tokenize(mut self) -> CResult<Vec<Token<'a>>> {
        let mut res: CResult<Vec<Token<'a>>> = CResult::default();
        while self.good() {
            let c = self.advance();
            let t = match c {
                '+' => glue!(self, 1, Plus; '=' => PlusEqual),
                '-' => glue!(self, 1, Minus; '=' => MinusEqual, '>' => ThinArrow),
                '*' => glue!(self, 1, Star; '=' => StarEqual),
                '/' => match self.peek() {
                    Some('=') => {
                        self.advance();
                        SlashEqual.token(Span::new(self.pos - 2, self.pos - 1))
                    },
                    Some('/') => {
                        self.advance();
                        while let Some(_) = self.peek() {
                            if self.advance() == '\n' {
                                break;
                            }
                        }
                        continue;
                    },
                    Some('*') => {
                        self.advance();
                        let mut n = 1;
                        while let Some(_) = self.peek() {
                            match self.advance() {
                                '/' if matches!(self.peek(), Some('*')) => {
                                    self.advance();
                                    n += 1;
                                },
                                '*' if matches!(self.peek(), Some('/')) => {
                                    self.advance();
                                    n -= 1;
                                },
                                _ => continue
                            }
                            if n == 0 { break; }
                        }
                        continue;
                    }
                    _ => Slash.token(Span::new(self.pos - 1, self.pos - 1)),
                },
                '%' => glue!(self, 1, Percent; '=' => PercentEqual),
                '^' => glue!(self, 1, Caret; '=' => CaretEqual),
                '|' => glue!(self, 1, Bar;
                    '|' => BarBar,
                    '=' => BarEqual),
                '&' => glue!(self, 1, And;
                    '&' => AndAnd,
                    '=' => AndEqual),
                '=' => glue!(self, 1, Equal;
                    '=' => EqualEqual,
                    '>' => FatArrow),
                '<' => glue!(self, 1, Less;
                    '=' => LessEqual,
                    '<' => LessLess; '=' => LessLessEqual),
                '>' => glue!(self, 1, Greater;
                    '=' => GreaterEqual,
                    '>' => GreaterGreater; '=' => GreaterGreaterEqual),
                '!' => glue!(self, 1, Not; '=' => NotEqual),
                '~' => Tilde.token(self.current_pos()),
                '.' => glue!(self, 1, Dot; '.' => DotDot),
                '(' => LParen.token(self.current_pos()),
                ')' => RParen.token(self.current_pos()),
                '[' => LBracket.token(self.current_pos()),
                ']' => RBracket.token(self.current_pos()),
                ',' => Comma.token(self.current_pos()),
                ';' => Semi.token(self.current_pos()),
                ':' => Colon.token(self.current_pos()),
                '?' => Question.token(self.current_pos()),
                '{' => LBrace.token(self.current_pos()),
                '}' => RBrace.token(self.current_pos()),
                // '\'' => match self.peek() {
                //     Some('\\') => 
                //     None => return 
                // },
                '"' => {
                    let start = self.pos;
                    while let Some(c) = self.peek() {
                        if c == '"' { break; }
                        match self.advance() {
                            '\\' => match self.peek() {
                                Some('"') => { self.advance(); },
                                Some('x' | 'X') => {
                                    self.advance();
                                    match self.peek() {
                                        None | Some('"') => return res.err("Numeric character escape is too short", self.current_pos()),
                                        Some(c) if is_hex_digit(c) => continue,
                                        Some(c) => return res.err(format!("'{}' is not a valid hexadecimal character", c), self.next_pos())
                                    }
                                }
                                Some(c) => return res.err(format!("Unknown escape character \"{}\"", c), self.next_pos()),
                                None => break,
                            }
                            _ => continue,
                        }
                    }
                    if !self.good() {
                        return res.err("Unterminated string, try adding a '\"' here", self.next_pos())
                    }
                    self.advance();
                    let inner = &self.source[start..self.pos - 1];
                    let outer_span = Span::new(start - 1, self.pos - 1);
                    let inner_span = if inner.len() == 0 {
                        Span::EMPTY
                    } else {
                        Span::new(start, self.pos - 1)
                    };
                    Lit(Literal {
                        kind: LitKind::Str(inner),
                        span: inner_span
                    }).token(outer_span)
                },
                '0' if matches!(self.peek(), Some('x')) => { unimplemented!() },
                c if is_digit(c) => {
                    let (int, span) = self.number();
                    if matches!((self.peek(), self.peek_next()), (Some('.'), Some(d)) if is_digit(d)) {
                        self.advance(); self.advance();
                        let (_, dspan) = self.number();
                        let float = &self.source[span.start..dspan.end + 1];
                        // SAFETY: This should never panic as we're checking if all the characters are valid digits
                        let float = std::str::from_utf8(float).unwrap();
                        let float: f64 = float.parse().unwrap();
                        let span = Span::combine(span, dspan);
                        Lit(Literal {
                            kind: LitKind::Float(FloatLit::F64(float)),
                            span
                        }).token(span)
                    } else {
                        // SAFETY: This should never panic as we're checking if all the characters are valid digits
                        let int = std::str::from_utf8(int).unwrap();
                        let int = match int.parse::<i32>() {
                            Ok(val) => IntLit::I32(val),
                            Err(_e) => return res.err("Bad number", span)
                        };
                        Lit(Literal {
                            kind: LitKind::Int(int),
                            span
                        }).token(span)
                    }
                }
                c if is_whitespace(c) => continue,
                c if is_alpha(c) => {
                    let start = self.pos - 1;
                    while let Some(c) = self.peek() {
                        if is_alphanumeric(c) { self.advance(); }
                        else { break; }
                    }
                    let ident = &self.source[start..self.pos];
                    let ident = std::str::from_utf8(ident).unwrap();
                    let span = Span::new(start, self.pos - 1);
                    self.keywords.get(ident).map(|t| {
                        let mut t = *t;
                        if let Lit(ref mut literal) = t {
                            literal.span = span;
                        }
                        t
                    }).unwrap_or_else(|| Ident(ident)).token(span)
                },
                _ => return res.err("Unexpected character", self.current_pos())
            };
            (*res).push(t);
        }
        (*res).push(Eof.token(self.current_pos()));
        res
    }

    fn number(&mut self) -> (&'a [u8], Span) {
        let start = self.pos - 1;
        while let Some(c) = self.peek() {
            if is_digit(c) { self.advance(); }
            else { break; }
        }
        let r = start..self.pos;
        (&self.source[r], Span::new(start, self.pos - 1))
    }

    fn current_pos(&self) -> Span {
        let s = self.pos - 1;
        Span::new(s, s)
    }

    fn next_pos(&self) -> Span {
        let s = self.pos;
        Span::new(s, s)
    }

    fn advance(&mut self) -> char { self.pos += 1; self.source[self.pos - 1] as char }

    fn peek(&self) -> Option<char> { self.source.get(self.pos).map(|&c| c as char) }

    fn peek_next(&self) -> Option<char> { self.source.get(self.pos + 1).map(|&c| c as char) }

    fn good(&self) -> bool {
        self.pos < self.source.len()
    }
}