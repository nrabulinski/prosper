use crate::span::Span;

#[derive(Debug, Copy, Clone)]
pub enum LitKind<'a> {
    Str(&'a [u8]),
    Char(u8),
    Int(i128),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Copy, Clone)]
pub struct Literal<'a> {
    pub kind: LitKind<'a>,
    pub span: Span,
}

#[derive(Debug, Copy, Clone)]
pub enum TokenKind<'a> {
    Plus, PlusEqual, Minus, MinusEqual, Star, StarEqual, Slash, SlashEqual,
    Percent, PercentEqual, Caret, CaretEqual, Bar, BarEqual, And, AndEqual, Tilde,
    Equal, EqualEqual, Less, LessEqual, Greater, GreaterEqual, Not, NotEqual,
    GreaterGreater, GreaterGreaterEqual, LessLess, LessLessEqual,
    LParen, RParen, LBracket, RBracket, LBrace, RBrace,
    Dot, DotDot, Comma, Semi, Colon, Question,
    ThinArrow, FatArrow, AndAnd, BarBar, Pipeline, // |>

    // Keywords
    Fun, Defer, Let, While, For, Do, Guard, Return, Enum, Union, Named, As, Break, In,

    Ident(&'a str), Lit(Literal<'a>),

    Eof
}

impl<'a> TokenKind<'a> {
    pub fn token(self, span: Span) -> Token<'a> { //start: usize, end: usize) -> Token<'a> {
        Token {
            kind: self,
            span //: Span { start, end }
        }
    }

    pub fn as_str(&self) -> &'static str {
        use TokenKind::*;
        match self {
            Plus => "+ operator",
            Minus => "- operator", 
            Fun => "\"fun\" keyword",
            Ident(_) => "an identifier",
            Lit(_) => "literal",
            _ => "some token"
        }
    }

    // pub fn len(&self) -> usize {
    //     use TokenKind::*;
    //     match self {
    //         Plus | Minus | Star | Slash | Percent | Caret | Bar | And | Tilde | Equal | Less | Greater | Not | LParen | RParen | LBracket | RBracket | Dot | Comma => 1,
    //         PlusEqual | MinusEqual | StarEqual | SlashEqual | PercentEqual | CaretEqual | BarEqual | AndEqual | EqualEqual | LessEqual | GreaterEqual | NotEqual | GreaterGreater | LessLess | Do | As | DotDot => 2,
    //         GreaterGreaterEqual | LessLessEqual | Fun | Let | For => 3,
    //         Enum => 4,
    //         Defer | While | Guard | Union | Named | Break => 5,
    //         Return => 6,
    //         Ident(ident) => ident.len(),
    //         Lit(literal) => 
    //     }
    // }
}

#[derive(Debug, Copy, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,
}