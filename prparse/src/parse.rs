use crate::span::Span;
use crate::token::Token;
use crate::token::TokenKind as TK;
use crate::ast::*;
use crate::Message;
use std::cell::RefCell;
use std::rc::Rc;

pub type PResult<'i, 'a, T> = Result<(T, &'i [Token<'a>]), Message>;

pub trait Parse<'a>: Sized {
    fn parse<'i>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Self>;
}

impl<'a> Parse<'a> for AST<'a> {
    fn parse<'i>(mut input: &'i [Token<'a>]) -> PResult<'i, 'a, Self> {
        let mut items = Vec::new();
        while input.len() > 1 {
            let i = Item::parse(input)?;
            items.push(i.0);
            input = i.1;
        }
        let res = AST { items };
        Ok((res, input))
    }
}

impl<'a> Parse<'a> for Item<'a> {
    fn parse<'i>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Self> {
        match input[0] {
            Token { kind: TK::Fun, .. } => if let Ok((func, input)) = FunDef::parse(input) {
                let span = func.span;
                Ok((Item {
                    kind: ItemKind::FunDef(func),
                    span
                }, input))
            } else {
                let (func, input) = Function::parse(input)?;
                let span = func.span;
                Ok((Item {
                    kind: ItemKind::Func(Box::new(func)),
                    span
                }, input))
            },
            t => Err(Message::new("Unexpected token", t.span))
        }
    }
}

impl<'a> Parse<'a> for FunDef<'a> {
    fn parse<'i>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Self> {
        let (fun, input) = Fun::parse(input)?;
        let start = fun.span;
        println!("{:?}", fun);
        let (ident, input) = Ident::parse(input)?;
        println!("{:?}", ident);
        let (args, mut input) = fargs(input)?;
        println!("{:?}", args);
        let ret = match input[0] {
            Token { kind: TK::ThinArrow, .. } => {
                let (ty, rest) = Type::parse(&input[1..])?;
                input = rest;
                ty
            },
            _ => Type {
                kind: TypeKind::Void,
                span: Span::EMPTY
            }
        };
        let (semi, input) = Semi::parse(input)?;
        let end = semi.span;
        let res = FunDef {
            ident,
            args,
            ret: Rc::new(RefCell::new(ret)),
            span: Span::combine(start, end)
        };
        Ok((res, input))
    }
}

impl<'a> Parse<'a> for Function<'a> {
    fn parse<'i>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Self> {
        let (fun, input) = Fun::parse(input)?;
        let start = fun.span;
        println!("{:?}", fun);
        let (ident, input) = Ident::parse(input)?;
        println!("{:?}", ident);
        let (args, mut input) = fargs(input)?;
        println!("{:?}", args);
        let ret = match input[0] {
            t @ Token { kind: TK::ThinArrow, .. } => {
                let (ty, rest) = Type::parse(&input[1..])?;
                input = rest;
                Some((t, ty))
            },
            _ => None
        };
        let (body, ret, input) = match input[0] {
            Token { kind: TK::FatArrow, .. } => {
                if let Some((arrow, ret)) = ret {
                    return Err(Message::new("Remove explicit return type", Span::combine(arrow.span, ret.span)));
                }
                let (body, input) = Expr::parse(&input[1..])?;
                (body, Type {
                    kind: TypeKind::Infer,
                    span: Span::EMPTY
                }, input)
            },
            Token { kind: TK::LBrace, ..} => {
                let (body, input) = Block::parse(input)?;
                let span = body.span;
                let body = Expr::new(
                    ExprKind::Block(body),
                    span
                );
                (body, ret.map(|(_, ret)| ret).unwrap_or_else(|| Type {
                    kind: TypeKind::Void,
                    span: Span::EMPTY
                }), input)
            },
            t => return Err(Message::new("Expected `=>` or `{`", t.span))
        };
        let ret = Rc::new(RefCell::new(ret));
        println!("{:?}", body);
        let end = body.span;
        let func = Function {
            ident,
            args,
            ret,
            body,
            span: Span::combine(start, end)
        };
        Ok((func, input))
        // Err(Message::new("this is the ident", ident.span))
    }
}

fn fargs<'a, 'i>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Vec<FArg<'a>>> {
    let mut res = Vec::new();
    let mut input = match input[0] {
        Token { kind: TK::LParen, .. } => &input[1..],
        _ => return Ok((res, input))
    };
    loop {
        match input[0] {
            Token { kind: TK::RParen, .. } => return Ok((res, &input[1..])),
            t => {
                if let Some(arg) = res.last() {
                    input = match t {
                        Token { kind: TK::Comma, .. } => &input[1..],
                        _ => return Err(Message::new("Missing comma after this argument", arg.span))
                    };
                }
                if let Token { kind: TK::Comma, span } = input[0] {
                    return Err(Message::new("Remove this comma", span))
                }
                let (arg, rest) = FArg::parse(input)?;
                input = rest;
                res.push(arg);
            }
        }
    }
}

impl<'a> Parse<'a> for FArg<'a> {
    fn parse<'i>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Self> {
        let (ident, input) = Ident::parse(input)?;
        let input = match input[0] {
            Token { kind: TK::Colon, .. } => &input[1..],
            t => return Err(Message::new("Expected a colon here", t.span))
        };
        let (ty, input) = Type::parse(input)?;
        let span = Span::combine(ident.span, ty.span);
        Ok((FArg { ident, ty, span }, input))
    }
}

impl<'a> Parse<'a> for Ident<'a> {
    fn parse<'i>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Self> {
        match input[0] {
            Token { kind: TK::Ident(inner), span } =>
                Ok((Ident { inner, span }, &input[1..])),
            t =>
                Err(Message::new("Expected an identifier", t.span))
        }
    }
}

impl<'a> Parse<'a> for Type<'a> {
    fn parse<'i>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Self> {
        Ok(match input[0] {
            Token { kind: TK::Question, span } => {
                let (inner, input) = Type::parse(&input[1..])?;
                (Type {
                    kind: TypeKind::Nullable(Box::new(inner)),
                    span
                }, input)
            },
            Token { kind: TK::Star, span } => {
                let (inner, input) = Type::parse(&input[1..])?;
                (Type {
                    kind: TypeKind::Ptr(Box::new(inner)),
                    span
                }, input)
            },
            Token { kind: TK::LParen, span } => {
                let start = span;
                let (inner, input, end) = {
                    let mut res: Vec<Type<'a>> = Vec::new();
                    let mut input = &input[1..];
                    loop {
                        match input[0] {
                            Token { kind: TK::RParen, span } => break (res, &input[1..], span),
                            t => {
                                if let Some(ty) = res.last() {
                                    input = match t {
                                        Token { kind: TK::Comma, .. } => &input[1..],
                                        _ => return Err(Message::new("Missing comma after this type", ty.span))
                                    };
                                }
                                if let Token { kind: TK::Comma, span } = input[0] {
                                    return Err(Message::new("Remove this comma", span))
                                }
                                let (ty, rest) = Type::parse(input)?;
                                input = rest;
                                res.push(ty);
                            }
                        }
                    }
                };
                (Type {
                    kind: TypeKind::Tuple(inner),
                    span: Span::combine(start, end)
                }, input)
            },
            Token { kind: TK::Ident(ident), span } => {
                (Primitive::maybe_str(ident).map(|p| Type {
                    kind: TypeKind::Primitive(p),
                    span
                }).unwrap_or_else(|| Type {
                    kind: TypeKind::Ident(Ident { inner: ident, span }),
                    span
                }), &input[1..])
            },
            t => return Err(Message::new("Expected a type", t.span))
        })
    }
}

impl<'a> Parse<'a> for Block<'a> {
    fn parse<'i>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Self> {
        println!("Trying to parse a block");
        let (label, input) = match Ident::parse(input) {
            Ok((label, input)) => match input[0] {
                Token { kind: TK::Colon, .. } => (Some(label), &input[1..]),
                t => return Err(Message::new("Expected `:`", t.span))
            },
            Err(_) => (None, input)
        };
        println!("Parsed a label");
        match input[0] {
            Token { kind: TK::LBrace, span: start } => {
                let start = label.as_ref().map(|l| l.span).unwrap_or(start);
                let mut input = &input[1..];
                let mut stmts = Vec::new();
                let end = loop {
                    while let Ok((_, rest)) = Semi::parse(input) { input = rest; }
                    if let Token { kind: TK::RBrace, span } = input[0] { break span; }
                    let (s, rest) = Stmt::parse(input)?;
                    input = rest;
                    let last_span = match &s {
                        Stmt::Expr(e) if e.requires_semi() => { Some(e.span) },
                        _ => None
                    };
                    stmts.push(s);
                    if let Token { kind: TK::RBrace, span } = input[0] { break span; }
                    else if let Some(span) = last_span {
                        return Err(Message::new("Unexpected token, did you forget a `;`?", span))
                    }
                };
                let items: Vec<_> = stmts
                    .drain_filter(|stmt| matches!(stmt, Stmt::Item(_)))
                    .map(|i| match i {
                        Stmt::Item(i) => i,
                        _ => unreachable!()
                    }).collect();
                let block = Block {
                    label,
                    items,
                    stmts,
                    span: Span::combine(start, end)
                };
                Ok((block, &input[1..]))
            },
            t => Err(Message::new("Expected `{` or a label", t.span))
        }
    }
}

impl<'a> Parse<'a> for Stmt<'a> {
    fn parse<'i>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Self> {
        match input[0] {
            Token { kind: TK::Let, .. } =>
                LetStmt::parse(input).map(|(l, input)| (Stmt::Let(l), input)),
            Token { kind: TK::Fun, .. } =>
                Item::parse(input).map(|(i, input)| (Stmt::Item(i), input)),
            Token { kind: TK::Return, .. } =>
                Expr::parse(&input[1..]).map(|(e, input)| (Stmt::Return(e), input)),
            _ => {
                let (e, rest) = Expr::parse(input)?;
                if let Token { kind: TK::Semi, .. } = rest[0] {
                    Ok((Stmt::Semi(e), &rest[1..]))
                } else {
                    Ok((Stmt::Expr(e), rest))
                }
            }
        }
    }
}

impl<'a> Parse<'a> for LetStmt<'a> {
    fn parse<'i>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Self> {
        let (lt, input) = Let::parse(input)?;
        let (ident, input) = Ident::parse(input)?;
        let (ty, input) = match input[0] {
            Token { kind: TK::Colon, .. } => {
                let (ty, input) = Type::parse(&input[1..])?;
                (ty, input)
            },
            _ => (Type {
                kind: TypeKind::Infer,
                span: Span::EMPTY
            }, input)
        };
        let (_, input) = Equal::parse(input)?;
        let (init, input) = Expr::parse(input)?;
        let init = Box::new(init);
        let (semi, input) = Semi::parse(input)?;
        let span = Span::combine(lt.span, semi.span);
        Ok((LetStmt {
            ident,
            ty: RefCell::new(ty),
            init,
            span
        }, input))
    }
}

impl<'a> Parse<'a> for Expr<'a> {
    fn parse<'i>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Self> {
        match (input[0], input[1]) {
            (Token { kind: TK::Ident(_), .. }, Token { kind: TK::Colon, .. }) |
            (Token { kind: TK::LBrace, .. }, _) => {
                let (block, input) = Block::parse(input)?;
                let span = block.span;
                Ok((Expr::new(
                    ExprKind::Block(block),
                    span
                ), input))
            },
            _ => op_eq(input)
        }
    }
}

fn op_eq<'i, 'a>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Expr<'a>> {
    let (mut expr, mut input) = or(input)?;
    while let t @ Token { kind:
      TK::Equal
    | TK::PlusEqual
    | TK::MinusEqual
    | TK::StarEqual
    | TK::SlashEqual
    | TK::PercentEqual
    | TK::AndEqual
    | TK::BarEqual
    | TK::CaretEqual
    | TK::LessLessEqual
    | TK::GreaterGreaterEqual, .. } = input[0] {
        let op = BinOp::from_token(t.kind);
        let (right, rest) = op_eq(&input[1..])?;
        let span = Span::combine(expr.span, right.span);
        expr = Expr::new(ExprKind::Binary {
            lhs: Box::new(expr),
            op,
            rhs: Box::new(right)
        }, span);
        input = rest;
    }
    Ok((expr, input))
}

fn or<'i, 'a>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Expr<'a>> {
    let (mut expr, mut input) = and(input)?;
    while let Token { kind: TK::BarBar, .. } = input[0] {
        let op = BinOp::Or;
        let (right, rest) = and(&input[1..])?;
        let span = Span::combine(expr.span, right.span);
        expr = Expr::new(ExprKind::Binary {
            lhs: Box::new(expr),
            op,
            rhs: Box::new(right)
        }, span);
        input = rest;
    }
    Ok((expr, input))
}

fn and<'i, 'a>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Expr<'a>> {
    let (mut expr, mut input) = eq(input)?;
    while let Token { kind: TK::AndAnd, .. } = input[0] {
        let op = BinOp::And;
        let (right, rest) = eq(&input[1..])?;
        let span = Span::combine(expr.span, right.span);
        expr = Expr::new(ExprKind::Binary {
            lhs: Box::new(expr),
            op,
            rhs: Box::new(right)
        }, span);
        input = rest;
    }
    Ok((expr, input))
}

fn eq<'i, 'a>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Expr<'a>> {
    let (mut expr, mut input) = bit_or(input)?;
    while let t @ Token { kind:
      TK::EqualEqual
    | TK::NotEqual
    | TK::Less
    | TK::LessEqual
    | TK::Greater
    | TK::GreaterEqual, .. } = input[0] {
        let op = BinOp::from_token(t.kind);
        let (right, rest) = bit_or(&input[1..])?;
        let span = Span::combine(expr.span, right.span);
        expr = Expr::new(ExprKind::Binary {
            lhs: Box::new(expr),
            op,
            rhs: Box::new(right)
        }, span);
        input = rest;
    }
    Ok((expr, input))
}

fn bit_or<'i, 'a>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Expr<'a>> {
    let (mut expr, mut input) = xor(input)?;
    while let Token { kind: TK::Bar, .. } = input[0] {
        let op = BinOp::BitOr;
        let (right, rest) = xor(&input[1..])?;
        let span = Span::combine(expr.span, right.span);
        expr = Expr::new(ExprKind::Binary {
            lhs: Box::new(expr),
            op,
            rhs: Box::new(right)
        }, span);
        input = rest;
    }
    Ok((expr, input))
}

fn xor<'i, 'a>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Expr<'a>> {
    let (mut expr, mut input) = bit_and(input)?;
    while let Token { kind: TK::Caret, .. } = input[0] {
        let op = BinOp::BitXor;
        let (right, rest) = bit_and(&input[1..])?;
        let span = Span::combine(expr.span, right.span);
        expr = Expr::new(ExprKind::Binary {
            lhs: Box::new(expr),
            op,
            rhs: Box::new(right)
        }, span);
        input = rest;
    }
    Ok((expr, input))
}

fn bit_and<'i, 'a>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Expr<'a>> {
    let (mut expr, mut input) = bit_shift(input)?;
    while let Token { kind: TK::And, .. } = input[0] {
        let op = BinOp::BitAnd;
        let (right, rest) = bit_shift(&input[1..])?;
        let span = Span::combine(expr.span, right.span);
        expr = Expr::new(ExprKind::Binary {
            lhs: Box::new(expr),
            op,
            rhs: Box::new(right)
        }, span);
        input = rest;
    }
    Ok((expr, input))
}

fn bit_shift<'i, 'a>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Expr<'a>> {
    let (mut expr, mut input) = addition(input)?;
    while let t @ Token { kind: TK::LessLess | TK::GreaterGreater, .. } = input[0] {
        let op = BinOp::from_token(t.kind);
        let (right, rest) = addition(&input[1..])?;
        let span = Span::combine(expr.span, right.span);
        expr = Expr::new(ExprKind::Binary {
            lhs: Box::new(expr),
            op,
            rhs: Box::new(right)
        }, span);
        input = rest;
    }
    Ok((expr, input))
}

fn addition<'i, 'a>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Expr<'a>> {
    let (mut expr, mut input) = mult(input)?;
    while let t @ Token { kind: TK::Plus | TK::Minus, .. } = input[0] {
        let op = BinOp::from_token(t.kind);
        let (right, rest) = mult(&input[1..])?;
        let span = Span::combine(expr.span, right.span);
        expr = Expr::new(ExprKind::Binary {
            lhs: Box::new(expr),
            op,
            rhs: Box::new(right)
        }, span);
        input = rest;
    }
    Ok((expr, input))
}

fn mult<'i, 'a>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Expr<'a>> {
    let (mut expr, mut input) = cast(input)?;
    while let t @ Token { kind: TK::Star | TK::Slash, .. } = input[0] {
        let op = BinOp::from_token(t.kind);
        let (right, rest) = cast(&input[1..])?;
        let span = Span::combine(expr.span, right.span);
        expr = Expr::new(ExprKind::Binary {
            lhs: Box::new(expr),
            op,
            rhs: Box::new(right)
        }, span);
        input = rest;
    }
    Ok((expr, input))
}

fn cast<'i, 'a>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Expr<'a>> {
    let (mut expr, mut input) = unary(input)?;
    while let Token { kind: TK::As, .. } = input[0] {
        let (_, rest) = As::parse(&input[1..])?;
        let (ty, rest) = Type::parse(rest)?;
        let span = Span::combine(expr.span, ty.span);
        expr = Expr::new(ExprKind::Cast {
            expr: Box::new(expr),
            ty
        }, span);
        input = rest;
    }
    Ok((expr, input))
}

fn unary<'i, 'a>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Expr<'a>> {
    while let t @ Token { kind:
      TK::Star
    | TK::Not
    | TK::Tilde
    | TK::And
    | TK::Minus, .. } = input[0] {
        let op = UnOp::from_token(t.kind);
        let (right, input) = unary(&input[1..])?;
        let span = Span::combine(t.span, right.span);
        return Ok((Expr::new(
            ExprKind::Unary { op, expr: Box::new(right) },
            span
        ), input))
    }
    fcall(input)
}

fn fcall<'i, 'a>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Expr<'a>> {
    let (mut expr, mut input) = primary(input)?;
    while let t @ Token { kind: TK::LParen, .. } = input[0] {
        let (args, rest, end) = {
            let mut res: Vec<Expr<'a>> = Vec::new();
            let mut input = &input[1..];
            loop {
                match input[0] {
                    Token { kind: TK::RParen, span } => break (res, &input[1..], span),
                    t => {
                        if let Some(ty) = res.last() {
                            input = match t {
                                Token { kind: TK::Comma, .. } => &input[1..],
                                _ => return Err(Message::new("Missing comma after this argument", ty.span))
                            };
                        }
                        if let Token { kind: TK::Comma, span } = input[0] {
                            return Err(Message::new("Remove this comma", span))
                        }
                        let (ty, rest) = Expr::parse(input)?;
                        input = rest;
                        res.push(ty);
                    }
                }
            }
        };
        let span = Span::combine(t.span, end);
        expr = Expr::new(ExprKind::Call {
            expr: Box::new(expr), args
        }, span);
        input = rest;
    } 

    Ok((expr, input))
}

fn primary<'i, 'a>(input: &'i [Token<'a>]) -> PResult<'i, 'a, Expr<'a>> {
    match input[0] {
        Token { kind: TK::Lit(literal), span } => Ok((Expr::new(
            ExprKind::Lit(literal),
            span
        ), &input[1..])),
        Token { kind: TK::Ident(inner), span } => Ok((Expr::new(
            ExprKind::Ident(Ident { inner, span }),
            span
        ), &input[1..])),
        Token { kind: TK::LParen, span: start } => {
            let (mut inner, rest) = Expr::parse(&input[1..])?;
            match rest[0] {
                Token { kind: TK::RParen, span: end } => {
                    inner.span = Span::combine(start, end);
                    Ok((inner, &rest[1..]))
                },
                t => Err(Message::new("Missing right parenthesis", t.span))
            }
        }
        t => Err(Message::new(format!("Expected primary, got {:?}", t), t.span))
    }
}