use crate::span::Span;
use crate::token::TokenKind as TK;
use crate::token::Token;
use crate::token::Literal;
use std::cell::RefCell;
use std::rc::Rc;
use std::rc::Weak;
use std::mem::discriminant;

#[derive(Debug)]
pub struct AST<'a> {
    pub items: Vec<Item<'a>>
}

#[derive(Debug, Clone)]
pub enum ItemKind<'a> {
    Func(Box<Function<'a>>),
    FunDef(FunDef<'a>),
}

#[derive(Debug, Clone)]
pub struct Item<'a> {
    pub kind: ItemKind<'a>,
    pub span: Span,
}

impl<'a> Item<'a> {
    pub fn into_func(&self) -> &Function<'a> {
        match &self.kind {
            ItemKind::Func(f) => f,
            _ => panic!("{:?} is not a function", self)
        }
    }

    pub fn ident(&self) -> Ident<'a> {
        match &self.kind {
            ItemKind::Func(f) => f.ident,
            ItemKind::FunDef(f) => f.ident
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunDef<'a> {
    pub ident: Ident<'a>,
    pub args: Vec<FArg<'a>>,
    pub ret: Rc<RefCell<Type<'a>>>,
    pub span: Span
}

impl<'a> FunDef<'a> {
    pub fn to_fn_type(&self) -> TypeKind<'a> {
        TypeKind::Fun(FunType {
            args: self.args.iter().map(|arg| arg.ty.clone()).collect::<Vec<_>>(),
            ret: Rc::downgrade(&self.ret)
        })
    }
}

#[derive(Debug, Clone)]
pub struct Function<'a> {
    pub ident: Ident<'a>,
    pub args: Vec<FArg<'a>>,
    pub ret: Rc<RefCell<Type<'a>>>,
    pub body: Expr<'a>,
    pub span: Span,
}

impl<'a> Function<'a> {
    pub fn to_fn_type(&self) -> TypeKind<'a> {
        TypeKind::Fun(FunType {
            args: self.args.iter().map(|arg| arg.ty.clone()).collect::<Vec<_>>(),
            ret: Rc::downgrade(&self.ret)
        })
    }
}

#[derive(Debug, Clone)]
pub struct FArg<'a> {
    pub ident: Ident<'a>,
    pub ty: Type<'a>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Ident<'a> {
    pub inner: &'a str,
    pub span: Span,
}

impl<'a> PartialEq for Ident<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl<'a> Eq for Ident<'a> {}

// impl<'a> std::hash::Hash for Ident<'a> {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         self.inner.hash(state)
//     }
// }

#[derive(Debug, Clone, Copy)]
pub enum Int {
    I8, U8,
    I16, U16,
    I32, U32,
    I64, U64,
    Generic
}

impl PartialEq for Int {
    #[inline]
    fn eq(&self, other: &Int) -> bool {
        match (self, other) {
            (Int::Generic, _) | (_, Int::Generic) => true,
            _ => discriminant(self) == discriminant(other),
        }
    }
}

impl Eq for Int {}

#[derive(Debug, Clone, Copy)]
pub enum Float {
    F32, F64,
    Generic
}

impl PartialEq for Float {
    #[inline]
    fn eq(&self, other: &Float) -> bool {
        match (self, other) {
            (Float::Generic, _) | (_, Float::Generic) => true,
            _ => discriminant(self) == discriminant(other),
        }
    }
}

impl Eq for Float {}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Primitive {
    Str,
    Char,
    Int(Int),
    Float(Float),
    Bool,
}

prmacros::enum_str! {
    Primitive:
    "string" => Primitive::Str,
    "char" => Primitive::Char,
    "bool" => Primitive::Bool,

    "i8" => Primitive::Int(Int::I8),
    "u8" => Primitive::Int(Int::U8),
    "i16" => Primitive::Int(Int::I16),
    "u16" => Primitive::Int(Int::U16),
    "i32" => Primitive::Int(Int::I32),
    "u32" => Primitive::Int(Int::U32),
    "i64" => Primitive::Int(Int::I64),
    "u64" => Primitive::Int(Int::U64),

    "f32" => Primitive::Float(Float::F32),
    "f64" => Primitive::Float(Float::F64)
}

#[derive(Debug, Clone)]
pub struct FunType<'a> {
    pub args: Vec<Type<'a>>,
    pub ret: Weak<RefCell<Type<'a>>>,
}

#[derive(Debug, Clone)]
pub enum TypeKind<'a> {
    Tuple(Vec<Type<'a>>),
    Ptr(Box<Type<'a>>),
    Primitive(Primitive),
    Ident(Ident<'a>),
    Nullable(Box<Type<'a>>),
    Fun(FunType<'a>),
    Infer,
    Any,
    Void,
}

impl<'a> TypeKind<'a> {
    pub fn eq(&self, other: &TypeKind<'a>) -> bool {
        match (self, other) {
            // (TypeKind::Tuple(_), TypeKind::Tuple(_)) => true,
            (TypeKind::Ptr(t), TypeKind::Ptr(other)) => t.kind.eq(&other.kind),
            (TypeKind::Primitive(p), TypeKind::Primitive(other)) => p == other,
            (TypeKind::Ident(i), TypeKind::Ident(other)) => i == other,
            (TypeKind::Nullable(t), TypeKind::Nullable(other)) => t.kind.eq(&other.kind),
            // (TypeKind::Fun(_), TypeKind::Fun(_)) => true,
            //(TypeKind::Infer, TypeKind::Infer) => true,
            (TypeKind::Any, _) | (_, TypeKind::Any) => true,
            (TypeKind::Void, TypeKind::Void) => true,
            _ => false,
        }
    }
}

impl<'a> ToString for TypeKind<'a> {
    fn to_string(&self) -> String {
        use TypeKind::*;
        match self {
            Tuple(tys) =>
                "(".to_string() + &tys.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ") + ")",
            Ptr(ty) => "*".to_string() + &ty.to_string(),
            Primitive(p) => p.as_str().to_string(),
            Ident(i) => i.inner.to_string(),
            Nullable(ty) => "?".to_string() + &ty.to_string(),
            Fun(fty) =>
                "fun(".to_string() +
                &fty.args.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ") +
                ")" + " -> " + &fty.ret.upgrade().unwrap().borrow().to_string(),
            Infer => "_".to_string(),
            Any => "!".to_string(),
            Void => "void".to_string()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Type<'a> {
    pub kind: TypeKind<'a>,
    pub span: Span,
}

impl<'a> ToString for Type<'a> {
    fn to_string(&self) -> String {
        self.kind.to_string()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Deref, // *expr
    Not, // !expr, logical not
    Complement, // ~expr, binary not
    Addr, // &expr
    Neg // -expr
}

impl UnOp {
    pub fn from_token(token: TK<'_>) -> Self {
        use UnOp::*;
        match token {
            TK::Star => Deref,
            TK::Not => Not,
            TK::Tilde => Complement,
            TK::And => Addr,
            TK::Minus => Neg,
            _ => unreachable!()
        }
    }

    pub fn as_str(&self) -> &'static str {
        use UnOp::*;
        match self {
            Deref => "`*` (dereference)",
            Not => "`!` (logical not)",
            Complement => "`~` (bitwise not)",
            Addr => "`&` (address-of)",
            Neg => "`-` (negation)"
        }
    }

    pub fn get_ty<'a>(&self, ty: &Type<'a>) -> Option<TypeKind<'a>> {
        use UnOp::*;
        let kind = match (self, &ty.kind) {
            (Deref, TypeKind::Ptr(p)) => p.clone().kind,
            (Not, TypeKind::Primitive(Primitive::Bool)) => TypeKind::Primitive(Primitive::Bool),
            (Complement, TypeKind::Primitive(Primitive::Int(a))) => TypeKind::Primitive(Primitive::Int(*a)),
            (Addr, _) => TypeKind::Ptr(Box::new(ty.clone())),
            (Neg, kind @ TypeKind::Primitive(Primitive::Int(_) | Primitive::Float(_))) => kind.clone(),
            _ => return None
        };
        // Some(Type {
        //     kind,
        //     span: ty.span
        // })
        Some(kind)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Assign, // =
    Add, AddEq, // + +=
    Sub, SubEq, // - -=
    Mul, MulEq, // * *=
    Div, DivEq, // / /=
    Rem, RemEq, // % %=
    BitAnd, BitAndEq, // & &=
    BitOr, BitOrEq, // | |=
    BitXor, BitXorEq, // ^ ^=
    Shr, ShrEq, // >> >>=
    Shl, ShlEq, // << <<=
    And, Or, Eq, NEq, // && || == !=
    Ls, LsEq, // < <=
    Gr, GrEq, // > >=
}

impl BinOp {
    pub fn from_token(token: TK<'_>) -> Self {
        use BinOp::*;
        match token {
            TK::Equal => Assign,
            TK::Plus => Add,
            TK::PlusEqual => AddEq,
            TK::Minus => Sub,
            TK::MinusEqual => SubEq,
            TK::Star => Mul,
            TK::StarEqual => MulEq,
            TK::Slash => Div,
            TK::SlashEqual => DivEq,
            TK::Percent => Rem,
            TK::PercentEqual => RemEq,
            TK::And => BitAnd,
            TK::AndEqual => BitAndEq,
            TK::Bar => BitOr,
            TK::BarEqual => BitOrEq,
            TK::Caret => BitXor,
            TK::CaretEqual => BitXorEq,
            TK::GreaterGreater => Shr,
            TK::GreaterGreaterEqual => ShrEq,
            TK::LessLess => Shl,
            TK::LessLessEqual => ShlEq,
            TK::AndAnd => And,
            TK::BarBar => Or,
            TK::EqualEqual => Eq,
            TK::NotEqual => NEq,
            TK::Less => Ls,
            TK::LessEqual => LsEq,
            TK::Greater => Gr,
            TK::GreaterEqual => GrEq,
            _ => unreachable!()
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Assign => "=",
            Add => "+",
            _ => "other",
        }
    }

    pub fn get_ty<'a>(&self, ty1: &Type<'a>, ty2: &Type<'a>) -> Option<TypeKind<'a>> {
        use BinOp::*;
        match (self, &ty1.kind, &ty2.kind) {
            (Assign, a, b) if a.eq(&b) => Some(TypeKind::Void),
            (
                Add | Sub, a @ TypeKind::Ptr(_), TypeKind::Primitive(Primitive::Int(_))
            ) => Some(a.clone()),
            (
                AddEq | SubEq, a @ TypeKind::Ptr(_), TypeKind::Primitive(Primitive::Int(_))
            ) => Some(TypeKind::Void),
            (
                Add | Sub | Mul | Div | Rem,
                a @ TypeKind::Primitive(Primitive::Int(_) | Primitive::Char | Primitive::Float(_)),
                b @ TypeKind::Primitive(Primitive::Int(_) | Primitive::Char | Primitive::Float(_)),
            ) if a.eq(&b) => Some(a.clone()),
            (
                AddEq | SubEq | MulEq | DivEq | RemEq,
                a @ TypeKind::Primitive(Primitive::Int(_) | Primitive::Char | Primitive::Float(_)),
                b @ TypeKind::Primitive(Primitive::Int(_) | Primitive::Char | Primitive::Float(_)),
            ) if a.eq(&b) => Some(TypeKind::Void),
            (
                BitAnd | BitOr | BitXor | Shr | Shl,
                TypeKind::Primitive(Primitive::Int(a)), TypeKind::Primitive(Primitive::Int(_))
            ) => Some(TypeKind::Primitive(Primitive::Int(*a))),
            (
                BitAndEq | BitOrEq | BitXorEq | ShrEq | ShlEq,
                TypeKind::Primitive(Primitive::Int(_)), TypeKind::Primitive(Primitive::Int(_))
            ) => Some(TypeKind::Void),
            _ => None,
        }
        // kind.or_else(|| self.get_ty(ty2, ty1))
    }
}

#[derive(Debug, Clone)]
pub struct LetStmt<'a> {
    pub ident: Ident<'a>,
    pub ty: RefCell<Type<'a>>,
    pub init: Box<Expr<'a>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    Semi(Expr<'a>), // Expression with a semicolon
    Item(Item<'a>),
    Let(LetStmt<'a>),
    Return(Expr<'a>),
}

#[derive(Debug, Clone)]
pub struct Block<'a> {
    pub label: Option<Ident<'a>>,
    pub items: Vec<Item<'a>>,
    pub stmts: Vec<Stmt<'a>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind<'a> {
    Ident(Ident<'a>),
    Lit(Literal<'a>),
    Call { expr: Box<Expr<'a>>, args: Vec<Expr<'a>> },
    Unary { op: UnOp, expr: Box<Expr<'a>> },
    Binary { lhs: Box<Expr<'a>>, op: BinOp, rhs: Box<Expr<'a>> },
    Cast { expr: Box<Expr<'a>>, ty: Type<'a> },
    Block(Block<'a>)
}

#[derive(Debug, Clone)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub ty: RefCell<Option<TypeKind<'a>>>,
    pub span: Span,
}

impl<'a> Expr<'a> {
    pub fn new(kind: ExprKind<'a>, span: Span) -> Self {
        Expr {
            kind, span,
            ty: RefCell::new(None)
        }
    }

    pub fn new_with_type(kind: ExprKind<'a>, span: Span, ty: TypeKind<'a>) -> Self {
        Expr {
            kind, span,
            ty: RefCell::new(Some(ty))
        }
    }

    pub fn requires_semi(&self) -> bool {
        match self.kind {
            ExprKind::Block(_) => false,
            _ => true
        }
    }

    pub fn unwrap_type(&self) -> TypeKind<'a> {
        self.ty.borrow().as_ref().cloned().unwrap()
    }
}

macro_rules! keywords {
    ($($p:pat => $i:ident),+ $(,)?) => {
        $(
            #[derive(Debug, Clone, Copy)]
            pub struct $i {
                pub span: Span,
            }

            impl<'a> crate::parse::Parse<'a> for $i {
                fn parse<'i>(input: &'i [Token<'a>]) -> crate::parse::PResult<'i, 'a, Self> {
                    let t = input[0];
                    match t {
                        Token { kind: $p, span } => {
                            Ok(($i { span }, &input[1..]))
                        },
                        _ => Err(crate::Message::new("Unexpected token", t.span))
                    }
                }
            }
        )+
    }
}

keywords! {
    TK::Fun => Fun,
    TK::As => As,
    TK::Let => Let,
    TK::Semi => Semi,
    TK::Equal => Equal,
}