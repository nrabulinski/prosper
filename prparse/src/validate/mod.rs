use crate::Message;
use crate::Span;
use crate::ast::*;
use std::collections::HashSet;
use std::borrow::Cow;
use std::cell::RefCell;

mod literal;
mod expr;
mod function;

#[derive(Debug)]
pub struct Var<'a> {
    pub ident: Ident<'a>,
    pub ty: TypeKind<'a>,
}

#[derive(Debug)]
pub enum Parent<'i, 'a> {
    Vars(&'i Scope<'i, 'a>),
    NoVars(&'i Scope<'i, 'a>),
    None
}

impl<'i, 'a> Parent<'i, 'a> {
    pub fn new(parent: &'i Scope<'i, 'a>, should_capture: bool) -> Self {
        if should_capture {
            Parent::Vars(parent)
        } else {
            Parent::NoVars(parent)
        }
    }
}

pub enum FnStatus<'i, 'a> {
    Validated,
    Validating,
    Waiting(&'i Scope<'i, 'a>, &'i Item<'a>),
}

// TODO: Keep track of the scope type
// so we can give better errors and analyze the code better
#[derive(Debug)]
pub struct Scope<'i, 'a> {
    parent: Parent<'i, 'a>,
    items: &'i [Item<'a>],
    validating: RefCell<HashSet<&'a str>>,
    validated: RefCell<HashSet<&'a str>>,
    vars: Vec<Var<'a>>,
}

impl<'i, 'a> Scope<'i, 'a> {
    pub fn from_items(items: &'i [Item<'a>]) -> Result<Self, Message> {
        let mut res = Scope {
            parent: Parent::None,
            items,
            validating: RefCell::new(HashSet::new()),
            validated: RefCell::new(HashSet::new()),
            vars: Vec::new(),
        };
        res.init_vars()?;
        Ok(res)
    }

    pub fn from_parent(parent: &'i Scope<'i, 'a>) -> Self {
        let mut res = Scope {
            parent: Parent::new(parent, false),
            items: &[],
            validating: RefCell::new(HashSet::new()),
            validated: RefCell::new(HashSet::new()),
            vars: Vec::new(),
        };
        res
    }

    pub fn from_parent_with_items(
        items: &'i [Item<'a>],
        parent: &'i Scope<'i, 'a>) -> Result<Self, Message>
    {
        let mut res = Scope {
            parent: Parent::new(parent, true),
            items,
            validating: RefCell::new(HashSet::new()),
            validated: RefCell::new(HashSet::new()),
            vars: Vec::new(),
        };
        res.init_vars()?;
        Ok(res)
    }

    fn init_vars(&mut self) -> Result<(), Message> {
        for item in self.items {
            let (ident, ty, span) = match &item.kind {
                ItemKind::Func(f) => (f.ident, f.to_fn_type(), f.span),
                ItemKind::FunDef(f) => (f.ident, f.to_fn_type(), f.span),
                _ => continue,
            };
            if self.vars.iter().any(|var| var.ident == ident && matches!(var.ty, TypeKind::Fun { .. })) {
                return Err(Message::new(format!("`{}` is redefined", ident.inner), span))
            }
            let var = Var {
                ident,
                ty
            };
            self.vars.push(var);
        }
        Ok(())
    }

    pub fn validate_items(&self) -> Result<Vec<Message>, Message> {
        let mut warns = Vec::new();
        for item in self.items {
            let ident = match &item.kind {
                ItemKind::Func(f) => f.ident,
                ItemKind::FunDef(f) => f.ident,
            };
            if self.validated.borrow().contains(ident.inner) { continue; }
            self.validating.borrow_mut().insert(ident.inner);
            item.validate(self, &mut warns)?;
            debug_assert!(self.validating.borrow_mut().remove(ident.inner));
            self.validated.borrow_mut().insert(ident.inner);
        }
        Ok(warns)
    }

    pub fn push_var(&mut self, var: Var<'a>) {
        self.vars.push(var);
    }

    pub fn get_var<'s>(&'s self, ident: Ident<'a>) -> Option<&'s Var<'a>> {
        self.vars.iter().rfind(|other| other.ident == ident)
            .or_else(|| match &self.parent {
                Parent::None => None,
                Parent::NoVars(p) => p.get_fn_var(ident),
                Parent::Vars(p) => p.get_var(ident)
            })
    }

    pub fn has_var(&self, ident: Ident<'a>) -> bool {
        self.get_var(ident).is_some()
    }

    pub fn get_fn_var<'s>(&'s self, ident: Ident<'a>) -> Option<&'s Var<'a>> {
        self.vars.iter().rfind(|other| matches!(other.ty, TypeKind::Fun(_)) && other.ident == ident)
            .or_else(|| match &self.parent {
                Parent::None => None,
                Parent::NoVars(p) | Parent::Vars(p) => p.get_fn_var(ident)
            })
    }

    pub fn get_fn_status(&'i self, ident: &str) -> FnStatus<'i, 'a> {
        if self.validated.borrow().contains(ident) {
            FnStatus::Validated
        } else if self.validating.borrow().contains(ident) {
            FnStatus::Validating
        } else if let Some(f) = self.items.iter().find(|item| match &item.kind {
            ItemKind::Func(f) => f.ident.inner == ident,
            ItemKind::FunDef(f) => f.ident.inner == ident,
            _ => false,
        }) {
            FnStatus::Waiting(self, f)
        } else if let Parent::Vars(p) | Parent::NoVars(p) = self.parent {
            p.get_fn_status(ident)
        } else {
            panic!("Function {} doesn't exist yet we tried to validate it", ident);
        }
    }
}

trait Validate<'a> {
    fn validate<'i>(&self, scope: &'i Scope<'i, 'a>, warns: &mut Vec<Message>) -> Result<(Type<'a>, Vec<Type<'a>>), Message>;
}

pub fn validate_ast<'i, 'a>(items: &'i [Item<'a>]) -> Result<Vec<Message>, Message> {
    let mut scope: Scope<'i, 'a> = Scope::from_items(items)?;
    println!("{:#?}", scope);
    let res = scope.validate_items();
    println!("All global items:");
    for item in items {
        match &item.kind {
            ItemKind::Func(f) => println!("{} => {}", f.ident.inner, f.to_fn_type().to_string()),
            ItemKind::FunDef(f) => println!("(extern) {} => {}", f.ident.inner, f.to_fn_type().to_string()),
        }
    }
    let res = res?;
    if let Some(m) = items.iter().find_map(|item| match &item.kind {
        ItemKind::Func(f) if f.ident.inner == "main" => Some(f),
        _ => None
    }) {
        let res = (match &*m.ret.borrow() {
            Type { kind: TypeKind::Void | TypeKind::Primitive(Primitive::Int(Int::I32)), .. } => Ok(res),
            t => Err(Message::new("Main function can only return `void` or `i32`!", if t.span.is_empty() { m.span } else { t.span }))
        })?;
        if m.args.len() == 0 {
            Ok(res)
        } else {
            Err(Message::new("Main function can't take any arguments!", m.span))
        }
    } else {
        Err(Message::new("No main function!", Span::EMPTY))
    }
}

impl<'a> Type<'a> {
    fn validate<'i>(&self, scope: &'i Scope<'i, 'a>) -> Result<(), Message> {
        match &self.kind {
            TypeKind::Tuple(tys) => { for ty in tys {
                ty.validate(scope)?;
            } Ok(()) },
            TypeKind::Ptr(ty) => ty.validate(scope),
            TypeKind::Ident(_) => Err(Message::new("Custom types aren't supported yet!", self.span)),
            TypeKind::Nullable(_) => Err(Message::new("Nullable types aren't supported yet!", self.span)),
            TypeKind::Fun(f) => {
                println!("do we need to validate this? {:?}", f);
                Ok(())
            },
            _ => Ok(()) // rest of the types (i.e. infer, !, void and primitives) are always valid
        }
    }
}

impl<'a> Item<'a> {
    fn validate<'i>(&self, scope: &'i Scope<'i, 'a>, warns: &mut Vec<Message>) -> Result<(), Message> {
        match &self.kind {
            ItemKind::Func(f) => f.validate(scope, warns),
            ItemKind::FunDef(f) => f.validate(scope)
        }
    }
}

impl<'a> Function<'a> {
    fn validate<'i>(&self, scope: &'i Scope<'i, 'a>, warns: &mut Vec<Message>) -> Result<(), Message> {
        let ident = self.ident.inner;
        if ident.to_ascii_lowercase() != ident {
            warns.push(Message::new(
                format!("Function names should be snake_case: `{}`", helpers::to_snake_case(self.ident.inner)),
                self.ident.span
            ))
        }
        let mut scope = Scope::from_parent(scope);
        for arg in &self.args {
            arg.ty.validate(&scope)?;
            let var = Var {
                ident: arg.ident,
                ty: arg.ty.kind.clone(),
            };
            scope.push_var(var);
        }
        let (ty, r ) = match &self.body.kind {
            ExprKind::Block(b) => b.validate(&scope, warns, true)?,
            _ => self.body.validate(&scope, warns)?
        };
        for t in r {
            if !ty.kind.eq(&t.kind) {
                return Err(Message::new(format!("Type mismatch"), t.span))
            }
        }
        match &mut *self.ret.borrow_mut() {
            Type { kind: ref mut k @ TypeKind::Infer, .. } => {
                if let ExprKind::Block(_) = self.body.kind {
                    let def = format!(
                        "fun {}({}) -> {} {{ ... }}",
                        self.ident.inner,
                        self.args.iter().map(|arg|
                            format!("{}: {}", arg.ident.inner, arg.ty.to_string())
                        ).collect::<Vec<_>>().join(", "),
                        ty.to_string()
                    );
                    return Err(Message::new(format!("Don't use single expression syntax with a block body, use classic syntax instead: {}", def), self.ident.span));
                }
                *k = ty.kind.clone()
            },
            Type { kind, span } => if !kind.eq(&ty.kind) {
                return Err(Message::new(format!("Type mismatch - expected {} but body returns {}", kind.to_string(), ty.to_string()), *span));
            }
        }
        // if let TypeKind::Any = self.ret.borrow().kind {
        //     warns.push(Message::new("This function never returns", self.span));
        // }
        Ok(())
    }
}

impl<'a> FunDef<'a> {
    fn validate<'i>(&self, scope: &'i Scope<'i, 'a>) -> Result<(), Message> {
        for arg in &self.args {
            arg.ty.validate(scope)?;
        }
        self.ret.borrow().validate(scope)
    }
}

mod helpers {
    #[derive(Debug, Clone, Copy)]
    pub enum Case {
        Snake,
        ScreamingSnake,
        Other
    }

    pub fn get_case(s: &str) -> Case {
        if s.to_ascii_lowercase() == s {
            Case::Snake
        } else if s.to_ascii_uppercase() == s {
            Case::ScreamingSnake
        } else {
            Case::Other
        }
    }

    pub fn to_snake_case(s: &str) -> String {
        let len = s.len() + s.chars().filter(|c| c.is_ascii_uppercase()).count();
        s.chars().fold(String::with_capacity(len), |mut acc, ch| if ch.is_ascii_uppercase() {
            if !acc.is_empty() { acc.push('_'); } acc.push(ch.to_ascii_lowercase()); acc
        } else { acc.push(ch); acc })
    }

    pub fn expected_count(ex: usize, recv: usize, thing: &str) -> String {
        format!("Expected `{}` {}{}, got `{}`", ex, thing, if ex == 1 { "" } else { "s" }, recv)
    }
}

impl<'a> Block<'a> {
    fn validate<'i>(&self, scope: &'i Scope<'i, 'a>, warns: &mut Vec<Message>, is_fun_body: bool) -> Result<(Type<'a>, Vec<Type<'a>>), Message> {
        let mut ret = Vec::new();
        let (last, stmts) = match self.stmts.split_last() {
            Some(a) => a,
            None => {
                return Ok((
                    Type {
                        kind: TypeKind::Void,
                        span: self.span,
                    },
                    ret
                ));
            }
        };
        let items = &self.items;
        if items.len() > 0 {
            return Err(Message::new("Items in blocks are not supported yet", self.span));
        }
        let mut scope = Scope::from_parent_with_items(items, scope)?;
        warns.append(&mut scope.validate_items()?);
        for stmt in stmts {
            match stmt {
                Stmt::Semi(e) | Stmt::Expr(e) => {
                    let (_, mut r) = e.validate(&scope, warns)?;
                    ret.append(&mut r);
                },
                // TODO: REWRITE THIS
                Stmt::Let(l) => {
                    let (ty, mut r) = l.init.validate(&scope, warns)?;
                    ret.append(&mut r);
                    match &mut *l.ty.borrow_mut() {
                        Type { kind: ref mut k @ TypeKind::Infer, .. } => match &mut *l.init.ty.borrow_mut() {
                            Some(TypeKind::Primitive(Primitive::Int(ref mut t @ Int::Generic))) => {
                                *t = Int::I32;
                                *k = TypeKind::Primitive(Primitive::Int(Int::I32));
                            },
                            Some(TypeKind::Primitive(Primitive::Float(ref mut t @ Float::Generic))) => {
                                *t = Float::F64;
                                *k = TypeKind::Primitive(Primitive::Float(Float::F64));
                            }
                            Some(kind) => *k = kind.clone(),
                            _ => panic!("Tried to assign an expression of unknown type to a variable")
                        },
                        Type { kind, span } => if !kind.eq(&ty.kind) {
                            return Err(Message::new(format!("Type mismatch - expected {} but body returns {}", kind.to_string(), ty.kind.to_string()), *span));
                        },
                        Type { kind, span } => match &mut *l.init.ty.borrow_mut() {
                            Some(ref mut t @ TypeKind::Primitive(Primitive::Int(Int::Generic) | Primitive::Float(Float::Generic))) => {
                                *t = kind.clone();
                            },
                            Some(k) => if !kind.eq(k) {
                                return Err(Message::new(format!("Type mismatch - expected {} but body returns {}", kind.to_string(), ty.kind.to_string()), *span));
                            },
                            _ => panic!("Tried to assign an expression of unknown type to a variable")
                        }
                    }
                    let var = Var {
                        ident: l.ident,
                        ty: l.ty.borrow().kind.clone()
                    };
                    scope.push_var(var);
                },
                Stmt::Return(e) => {
                    let (ty, mut r) = e.validate(&scope, warns)?;
                    ret.append(&mut r);
                    ret.push(ty);
                }
                _ => unreachable!()
            }
        }
        let ty = match last {
            Stmt::Semi(Expr { span, .. }) | Stmt::Let(LetStmt { span, .. }) => Type {
                kind: TypeKind::Void,
                span: *span
            },
            Stmt::Expr(e) => {
                let (ty, mut r) = e.validate(&scope, warns)?;
                ret.append(&mut r);
                ty
            },
            Stmt::Return(e) => {
                let (ty, mut r) = e.validate(&scope, warns)?;
                ret.append(&mut r);
                if is_fun_body {
                    ty
                } else {
                    ret.push(ty);
                    Type {
                        kind: TypeKind::Void,
                        span: e.span
                    }
                }
            }
            _ => unreachable!()
        };
        println!("Validating a block.\nIs fun body? {}\nReturn types: {:?}\nType of the block: {:?}\n", is_fun_body, ret, ty);
        Ok((ty, ret))
    }
}
