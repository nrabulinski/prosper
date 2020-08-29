use crate::Message;
use crate::ast::*;
use crate::token::LitKind;
use super::Scope;
use crate::span::Span;

impl<'a> Expr<'a> {
    pub fn get_type<'i>(&self, scope: &'i Scope<'i, 'a>) -> Type<'a> {
        let kind = match &self.kind {
            ExprKind::Lit(l) => TypeKind::Primitive(match l.kind {
                LitKind::Str(_) => Primitive::Str,
                LitKind::Char(_) => Primitive::Char,
                LitKind::Int(_) => Primitive::Int,
                LitKind::Float(_) => Primitive::Float,
                LitKind::Bool(_) => Primitive::Bool
            }),
            ExprKind::Ident(i) => scope.get_var(*i).unwrap().ty.clone(),
            ExprKind::Call { expr, .. } => if let TypeKind::Fun(f) = expr.get_type(scope).kind {
                match &f.ret.upgrade().unwrap().borrow().kind {
                    TypeKind::Infer => TypeKind::Any,
                    t => t.clone()
                }
            } else {
                panic!("{:?} is not a function but ended up in a valid function call", expr)
            },
            ExprKind::Unary { op, expr } => match op {
                UnOp::Deref => match expr.get_type(scope).kind {
                    TypeKind::Ptr(t) => t.kind,
                    t => panic!("Cannot dereference {:?}", t)
                },
                UnOp::Addr => TypeKind::Ptr(Box::new(Type {
                    kind: expr.get_type(scope).kind,
                    span: Span::EMPTY
                })),
                _ => return expr.get_type(scope)
            }
            t => panic!("{:?} ain't implemented yet", t)
        };
        Type {
            kind,
            span: self.span
        }
    }

    // pub fn get_ret_type<'i>(&self, scope: &'i Scope<'i, 'a>) -> Result<TypeKind<'a>, Message> {
    //     match &self.kind {
    //         ExprKind::Block(b) => unimplemented!(),
    //         _ => Ok(self.get_type(scope))
    //     }
    // }

    // TODO:get_ret_types - returns vec of types so I can verify ret types of blocks 
}