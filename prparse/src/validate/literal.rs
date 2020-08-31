use crate::token::{Literal, LitKind};
use crate::ast::*;
use crate::Message;
use super::Scope;

impl<'a> super::Validate<'a> for Literal<'a> {
    fn validate<'i>(&self, _scope: &'i Scope<'i, 'a>, _warns: &mut Vec<Message>) -> Result<(Type<'a>, Vec<Type<'a>>), Message> {
        let kind = TypeKind::Primitive(match &self.kind {
            LitKind::Str(_) => Primitive::Str,
            LitKind::Char(_) => Primitive::Int(Int::U8),
            LitKind::Int(_) => Primitive::Int(Int::Generic),
            LitKind::Float(_) => Primitive::Float(Float::Generic),
            LitKind::Bool(_) => Primitive::Bool,
        });
        let ty = Type {
            kind,
            span: self.span
        };
        Ok((ty, vec![]))
    }
}