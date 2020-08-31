use crate::ast::*;
use crate::Message;
use super::helpers;
use super::Scope;
use super::FnStatus;

impl<'a> super::Validate<'a> for Expr<'a> {
    // TODO: MAKE VALIDATE RETURN (TYPE, RET_TYPE, WARNS) INSTEAD OF JUST WARNS
    // AND IN FUNCTION::VALIDATE CHECK THE RETURN TYPE 
    // ^ kinda done
    fn validate<'i>(&self, scope: &'i Scope<'i, 'a>, warns: &mut Vec<Message>) -> Result<(Type<'a>, Vec<Type<'a>>), Message> {
        let (mut ty, ret) = match &self.kind {
            ExprKind::Ident(i) => if let Some(var) = scope.get_var(*i) {
                if matches!(var.ty, TypeKind::Fun(_)) {
                    if let FnStatus::Waiting(s, f) = scope.get_fn_status(var.ident.inner) {
                        s.validating.borrow_mut().insert(f.ident().inner);
                        f.validate(s, warns)?;
                        assert!(s.validating.borrow_mut().remove(f.ident().inner));
                        s.validated.borrow_mut().insert(f.ident().inner);
                    }
                }
                let ty = Type {
                    kind: var.ty.clone(),
                    span: self.span,
                };
                (ty, vec![])
            } else {
                return Err(Message::new(format!("`{}` is not defined", i.inner), i.span));
            },
            ExprKind::Lit(lit) => lit.validate(scope, warns)?,
            ExprKind::Call { expr, args } => {
                let (ty, mut returned) = expr.validate(scope, warns)?;
                let (a, ret) = if let TypeKind::Fun(a) = ty.kind {
                    (a.args, a.ret.upgrade().unwrap().borrow().kind.clone())
                } else {
                    return Err(Message::new(format!("Expected a function, found `{}`", ty.to_string()), expr.span));
                };
                if a.len() != args.len() {
                    return Err(Message::new(helpers::expected_count(a.len(), args.len(), "argument"), self.span));
                }
                for (arg, ex) in args.iter().zip(a.iter()) {
                    let (ty, mut r) = arg.validate(scope, warns)?;
                    returned.append(&mut r);
                    if !ex.kind.eq(&ty.kind) {
                        return Err(Message::new(format!("Type mismatch: expected `{}`, got `{}`", ex.to_string(), ty.to_string()), arg.span));
                    }
                }
                let ret = Type {
                    kind: ret,
                    span: self.span
                };
                (ret, returned)
            },
            ExprKind::Unary { op, expr } => {
                let (ty, ret) = expr.validate(scope, warns)?;
                if let Some(t) = op.get_ty(&ty) {
                    let t = Type {
                        kind: t,
                        span: self.span
                    };
                    (t, ret)
                } else {
                    return Err(Message::new(format!("Can't use {} operator with {}", op.as_str(), ty.to_string()), self.span));
                }
                // if !op.applies_to(&ty.kind) {
                //     return Err(Message::new(format!("Can't use {} operator with {}", op.as_str(), ty.to_string()), self.span));
                // }
                // (ty, ret)
            },
            ExprKind::Block(b) => b.validate(scope, warns, false)?,
            ExprKind::Binary { lhs, op, rhs } => {
                let (ty1, mut ret) = lhs.validate(scope, warns)?;
                let (ty2, mut r) = rhs.validate(scope, warns)?;
                ret.append(&mut r);
                if let Some(t) = op.get_ty(&ty1, &ty2) {
                    let t = Type {
                        kind: t,
                        span: self.span
                    };
                    (t, ret)
                } else {
                    return Err(Message::new(format!("{} is not implemented for {} and {}", op.as_str(), ty1.to_string(), ty2.to_string()), self.span))
                }
            }
            _ => unimplemented!()
        };
        // match ty.kind {
        //     TypeKind::Primitive(Primitive::Int(ref mut i @ Int::Generic)) => *i = Int::I32,
        //     TypeKind::Primitive(Primitive::Float(ref mut f @ Float::Generic)) => *f = Float::F64,
        //     _ => ()
        // }
        *self.ty.borrow_mut() = Some(ty.kind.clone());
        Ok((ty, ret))
    }
}