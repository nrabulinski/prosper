use crate::ast::*;
use crate::token::LitKind;
use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, FunctionType, BasicType};
use inkwell::values::{AnyValue, AnyValueEnum, BasicValueEnum, BasicValue, FunctionValue, PointerValue};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{Target, TargetMachine, CodeModel, RelocMode, FileType, InitializationConfig};
use inkwell::OptimizationLevel;
use inkwell::AddressSpace;
use inkwell::basic_block::BasicBlock;
use std::convert::TryFrom;
use std::collections::HashMap;
use std::path::Path;
use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::Rc;

struct Codegen<'ctx> {
    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

pub fn codegen(ast: AST, output: &Path) {
    let ctx = Context::create();
    let cg = Codegen {
        ctx: &ctx,
        module: ctx.create_module("program"),
        builder: ctx.create_builder()
    };
    let mut scope = Scope::new();
    ast.items.iter()
        .filter_map(|item| Some(match &item.kind {
            ItemKind::Func(f) =>
                (Cow::from(format!("_{}ABC123", f.ident.inner)),
                f.ident.inner,
                &f.args[..],
                &f.ret,
                f.to_fn_type()),
            ItemKind::FunDef(f @ FunDef { ident, args, ret, .. }) =>
                (Cow::from(ident.inner),
                ident.inner,
                &args[..],
                ret,
                f.to_fn_type()),
            _ => return None
        }))
        .for_each(|(ident, name, args, ret, kind)| {
            let args: Vec<_> = args.iter().map(|arg| arg.ty.kind.llvm_basic_type(cg.ctx)).collect();
            let ty = ret.borrow().kind.llvm_fn_type(cg.ctx, &args);
            let val = cg.module.add_function(&ident, ty, None);
            scope.fns.insert(name.to_string(), (val, kind));
        });
    eprintln!("\nBEFORE CODEGEN:");
    cg.module.print_to_stderr();
    ast.items.iter().filter_map(|item| match &item.kind {
        ItemKind::Func(f) => Some(f),
        _ => None
    }).for_each(|fun| fun.codegen(&cg, &scope));
    if scope.fns.contains_key("main") {
        gen_main(&cg, &scope);
    }
    eprintln!("\nAFTER CODEGEN:");
    cg.module.print_to_stderr();
    cg.module.verify().unwrap();
    
    // TODO: Move those to separate functions, modules etc.
    // -- NEEDS A SEPARATE FUNCTION
    let pmb = PassManagerBuilder::create();
    pmb.set_optimization_level(OptimizationLevel::Aggressive);
    let pm = PassManager::create(());
    pmb.populate_module_pass_manager(&pm);
    pm.run_on(&cg.module);
    // -- END
    eprintln!("\nAFTER OPTIMIZATIONS:");
    cg.module.print_to_stderr();
    cg.module.verify().unwrap();

    // -- NEEDS A SEPARATE FUNCTION
    let triple = TargetMachine::get_default_triple();
    Target::initialize_native(&InitializationConfig::default()).unwrap();
    let target = Target::from_triple(&triple).unwrap();
    let cpu = TargetMachine::get_host_cpu_name();
    let cpu = cpu.to_str().unwrap();
    let features = TargetMachine::get_host_cpu_features();
    let features = features.to_str().unwrap();
    let reloc = RelocMode::PIC; //Default;
    let model = CodeModel::Default;
    let machine = target.create_target_machine(
        &triple,
        cpu,
        features,
        OptimizationLevel::Aggressive,
        reloc,
        model
    ).unwrap();
    machine.write_to_file(&cg.module, FileType::Object, output).unwrap();
}

fn gen_main(cg: &Codegen, s: &Scope) {
    let fn_type = cg.ctx.i32_type().fn_type(&[], false);
    let decl = cg.module.add_function("main", fn_type, None);
    let main = s.get_fn("main");
    let start = cg.ctx.append_basic_block(decl, "start");
    cg.builder.position_at_end(start);
    let ret = cg.builder.build_call(main, &[], "status")
        .try_as_basic_value().left().unwrap_or_else(|| {
            cg.ctx.i32_type().const_int(0, false).as_basic_value_enum()
        });
    cg.builder.build_return(Some(&ret));
}

impl<'a> TypeKind<'a> {
    fn llvm_basic_type<'ctx>(&self, ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match self {
            TypeKind::Primitive(p) => match p {
                Primitive::Char => ctx.i8_type().as_basic_type_enum(),
                Primitive::Int(i) => match i {
                    Int::I8  | Int::U8  => ctx.i8_type().as_basic_type_enum(),
                    Int::I16 | Int::U16 => ctx.i16_type().as_basic_type_enum(),
                    Int::I32 | Int::U32 => ctx.i32_type().as_basic_type_enum(),
                    Int::I64 | Int::U64 => ctx.i64_type().as_basic_type_enum(),
                    Int::Generic => ctx.i32_type().as_basic_type_enum(), //unreachable!(), // we should never see generic numeric type in codegen
                },
                Primitive::Float(f) => match f {
                    Float::F32 => ctx.f32_type().as_basic_type_enum(),
                    Float::F64 => ctx.f64_type().as_basic_type_enum(),
                    Float::Generic => unreachable!(), // we should never see generic numeric type in codegen
                },
                Primitive::Bool => ctx.bool_type().as_basic_type_enum(),
                t => panic!("not implemented for {:?}", t)
            },
            TypeKind::Ptr(p) => p.kind.llvm_basic_type(ctx).ptr_type(AddressSpace::Generic).as_basic_type_enum(),
            t => panic!("not implemented for {:?}", t)
        }
    }

    fn llvm_fn_type<'ctx>(&self, ctx: &'ctx Context, args: &[BasicTypeEnum<'ctx>]) -> FunctionType<'ctx> {
        match self {
            TypeKind::Void | TypeKind::Any => ctx.void_type().fn_type(args, false),
            _ => self.llvm_basic_type(ctx).fn_type(args, false),
            t => panic!("not implemented for {:?}", t)
        }
    }
}

#[derive(Debug)]
struct Scope<'a, 'p, 'ctx> {
    parent: Option<&'p Scope<'a, 'p, 'ctx>>,
    vars: HashMap<String, (PointerValue<'ctx>, TypeKind<'a>)>,
    fns: HashMap<String, (FunctionValue<'ctx>, TypeKind<'a>)>,
}

#[derive(Debug, Copy, Clone)]
enum ValKind<'ctx> {
    L(PointerValue<'ctx>),
    R(BasicValueEnum<'ctx>),
}

#[derive(Debug, Clone)]
struct Val<'a, 'ctx> {
    kind: ValKind<'ctx>,
    ty: TypeKind<'a>,
}

impl<'a, 'ctx> Val<'a, 'ctx> {
    fn new_l(ptr: PointerValue<'ctx>, ty: TypeKind<'a>) -> Self {
        Val {
            kind: ValKind::L(ptr),
            ty
        }
    }

    fn new_r(val: BasicValueEnum<'ctx>, ty: TypeKind<'a>) -> Self {
        Val {
            kind: ValKind::R(val),
            ty
        }
    }

    // fn new_void() -> Self {
    //     Val {
    //         kind: None,
    //         ty: TypeKind::Primitive::Void,
    //     }
    // }

    fn get_val(&self, cg: &Codegen<'ctx>) -> BasicValueEnum<'ctx> {
        self.kind.get_val(cg)
    }
}

impl<'ctx> ValKind<'ctx> {
    fn get_val(&self, cg: &Codegen<'ctx>) -> BasicValueEnum<'ctx> {
        match self {
            ValKind::L(p) => cg.builder.build_load(*p, ""),
            ValKind::R(v) => *v
        }
    }
}

impl<'a, 'p, 'ctx> Scope<'a, 'p, 'ctx> {
    pub fn new() -> Self {
        Scope {
            parent: None,
            vars: HashMap::new(),
            fns: HashMap::new(),
        }
    }

    pub fn from_parent(parent: &'p Self) -> Self {
        Scope {
            parent: Some(parent),
            vars: HashMap::new(),
            fns: HashMap::new(),
        }
    }

    pub fn new_var(&mut self, name: &str, val: PointerValue<'ctx>, ty: TypeKind<'a>) { //BasicValueEnum<'ctx>) {
        self.vars.insert(name.to_string(), (val, ty));
    }

    pub fn get_var(&self, name: &str) -> Val<'a, 'ctx> { //BasicValueEnum<'ctx> {
        self.vars.get(name)
            .map(|(var, ty)| Val::new_l(*var, ty.clone()))
            .or_else(|| self.fns.get(name).map(|(f, ty)|
                Val::new_r(f.as_global_value()
                    .as_pointer_value()
                    .as_basic_value_enum(), ty.clone())
            )).or_else(|| self.parent.map(|p| p.get_var(name)))
            .expect("No such variable")
    }

    pub fn get_fn(&self, name: &str) -> FunctionValue<'ctx> {
        self.fns.get(name)
            .map(|(f, _)| *f)
            .or_else(|| self.parent.map(|p| p.get_fn(name)))
            .expect("No such function")
    }
}

impl<'a, 'ctx> Function<'a> {
    fn codegen(&self, cg: &Codegen<'ctx>, scope: &Scope<'a, '_, 'ctx>) {
        let mut scope = Scope::from_parent(scope); //Scope { parent: Some(scope), vars: HashMap::new(), fns: HashMap::new() };
        let decl = scope.get_fn(self.ident.inner);
        let start = cg.ctx.append_basic_block(decl, "start");
        cg.builder.position_at_end(start);
        decl.get_param_iter()
            .zip(self.args.iter()/*.map(|arg| arg.ident.inner)*/)
            .for_each(|(param, arg)| {
                let ptr = cg.builder.build_alloca(param.get_type(), arg.ident.inner);
                cg.builder.build_store(ptr, param);
                scope.new_var(arg.ident.inner, ptr, arg.ty.kind.clone());
            });
        match &self.body.kind {
            ExprKind::Block(b) => assert!(b.codegen(cg, &scope, None).is_none()),
            _ => {
                let ret = self.body.codegen(cg, &scope, None).map(|v| v.get_val(cg));
                cg.builder.build_return(ret.as_ref().map(|ret| ret as &dyn BasicValue));
            }
        }
        println!("Codegen function\n{:#?}\n", scope);
    }
}

impl<'a, 'ctx> Expr<'a> {
    fn codegen(&self, cg: &Codegen<'ctx>, scope: &Scope<'a, '_, 'ctx>, name: Option<&str>) -> Option<Val<'a, 'ctx>> {
        println!("CODEGEN EXPR: {:?}", self);
        let ty = self.unwrap_type();
        match &self.kind {
            ExprKind::Lit(l) => Some(Val::new_r(match &l.kind {
                LitKind::Char(i) => cg.ctx.i8_type().const_int((*i) as u64, false).as_basic_value_enum(),
                LitKind::Bool(b) => cg.ctx.bool_type().const_int(if *b { 1 } else { 0 }, false).as_basic_value_enum(),
                LitKind::Int(i) => match &ty {
                    TypeKind::Primitive(Primitive::Int(t)) => match t {
                        Int::I8  | Int::U8  =>  cg.ctx.i8_type().const_int((*i) as u64, false).as_basic_value_enum(),
                        Int::I16 | Int::U16 => cg.ctx.i16_type().const_int((*i) as u64, false).as_basic_value_enum(),
                        Int::I32 | Int::U32 => cg.ctx.i32_type().const_int((*i) as u64, false).as_basic_value_enum(),
                        Int::I64 | Int::U64 => cg.ctx.i64_type().const_int((*i) as u64, false).as_basic_value_enum(),
                        Int::Generic => {
                            eprintln!("Found generic int type in codegen?");
                            cg.ctx.i32_type().const_int((*i) as u64, false).as_basic_value_enum()
                        }
                    },
                    _ => unimplemented!()
                },
                // LitKind::Float(f) => match f {
                //     FloatLit::F64(f) => cg.ctx.f64_type().const_float(*f).as_basic_value_enum(),
                //     FloatLit::F32(f) => cg.ctx.f32_type().const_float((*f) as f64).as_basic_value_enum(),
                // }
                _ => unimplemented!()
            }, ty)),
            ExprKind::Call { expr, args } => {
                let args = args.iter()
                    .map(|arg| arg.codegen(cg, scope, None).map(|v| v.get_val(cg))
                        // .and_then(|arg| BasicValueEnum::try_from(arg).ok())
                    ).collect::<Option<Vec<_>>>()
                    .expect("illegal type");
                let f = match expr.codegen(cg, scope, None).unwrap().get_val(cg) {
                    BasicValueEnum::PointerValue(f) => f,
                    t => panic!("unexpected type in function call {:?}", t)
                };
                cg.builder.build_call(f, &args, name.unwrap_or(""))
                    .try_as_basic_value()
                    .left()
                    .map(|val| Val::new_r(val, ty))
            },
            ExprKind::Ident(i) => Some(scope.get_var(i.inner)),
            ExprKind::Unary { op, expr } => {
                let expr = expr.codegen(cg, scope, None).unwrap();
                match op {
                    UnOp::Deref => {
                        let expr = expr.get_val(cg);
                        let val = match expr {
                            BasicValueEnum::PointerValue(val) => val,
                            _ => unreachable!()
                        };
                        Some(Val::new_r(cg.builder.build_load(val, name.unwrap_or("")), self.unwrap_type()))
                    },
                    UnOp::Not => {
                        let expr = expr.get_val(cg);
                        let val = match expr {
                            BasicValueEnum::IntValue(val) => val,
                            _ => unreachable!()
                        };
                        Some(Val::new_r(cg.builder.build_not(val, name.unwrap_or("")).as_basic_value_enum(), self.unwrap_type()))
                    },
                    UnOp::Addr => {
                        match expr.kind {
                            ValKind::L(ptr) => Some(Val::new_r(ptr.as_basic_value_enum(), self.unwrap_type())),
                            ValKind::R(v) => if match v {
                                BasicValueEnum::IntValue(val) => val.is_const(),
                                BasicValueEnum::FloatValue(val) => val.is_const(),
                                _ => unimplemented!()
                            } {
                                let ty = self.unwrap_type();
                                let g = cg.module.add_global(v.get_type(), None, "");
                                g.set_constant(true);
                                g.set_initializer(&v);
                                Some(Val::new_r(g.as_pointer_value().as_basic_value_enum(), ty))
                            } else {
                                unimplemented!()
                            }
                        }
                    }
                    t => panic!("not implemented for {:?}", t)
                }
            },
            ExprKind::Binary { lhs, op, rhs } => {
                let e2 = rhs.codegen(cg, scope, None).unwrap();
                let ty = self.unwrap_type();
                match op {
                    BinOp::Assign => {
                        let ptr = match &lhs.kind {
                            ExprKind::Unary { op: UnOp::Deref, expr } =>
                                expr.codegen(cg, scope, None).unwrap().get_val(cg).into_pointer_value(),
                            _ => match lhs.codegen(cg, scope, None).unwrap().kind {
                                ValKind::L(ptr) => ptr,
                                ValKind::R(_) => unreachable!()
                            }
                        };
                        let new_val = e2.get_val(cg);
                        cg.builder.build_store(ptr, new_val);
                        return None;
                    },
                    _ => (),
                }
                let e1 = lhs.codegen(cg, scope, None).unwrap();
                match op {
                    BinOp::Add => Some(Val::new_r(gen_add(cg, e1.get_val(cg), e2.get_val(cg), name.unwrap_or("")), ty)),
                    BinOp::Sub => Some(Val::new_r(gen_sub(cg, e1.get_val(cg), e2.get_val(cg), name.unwrap_or("")), ty)),
                    BinOp::Mul => Some(Val::new_r(gen_mul(cg, e1.get_val(cg), e2.get_val(cg), name.unwrap_or("")), ty)),
                    BinOp::Div => Some(Val::new_r(gen_div(cg, &ty, e1.get_val(cg), e2.get_val(cg), name.unwrap_or("")), ty)),
                    _ => unimplemented!()
                }
            },
            ExprKind::Block(block) => {
                let current_block = cg.builder.get_insert_block().unwrap();
                let b = cg.ctx.insert_basic_block_after(current_block, "b");
                cg.builder.build_unconditional_branch(b);
                block.codegen(cg, scope, Some(b))
            },
            _ => unimplemented!()
        }
    }
}

fn gen_add<'ctx>(cg: &Codegen<'ctx>, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>, name: &str) -> BasicValueEnum<'ctx> {
    let ty = lhs.get_type();
    debug_assert_eq!(ty, rhs.get_type());
    match ty {
        BasicTypeEnum::IntType(_) => {
            let lhs = lhs.into_int_value();
            let rhs = rhs.into_int_value();
            cg.builder.build_int_add(lhs, rhs, name).as_basic_value_enum()
        },
        BasicTypeEnum::FloatType(_) => {
            let lhs = lhs.into_float_value();
            let rhs = rhs.into_float_value();
            cg.builder.build_float_add(lhs, rhs, name).as_basic_value_enum()
        }
        _ => unimplemented!(),
    }
}

fn gen_sub<'ctx>(cg: &Codegen<'ctx>, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>, name: &str) -> BasicValueEnum<'ctx> {
    let ty = lhs.get_type();
    debug_assert_eq!(ty, rhs.get_type());
    match ty {
        BasicTypeEnum::IntType(_) => {
            let lhs = lhs.into_int_value();
            let rhs = rhs.into_int_value();
            cg.builder.build_int_sub(lhs, rhs, name).as_basic_value_enum()
        },
        BasicTypeEnum::FloatType(_) => {
            let lhs = lhs.into_float_value();
            let rhs = rhs.into_float_value();
            cg.builder.build_float_sub(lhs, rhs, name).as_basic_value_enum()
        },
        _ => unimplemented!(),
    }
}

fn gen_mul<'ctx>(cg: &Codegen<'ctx>, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>, name: &str) -> BasicValueEnum<'ctx> {
    let ty = lhs.get_type();
    debug_assert_eq!(ty, rhs.get_type());
    match ty {
        BasicTypeEnum::IntType(_) => {
            let lhs = lhs.into_int_value();
            let rhs = rhs.into_int_value();
            cg.builder.build_int_mul(lhs, rhs, name).as_basic_value_enum()
        },
        _ => unimplemented!(),
    }
}

fn gen_div<'ctx>(cg: &Codegen<'ctx>, kind: &TypeKind<'_>, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>, name: &str) -> BasicValueEnum<'ctx> {
    let ty = lhs.get_type();
    debug_assert_eq!(ty, rhs.get_type());
    match ty {
        BasicTypeEnum::IntType(_) => {
            let lhs = lhs.into_int_value();
            let rhs = rhs.into_int_value();
            match kind {
                TypeKind::Primitive(p) => match p {
                    Primitive::Int(i) => match i {
                        Int::I8 | Int::I16 | Int::I32 | Int::I64 => cg.builder.build_int_signed_div(lhs, rhs, name).as_basic_value_enum(),
                        _ => cg.builder.build_int_unsigned_div(lhs, rhs, name).as_basic_value_enum(),
                    },
                    _ => unreachable!()
                },
                _ => unreachable!()
            }
        },
        _ => unimplemented!(),
    }
}

impl<'a, 'ctx> Block<'a> {
    fn codegen(&self, cg: &Codegen<'ctx>, scope: &Scope<'a, '_, 'ctx>, inner_block: Option<BasicBlock<'ctx>>) /*is_fun_body: bool)*/ -> Option<Val<'a, 'ctx>> {
        let b = if let Some(b) = inner_block { b } else { //is_fun_body {
            // return self.codegen_inner(cg, scope);
            match self.codegen_inner(cg, scope) {
                Some(val) => {
                    cg.builder.build_return(Some(&val.get_val(cg)));
                },
                _ if self.stmts.iter().all(|stmt| !matches!(stmt, Stmt::Return(_))) => {
                    if let None = cg.builder.get_insert_block().unwrap().get_terminator() {
                        cg.builder.build_return(None);
                    }
                },
                _ => ()
            }
            return None;
        };
        cg.builder.position_at_end(b);
        let ret = self.codegen_inner(cg, scope);
        if self.stmts.iter().all(|stmt| !matches!(stmt, Stmt::Return(_))) {
            let end_name = format!("{}.end", b.get_name().to_str().unwrap());
            let end = cg.ctx.insert_basic_block_after(b, &end_name);
            cg.builder.build_unconditional_branch(end);
            cg.builder.position_at_end(end);
        }
        ret
    }

    fn codegen_inner(&self, cg: &Codegen<'ctx>, scope: &Scope<'a, '_, 'ctx>) -> Option<Val<'a, 'ctx>> {
        let mut scope = Scope::from_parent(scope);
        let (last, stmts) = self.stmts.split_last()?;
        for stmt in stmts {
            match stmt {
                Stmt::Return(e) => {
                    build_return(cg, e.codegen(cg, &scope, None));
                    return None;
                },
                Stmt::Expr(e) | Stmt::Semi(e) => {
                    e.codegen(cg, &scope, None);
                },
                Stmt::Let(l) => { //l.codegen(cg, &mut scope),
                    println!("LET STATEMENT: {:?}", l);
                    let val = l.init.codegen(cg, &scope, None)?;
                    println!("VAL: {:?}", val);
                    let ty = l.ty.borrow().kind.llvm_basic_type(cg.ctx);
                    println!("TY: {:?}", ty);
                    let ptr = cg.builder.build_alloca(ty, l.ident.inner);
                    cg.builder.build_store(ptr, val.get_val(cg));
                    scope.new_var(l.ident.inner, ptr, val.ty.clone());
                },
                _ => unreachable!(),
            }
        }
        match last {
            Stmt::Expr(e) => return e.codegen(cg, &scope, None),
            Stmt::Semi(e) => {
                e.codegen(cg, &scope, None);
            },
            Stmt::Return(e) => build_return(cg, e.codegen(cg, &scope, None)),
            _ => (), // ignore last let statement lol
        }
        None
    }
}

fn build_return<'ctx>(cg: &Codegen<'ctx>, val: Option<Val<'_, 'ctx>>) {
    let val = val.map(|v| v.get_val(cg));
    cg.builder.build_return(val.as_ref().map(|val| val as &dyn BasicValue));
}

impl<'a, 'ctx> LetStmt<'a> {
    fn codegen(&self, cg: &Codegen<'ctx>, scope: &mut Scope<'a, '_, 'ctx>) {
        let val = self.init.codegen(cg, scope, None).unwrap();
        let ty = val.ty.llvm_basic_type(cg.ctx);
        let ptr = cg.builder.build_alloca(ty, self.ident.inner);
        cg.builder.build_store(ptr, val.get_val(cg));
        scope.new_var(self.ident.inner, ptr, val.ty.clone());
    }
}
