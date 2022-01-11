use std::{collections::HashMap, path::Path};

use inkwell::{
	builder::Builder,
	context::Context,
	module::Module,
	targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
	types::{
		AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
		StringRadix,
	},
	values::{BasicValue, BasicValueEnum, CallableValue, FunctionValue, PointerValue},
	OptimizationLevel,
};

use crate::{ast::*, token::Literal};

struct Codegen<'ctx> {
	context: &'ctx Context,
	module: Module<'ctx>,
	builder: Builder<'ctx>,
	name: String,
}

struct Scope<'s, 'p> {
	parent: Option<&'p Scope<'s, 'p>>,
	funcs: HashMap<String, FunctionValue<'s>>,
	vars: HashMap<String, PointerValue<'s>>,
}

enum Val<'ctx> {
	L(PointerValue<'ctx>),
	R(BasicValueEnum<'ctx>),
}

impl<'ctx> Val<'ctx> {
	fn get_val(&self, cg: &Codegen<'ctx>) -> BasicValueEnum<'ctx> {
		match self {
			Val::L(ptr) => cg.builder.build_load(*ptr, ""),
			Val::R(val) => *val,
		}
	}
}

impl<'s, 'p> Scope<'s, 'p> {
	fn new() -> Self {
		Scope {
			parent: None,
			funcs: HashMap::new(),
			vars: HashMap::new(),
		}
	}

	fn with_items(items: &'s [Item], cg: &Codegen<'s>) -> Self {
		let mut res = Self::new();
		for item in items.iter() {
			match item {
				Item::Function(f) => {
					let fun_val = f.define(cg, &mut res);
					res.funcs.insert(f.sig.0.name.clone(), fun_val);
				}
				Item::Extern(funcs) => {
					for (def, _) in funcs {
						let fun_val = def.define(cg, &mut res);
						res.funcs.insert(def.name.clone(), fun_val);
					}
				}
				_ => unreachable!("Not a function"),
			}
		}
		res
	}

	fn with_parent(parent: &'p Scope<'s, 'p>) -> Self {
		Scope {
			parent: Some(parent),
			..Self::new()
		}
	}

	fn get_func(&self, name: &str, cg: &Codegen<'s>) -> FunctionValue<'s> {
		self.funcs
			.get(name)
			.copied()
			.or_else(|| self.parent.map(|p| p.get_func(name, cg)))
			.expect("Found uncallable function")
	}

	fn get_var(&self, name: &str, cg: &Codegen<'s>) -> Val<'s> {
		self.vars
			.get(name)
			.map(|&ptr| Val::L(ptr))
			.or_else(|| {
				self.funcs
					.get(name)
					.map(|&f| Val::R(f.as_global_value().as_pointer_value().into()))
			})
			.or_else(|| self.parent.map(|p| p.get_var(name, cg)))
			.expect("Inexistant variable")
	}
}

pub fn compile(name: String, items: Vec<Item>) {
	let context = Context::create();
	let module = context.create_module(&name);
	let builder = context.create_builder();
	let cg = Codegen {
		context: &context,
		module,
		builder,
		name,
	};
	let mut scope = Scope::with_items(&items, &cg);
	for item in items.iter() {
		item.compile(&cg, &mut scope);
	}
	gen_main(&cg, &mut scope);
	cg.module.print_to_stderr();

	let triple = TargetMachine::get_default_triple();
	Target::initialize_native(&InitializationConfig::default()).unwrap();
	let target = Target::from_triple(&triple).unwrap();

	let cpu = TargetMachine::get_host_cpu_name();
	let cpu = cpu.to_str().unwrap();

	let features = TargetMachine::get_host_cpu_features();
	let features = features.to_str().unwrap();

	let reloc = RelocMode::PIC;
	let model = CodeModel::Default;

	let machine = target
		.create_target_machine(
			&triple,
			cpu,
			features,
			OptimizationLevel::Default,
			reloc,
			model,
		)
		.unwrap();

	machine
		.write_to_file(&cg.module, FileType::Object, Path::new("target/output.o"))
		.unwrap();
}

fn gen_main<'c>(cg: &Codegen<'c>, scope: &mut Scope<'c, '_>) {
	let fn_type = cg.context.i32_type().fn_type(&[], false);
	let decl = cg.module.add_function("main", fn_type, None);
	let main = scope.get_func("main", cg);
	let start = cg.context.append_basic_block(decl, "start");
	cg.builder.position_at_end(start);
	cg.builder.build_call(main, &[], "main call");
	let ret = cg
		.context
		.i32_type()
		.const_int(0, false)
		.as_basic_value_enum();
	cg.builder.build_return(Some(&ret));
}

impl Item {
	fn compile<'c>(&self, cg: &Codegen<'c>, scope: &mut Scope<'c, '_>) {
		match self {
			Item::Extern(_) => {}
			Item::Function(fun) => {
				fun.compile(cg, scope);
			}
		}
	}
}

impl Type {
	fn to_basic_type<'ctx>(
		&self,
		cg: &Codegen<'ctx>,
		_scope: &mut Scope<'ctx, '_>,
	) -> BasicTypeEnum<'ctx> {
		match self {
			Type::Udt(_) => todo!(),
			Type::Primitive(p) => match p {
				Primitive::Int => cg.context.i32_type().as_basic_type_enum(),
				Primitive::Unit => unreachable!("Unit type in value position"),
			},
			Type::Infer => unreachable!("Inferred type in codegen"),
		}
	}

	fn to_basic_meta_type<'ctx>(
		&self,
		cg: &Codegen<'ctx>,
		scope: &mut Scope<'ctx, '_>,
	) -> BasicMetadataTypeEnum<'ctx> {
		self.to_basic_type(cg, scope).into()
	}

	fn to_any_type<'ctx>(
		&self,
		cg: &Codegen<'ctx>,
		_scope: &mut Scope<'ctx, '_>,
	) -> AnyTypeEnum<'ctx> {
		match self {
			Type::Udt(_) => todo!(),
			Type::Primitive(p) => match p {
				Primitive::Int => cg.context.i32_type().as_any_type_enum(),
				Primitive::Unit => cg.context.void_type().as_any_type_enum(),
			},
			Type::Infer => unreachable!("Inferred type in codegen"),
		}
	}

	fn to_fn_type<'ctx>(
		&self,
		params: &[BasicMetadataTypeEnum<'ctx>],
		cg: &Codegen<'ctx>,
		_scope: &mut Scope<'ctx, '_>,
	) -> FunctionType<'ctx> {
		match self {
			Type::Udt(_) => todo!(),
			Type::Primitive(p) => match p {
				Primitive::Int => cg.context.i32_type().fn_type(params, false),
				Primitive::Unit => cg.context.void_type().fn_type(params, false),
			},
			Type::Infer => unreachable!("Inferred type in codeged"),
		}
	}
}

impl FuncDef {
	fn define<'ctx>(&self, cg: &Codegen<'ctx>, scope: &mut Scope<'ctx, '_>) -> FunctionValue<'ctx> {
		let args: Vec<_> = self
			.args
			.iter()
			.map(|((_, ty), _)| ty.to_basic_meta_type(cg, scope))
			.collect();
		let fn_type = self.ret.to_fn_type(&args, cg, scope);
		// let name = format!("_{}_{}", cg.name, self.name);
		cg.module.add_function(&self.name, fn_type, None)
	}
}

impl Function {
	fn define<'ctx>(&self, cg: &Codegen<'ctx>, scope: &mut Scope<'ctx, '_>) -> FunctionValue<'ctx> {
		let args: Vec<_> = self
			.sig
			.0
			.args
			.iter()
			.map(|((_, ty), _)| ty.to_basic_meta_type(cg, scope))
			.collect();
		let fn_type = self.sig.0.ret.to_fn_type(&args, cg, scope);
		let name = format!("_{}_{}", cg.name, self.sig.0.name);
		cg.module.add_function(&name, fn_type, None)
	}

	fn compile<'ctx>(&self, cg: &Codegen<'ctx>, scope: &mut Scope<'ctx, '_>) {
		let f = scope.get_func(&self.sig.0.name, cg);

		let start = cg.context.append_basic_block(f, "start");

		cg.builder.position_at_end(start);

		let mut scope = Scope::with_parent(scope);
		self.sig
			.0
			.args
			.iter()
			.zip(f.get_param_iter())
			.for_each(|(((name, ty), _), arg)| {
				let ty = ty.to_basic_type(cg, &mut scope);
				let ptr = cg.builder.build_alloca(ty, name);
				cg.builder.build_store(ptr, arg);
				scope.vars.insert(name.clone(), ptr);
			});

		let ret_val = self
			.body
			.0
			.compile(cg, &mut scope)
			.map(|val| val.get_val(cg));
		cg.builder
			.build_return(ret_val.as_ref().map(|val| val as _));
		// todo!()
	}
}

impl Expr {
	fn compile<'ctx>(&self, cg: &Codegen<'ctx>, scope: &mut Scope<'ctx, '_>) -> Option<Val<'ctx>> {
		match self {
			Expr::Ident(s) => Some(scope.get_var(s, cg)),
			Expr::Lit(lit) => lit.compile(cg, scope),
			Expr::Block(block) => block.compile(cg, scope),
			Expr::Call { expr, args } => {
				let f: CallableValue<'ctx> = match expr.compile(cg, scope).map(|v| v.get_val(cg)) {
					Some(BasicValueEnum::PointerValue(ptr)) => {
						ptr.try_into().expect("Uncallable pointer")
					}
					_ => unreachable!("Uncallable value in call expression"),
				};
				let args: Vec<_> = args
					.iter()
					.filter_map(|(expr, _)| expr.compile(cg, scope).map(|v| v.get_val(cg).into()))
					.collect();
				cg.builder
					.build_call(f, &args, "")
					.try_as_basic_value()
					.left()
					.map(Val::R)
			}
		}
	}
}

impl Literal {
	fn compile<'ctx>(&self, cg: &Codegen<'ctx>, _scope: &mut Scope<'ctx, '_>) -> Option<Val<'ctx>> {
		match self {
			Literal::Int { val } => Some(Val::R(
				cg.context
					.i32_type()
					.const_int_from_string(val, StringRadix::Decimal)
					.unwrap()
					.as_basic_value_enum(),
			)),
			Literal::Float { val } => Some(Val::R(
				cg.context
					.f64_type()
					.const_float_from_string(val)
					.as_basic_value_enum(),
			)),
			Literal::Str { val: _ } => todo!(),
		}
	}
}

impl Block {
	fn compile<'ctx>(&self, cg: &Codegen<'ctx>, scope: &mut Scope<'ctx, '_>) -> Option<Val<'ctx>> {
		let mut scope = Scope::with_parent(scope);
		let ((last, _), stmts) = self.stmts.split_last()?;
		for (stmt, _) in stmts {
			match stmt {
				Stmt::Semi(expr) | Stmt::Expr(expr) => {
					expr.compile(cg, &mut scope);
				}
			}
		}
		match last {
			Stmt::Semi(expr) => {
				expr.compile(cg, &mut scope);
				None
			}
			Stmt::Expr(expr) => expr.compile(cg, &mut scope),
		}
	}
}
