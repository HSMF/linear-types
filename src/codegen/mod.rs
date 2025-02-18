use std::{collections::HashMap, path::Path, rc::Rc};

use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::Module,
    targets::{InitializationConfig, Target, TargetMachine},
    types::{BasicType, BasicTypeEnum, FunctionType},
    values::{BasicValue, BasicValueEnum, FunctionValue},
    AddressSpace, OptimizationLevel,
};

use crate::{
    ast::{Pat, Type},
    ast_with_type_info::{Expression, FuncDecl, Item, Program, Statement},
};

pub struct Codegen<'a> {
    llvm: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,

    functions: HashMap<String, Rc<FunctionValue<'a>>>,
}

#[derive(Debug, Default, Clone)]
struct Scope<'a> {
    names: HashMap<String, Rc<BasicValueEnum<'a>>>,
}

impl<'a> Codegen<'a> {
    pub fn new(llvm: &'a Context, mod_name: &str) -> Self {
        let module = llvm.create_module(mod_name);
        let builder = llvm.create_builder();

        Self {
            llvm,
            module,
            builder,
            functions: HashMap::new(),
        }
    }

    pub fn compile(llvm: &'a Context, mod_name: &str, prog: &Program) -> Compiled<'a> {
        let mut build = Codegen::new(llvm, mod_name);

        for item in &prog.0 {
            match item {
                Item::Type(_) => {}
                Item::Func(func) => {
                    let args: Vec<_> = func
                        .args
                        .iter()
                        .map(|arg| build.compile_typ(&arg.typ).into())
                        .collect();
                    let ret = build.compile_typ(&func.ret_typ);
                    let fnt = ret.fn_type(&args, false);
                    let function = build.module.add_function(&func.name, fnt, None);
                    build.functions.insert(func.name.clone(), Rc::new(function));
                }
            }
        }

        for item in &prog.0 {
            match item {
                Item::Type(_) => {}
                Item::Func(func) => {
                    build.compile_fn(func);
                }
            }
        }

        Compiled::new(build.module)
    }

    fn compile_typ_bare(llvm: &'a Context, typ: &Type) -> BasicTypeEnum<'a> {
        match typ {
            Type::Var(name, _span) => match name.as_str() {
                "int" => llvm.i64_type().into(),
                _ => todo!("compile type variable {name}"),
            },
            _ => llvm.ptr_type(AddressSpace::default()).into(),
        }
    }

    fn compile_typ_derefed_bare(llvm: &'a Context, typ: &Type) -> BasicTypeEnum<'a> {
        match typ {
            Type::Var(name, _span) => match name.as_str() {
                "int" => llvm.i64_type().into(),
                _ => todo!("compile type variable {name}"),
            },
            Type::Ref(_, _span) => todo!(),
            Type::Product(items, _span) => {
                let item_types: Vec<_> = items
                    .iter()
                    .map(|x| Self::compile_typ_bare(llvm, x))
                    .collect();

                llvm.struct_type(&item_types, false).into()
            }
            Type::Fun(_args, _ret, _span) => todo!(),
        }
    }

    fn compile_typ(&self, typ: &Type) -> BasicTypeEnum<'a> {
        Self::compile_typ_bare(self.llvm, typ)
    }
    fn compile_typ_derefed(&self, typ: &Type) -> BasicTypeEnum<'a> {
        Self::compile_typ_derefed_bare(self.llvm, typ)
    }

    fn compile_func_typ<'b>(
        &self,
        args: impl IntoIterator<Item = &'b Type>,
        typ: &Type,
    ) -> FunctionType<'a> {
        let args: Vec<_> = args
            .into_iter()
            .map(|t| self.compile_typ(t).into())
            .collect();
        let ret = self.compile_typ(typ);
        ret.fn_type(&args, false)
    }

    pub fn compile_fn(&self, func: &FuncDecl) {
        let function = self
            .module
            .get_function(&func.name)
            .expect("have added all functions already");

        let basic_block = self.llvm.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        let mut scope = Scope::default();

        for (i, arg) in func.args.iter().enumerate() {
            let argument = function.get_nth_param(i as _).unwrap();
            scope.names.insert(arg.var.clone(), Rc::new(argument));
        }

        for (name, fv) in self.functions.iter() {
            let fv = fv.as_global_value().as_basic_value_enum();
            scope.names.insert(name.to_owned(), Rc::new(fv));
        }

        self.compile_block(&func.body, &scope);
    }

    fn compile_block(&self, block: &[Statement], scope: &Scope<'a>) {
        let mut scope = scope.clone();
        for stmt in block {
            self.compile_statement(stmt, &mut scope);
        }
    }

    fn assign_to_pattern(
        &self,
        pat: &Pat,
        typ: &Type,
        e: BasicValueEnum<'a>,
        scope: &mut Scope<'a>,
    ) -> Result<(), ()> {
        match pat {
            crate::ast::Pat::Single(v, _) => {
                scope.names.insert(v.to_owned(), Rc::new(e));
                Ok(())
            }
            crate::ast::Pat::Tuple(patterns, _) => {
                let Type::Product(typs, _) = typ else {
                    return Err(());
                };
                if patterns.len() != typs.len() {
                    return Err(());
                }
                let typ = self.compile_typ_derefed(typ);

                for (i, (pat, t)) in patterns.iter().zip(typs.iter()).enumerate() {
                    let index = self
                        .builder
                        .build_struct_gep(typ, e.into_pointer_value(), i as u32, "")
                        .map_err(|_| ())?;
                    let pointee = self.compile_typ(t);
                    let e = self
                        .builder
                        .build_load(pointee, index, "")
                        .map_err(|_| ())?;
                    self.assign_to_pattern(pat, t, e, scope)?;
                }

                Ok(())
            }
        }
    }

    fn compile_statement(&self, stmt: &Statement, scope: &mut Scope<'a>) {
        match stmt {
            Statement::Let {
                pattern, expr, typ, ..
            } => {
                let e = self.compile_expr(expr, scope).unwrap();
                self.assign_to_pattern(pattern, typ, e, scope).unwrap();
            }
            Statement::Return { expr, .. } => {
                let e = self.compile_expr(expr, scope).unwrap();
                self.builder.build_return(Some(&e)).unwrap();
            }
        }
    }

    fn compile_expr(
        &self,
        expr: &Expression,
        scope: &Scope<'a>,
    ) -> Result<BasicValueEnum<'a>, BuilderError> {
        match expr {
            Expression::Binop {
                left, right, op, ..
            } => {
                let left = self.compile_expr(left, scope)?.into_int_value();
                let right = self.compile_expr(right, scope)?.into_int_value();

                let res = match op {
                    crate::ast::OpCode::Add => self.builder.build_int_add(left, right, "")?,
                    crate::ast::OpCode::Sub => self.builder.build_int_sub(left, right, "")?,
                    crate::ast::OpCode::Mul => self.builder.build_int_mul(left, right, "")?,
                    crate::ast::OpCode::Div => {
                        self.builder.build_int_signed_div(left, right, "")?
                    }
                };

                Ok(BasicValueEnum::IntValue(res))
            }
            Expression::New { inner, .. } => todo!("compile {inner}"),
            Expression::Tuple { elems, typ, .. } => {
                let elems = elems
                    .iter()
                    .map(|elem| self.compile_expr(elem, scope))
                    .collect::<Result<Vec<_>, _>>()?;
                let typ = self.compile_typ_derefed(typ);
                let tuple = self.builder.build_malloc(typ, "")?;

                for (i, elem) in elems.into_iter().enumerate() {
                    let index = self.builder.build_struct_gep(typ, tuple, i as u32, "")?;
                    self.builder.build_store(index, elem)?;
                }

                Ok(tuple.into())
            }
            Expression::Call { func, args, .. } => {
                let ft = func.typ();
                let func_type = ft.as_func().expect("calling a non-function");
                let func_type = self.compile_func_typ(func_type.0, func_type.1);

                let func = self.compile_expr(func, scope)?;
                let args: Result<Vec<_>, _> = args
                    .iter()
                    .map(|arg| self.compile_expr(arg, scope).map(|x| x.into()))
                    .collect();
                let args = args?;
                let ret = self
                    .builder
                    .build_indirect_call(func_type, func.into_pointer_value(), args.as_slice(), "")?
                    .try_as_basic_value()
                    .left()
                    .expect("must return a basic value");
                Ok(ret)
            }
            Expression::Var { name, .. } => {
                let name = scope.names.get(name).unwrap();
                let cloned = Rc::clone(name);
                // let pointee = self.llvm.ptr_type(AddressSpace::default());
                // let loaded = self
                //     .builder
                //     .build_load(pointee, name.into_pointer_value(), "")?;
                Ok(*cloned)
            }
            Expression::Int { value, .. } => {
                let i64_type = self.llvm.i64_type();
                let i64_val = i64_type.const_int(*value as u64, false);
                Ok(BasicValueEnum::IntValue(i64_val))
            }
        }
    }
}

pub struct Compiled<'a> {
    module: Module<'a>,
    opt_level: OptimizationLevel,
}

impl<'a> Compiled<'a> {
    fn new(module: Module<'a>) -> Self {
        Self {
            module,
            opt_level: OptimizationLevel::None,
        }
    }

    pub fn set_opt_level(&mut self, opt_level: i32) {
        let opt_level = match opt_level {
            x if x < 0 => OptimizationLevel::Less,
            0 => OptimizationLevel::None,
            1 => OptimizationLevel::Default,
            _ => OptimizationLevel::Aggressive,
        };
        self.opt_level = opt_level;
    }

    #[allow(dead_code)]
    pub fn print_to_stderr(&self) {
        self.module.print_to_stderr();
    }

    #[allow(dead_code)]
    pub fn print_to_file(&self, p: impl AsRef<Path>) -> Result<(), String> {
        self.module.print_to_file(p).map_err(|e| e.to_string())
    }

    pub fn emit(&self, p: impl AsRef<Path>) -> Result<(), String> {
        let default_triple = TargetMachine::get_default_triple();
        Target::initialize_x86(&InitializationConfig::default());
        let target = Target::from_triple(&default_triple).expect("could create target");
        let cpu = TargetMachine::get_host_cpu_name();
        let features = "";
        let target_machine = target
            .create_target_machine(
                &default_triple,
                cpu.to_str().expect("valid utf-8"),
                features,
                self.opt_level,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .expect("could create target machine");

        self.module
            .set_data_layout(&target_machine.get_target_data().get_data_layout());
        self.module.set_triple(&default_triple);

        target_machine
            .write_to_file(&self.module, inkwell::targets::FileType::Object, p.as_ref())
            .map_err(|e| e.to_string())
    }
}
