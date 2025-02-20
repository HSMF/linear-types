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
use thiserror::Error;

use crate::{
    ast::{Pat, Type},
    ast_with_type_info::{Expression, FuncDecl, Item, Program, Statement},
    builtin::builtins,
};

pub struct Codegen<'a> {
    llvm: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,

    functions: HashMap<String, Rc<FunctionValue<'a>>>,
}

#[derive(Debug, Clone)]
struct Scope<'a> {
    names: HashMap<String, Rc<BasicValueEnum<'a>>>,
    function: FunctionValue<'a>,
}

pub type Result<T> = std::result::Result<T, CompileError>;
#[derive(Debug, PartialEq, Eq, Error)]
pub enum CompileError {
    #[error("{0}")]
    BuilderError(#[from] BuilderError),
    #[error("{0}")]
    Msg(String),
}

impl From<String> for CompileError {
    fn from(value: String) -> Self {
        Self::Msg(value)
    }
}

impl From<&str> for CompileError {
    fn from(value: &str) -> Self {
        Self::Msg(value.to_owned())
    }
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

    fn declare_func<'b>(
        &mut self,
        args: impl IntoIterator<Item = &'b Type>,
        ret: &Type,
        name: &str,
    ) {
        let args: Vec<_> = args
            .into_iter()
            .map(|arg| self.compile_typ(arg).into())
            .collect();
        let ret = self.compile_typ(ret);
        let fnt = ret.fn_type(&args, false);
        let function = self.module.add_function(name, fnt, None);
        self.functions.insert(name.to_owned(), Rc::new(function));
    }

    pub fn compile(llvm: &'a Context, mod_name: &str, prog: &Program) -> Compiled<'a> {
        let mut build = Codegen::new(llvm, mod_name);

        for item in &prog.0 {
            match item {
                Item::Type(_) => {}
                Item::Func(func) => {
                    build.declare_func(func.args.iter().map(|x| &x.typ), &func.ret_typ, &func.name);
                }
            }
        }

        for builtin in builtins() {
            build.declare_func(&builtin.args, &builtin.ret_typ, builtin.name());
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
                "bool" => llvm.bool_type().into(),
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
            Type::Ref(_, _span) => llvm.ptr_type(AddressSpace::default()).into(),
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

        let mut scope = Scope {
            names: HashMap::new(),
            function,
        };

        for (i, arg) in func.args.iter().enumerate() {
            let argument = function.get_nth_param(i as _).unwrap();
            scope.names.insert(arg.var.clone(), Rc::new(argument));
        }

        for (name, fv) in self.functions.iter() {
            let fv = fv.as_global_value().as_basic_value_enum();
            scope.names.insert(name.to_owned(), Rc::new(fv));
        }

        self.compile_block(&func.body, &scope).unwrap();
    }

    fn compile_block(&self, block: &[Statement], scope: &Scope<'a>) -> Result<bool> {
        let mut scope = scope.clone();
        for stmt in block {
            if self.compile_statement(stmt, &mut scope)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn assign_to_pattern(
        &self,
        pat: &Pat,
        typ: &Type,
        e: BasicValueEnum<'a>,
        scope: &mut Scope<'a>,
    ) -> Result<()> {
        match pat {
            crate::ast::Pat::Single(v, _) => {
                scope.names.insert(v.to_owned(), Rc::new(e));
                Ok(())
            }
            crate::ast::Pat::Tuple(patterns, _) => {
                let Type::Product(typs, _) = typ else {
                    return Err("not a product type".into());
                };
                if patterns.len() != typs.len() {
                    return Err("lengths mismatch".into());
                }
                let typ = self.compile_typ_derefed(typ);
                let e = e.into_pointer_value();

                for (i, (pat, t)) in patterns.iter().zip(typs.iter()).enumerate() {
                    let index = self.builder.build_struct_gep(typ, e, i as u32, "")?;
                    let pointee = self.compile_typ(t);
                    let e = self.builder.build_load(pointee, index, "")?;
                    self.assign_to_pattern(pat, t, e, scope)?;
                }

                // is it safe to free here?
                // we could have tuples be non-duplicable types => i.e. it is safe to free here
                // (because we destructed the only tuple)
                // self.builder.build_free(e)?;

                Ok(())
            }
        }
    }

    fn compile_statement(&self, stmt: &Statement, scope: &mut Scope<'a>) -> Result<bool> {
        match stmt {
            Statement::Let {
                pattern, expr, typ, ..
            } => {
                let e = self.compile_expr(expr, scope)?;
                self.assign_to_pattern(pattern, typ, e, scope)?;
                Ok(false)
            }
            Statement::Return { expr, .. } => {
                let e = self.compile_expr(expr, scope)?;
                self.builder.build_return(Some(&e))?;
                Ok(true)
            }
            Statement::IfElse {
                cond,
                then,
                otherwise,
                ..
            } => {
                let then_block = self.llvm.append_basic_block(scope.function, "");
                let otherwise_block = self.llvm.append_basic_block(scope.function, "");
                let after_block = self.llvm.append_basic_block(scope.function, "");

                let cond = self.compile_expr(cond, scope).unwrap();
                self.builder.build_conditional_branch(
                    cond.into_int_value(),
                    then_block,
                    otherwise_block,
                )?;

                self.builder.position_at_end(then_block);
                let diverges_then = self.compile_block(then, scope)?;
                if !diverges_then {
                    self.builder.build_unconditional_branch(after_block)?;
                }

                self.builder.position_at_end(otherwise_block);
                let diverges_else = self.compile_block(otherwise, scope)?;
                if !diverges_else {
                    self.builder.build_unconditional_branch(after_block)?;
                }

                self.builder.position_at_end(after_block);

                Ok(diverges_else && diverges_then)
            }
            Statement::Mutate { lhs, rhs, .. } => {
                let dest = Rc::clone(scope.names.get(lhs).unwrap());
                let dest = dest.into_pointer_value();
                let rhs = self.compile_expr(rhs, scope)?;

                self.builder.build_store(dest, rhs)?;
                Ok(false)
            }
        }
    }

    fn compile_expr(&self, expr: &Expression, scope: &Scope<'a>) -> Result<BasicValueEnum<'a>> {
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
                    crate::ast::OpCode::Eq => self.builder.build_int_compare(
                        inkwell::IntPredicate::EQ,
                        left,
                        right,
                        "",
                    )?,
                    crate::ast::OpCode::Neq => self.builder.build_int_compare(
                        inkwell::IntPredicate::EQ,
                        left,
                        right,
                        "",
                    )?,
                };

                Ok(BasicValueEnum::IntValue(res))
            }
            Expression::New { inner, .. } => {
                let typ = self.compile_typ_derefed(&inner.typ());
                let inner = self.compile_expr(inner, scope)?;
                let loc = self.builder.build_malloc(typ, "")?;
                self.builder.build_store(loc, inner)?;

                Ok(loc.into())
            }
            Expression::Deref { inner, typ, .. } => {
                let inner = self.compile_expr(inner, scope)?;
                let typ = self.compile_typ_derefed(typ);

                let loaded = self
                    .builder
                    .build_load(typ, inner.into_pointer_value(), "")?;

                Ok(loaded)
            }
            Expression::Free { inner, typ, .. } => {
                let inner = self.compile_expr(inner, scope)?.into_pointer_value();
                let typ = self.compile_typ_derefed(typ);

                let loaded = self.builder.build_load(typ, inner, "")?;
                self.builder.build_free(inner)?;

                Ok(loaded)
            }
            Expression::Tuple { elems, typ, .. } => {
                let elems = elems
                    .iter()
                    .map(|elem| self.compile_expr(elem, scope))
                    .collect::<Result<Vec<_>>>()?;
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
                let args: Result<Vec<_>> = args
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
            Expression::Bool { value, .. } => {
                let i1_type = self.llvm.bool_type();
                let val = i1_type.const_int(*value as u64, false);
                Ok(BasicValueEnum::IntValue(val))
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
    pub fn print_to_file(&self, p: impl AsRef<Path>) -> std::result::Result<(), String> {
        self.module.print_to_file(p).map_err(|e| e.to_string())
    }

    pub fn emit(&self, p: impl AsRef<Path>) -> std::result::Result<(), String> {
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
