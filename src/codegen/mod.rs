#![allow(unused_variables, unused_imports)]
use std::{collections::HashMap, rc::Rc};

use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::Module,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{
        AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue,
    },
    AddressSpace,
};

use crate::{
    ast::{Pat, Type},
    ast_with_type_info::{self, Expression, FuncDecl, Item, Program, Statement},
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

    pub fn compile(llvm: &'a Context, mod_name: &str, prog: &Program) {
        let mut build = Codegen::new(llvm, "hello_world");

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
        build.print_to_stderr();
    }

    fn compile_typ_bare(llvm: &'a Context, typ: &Type) -> BasicTypeEnum<'a> {
        match typ {
            Type::Var(name, span) => match name.as_str() {
                "int" => llvm.i64_type().into(),
                _ => todo!(),
            },
            Type::Ref(_, span) => todo!(),
            Type::Product(items, span) => todo!(),
            Type::Fun(args, ret, span) => todo!(),
        }
    }

    fn compile_typ(&self, typ: &Type) -> BasicTypeEnum<'a> {
        Self::compile_typ_bare(self.llvm, typ)
    }

    fn compile_func_typ(&self, args: &[Type], typ: &Type) -> FunctionType<'a> {
        let args: Vec<_> = args.iter().map(|t| self.compile_typ(t).into()).collect();
        let ret = self.compile_typ(typ);
        ret.fn_type(&args, false)
    }

    pub fn compile_fn(&self, func: &FuncDecl) {
        let args: Vec<_> = func
            .args
            .iter()
            .map(|arg| self.compile_typ(&arg.typ).into())
            .collect();
        let ret = //self.compile_typ(&func.ret_typ);
        self.llvm.i64_type();
        let fnt = ret.fn_type(&args, false);
        let function = self
            .module
            .get_function(&func.name)
            .expect("have added all functions already");

        let basic_block = self.llvm.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        let mut scope = Scope::default();

        for (i, arg) in func.args.iter().enumerate() {
            let argument = function.get_nth_param(i as _).unwrap();
            scope
                .names
                .insert(arg.var.clone(), Rc::new(argument.into()));
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

    fn assign_to_pattern(&self, pat: &Pat, e: BasicValueEnum<'a>, scope: &mut Scope<'a>) {
        match pat {
            crate::ast::Pat::Single(v, _) => {
                scope.names.insert(v.to_owned(), Rc::new(e));
            }
            crate::ast::Pat::Tuple(pats, span) => todo!(),
        }
    }

    fn compile_statement(&self, stmt: &Statement, scope: &mut Scope<'a>) {
        match stmt {
            Statement::Let { pattern, expr, .. } => {
                let e = self.compile_expr(expr, scope).unwrap();
                self.assign_to_pattern(pattern, e, scope);
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
            Expression::New { inner, .. } => todo!(),
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

    pub(crate) fn print_to_stderr(&self) {
        self.module.print_to_stderr();
    }
}
