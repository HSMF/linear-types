use std::{borrow::Cow, collections::HashMap, fmt::Display, rc::Rc};

use anyhow::Result;

use crate::{
    ast::{self, Pat, Spanned, Type},
    ast_with_type_info::{Expression, FuncDecl, Item, Program, Statement},
    span::Span,
};

#[derive(Debug, Clone)]
pub struct TypeError {
    msg: Cow<'static, str>,
    span: Span,
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}: {}", self.span, self.msg)
    }
}

#[derive(Debug, Clone)]
struct Ctx {
    variables: HashMap<String, Rc<Type>>,
    type_variables: HashMap<String, Rc<Type>>,
}

impl TypeError {
    fn new(msg: &'static str, span: Span) -> Self {
        Self {
            msg: Cow::Borrowed(msg),
            span,
        }
    }

    fn msg(msg: String, span: Span) -> Self {
        Self {
            msg: Cow::Owned(msg),
            span,
        }
    }
}

fn int() -> Rc<Type> {
    Rc::new(Type::Var(String::from("int"), Span::empty()))
}

fn infer_type_of_expr(e: ast::Expression, ctx: &Ctx) -> Result<(Rc<Type>, Expression), TypeError> {
    match e {
        ast::Expression::Binop {
            left,
            right,
            op,
            span,
        } => {
            let (_, left) = infer_type_of_expr(*left, ctx)?;
            let (_, right) = infer_type_of_expr(*right, ctx)?;
            let left = Box::new(left);
            let right = Box::new(right);
            Ok((
                int(),
                Expression::Binop {
                    left,
                    right,
                    op,
                    span,
                    typ: int(),
                },
            ))
        }
        ast::Expression::New { .. } => todo!(),
        ast::Expression::Call { func, args, span } => {
            let fspan = func.span();
            let (tf, func) = infer_type_of_expr(*func, ctx)?;
            let (_, res, _) = tf
                .as_func()
                .ok_or(TypeError::new("expression is not a string", fspan))?;

            let args = args
                .into_iter()
                .map(|e| infer_type_of_expr(e, ctx).map(|(_, x)| x))
                .collect::<Result<_, _>>()?;

            let typ = Rc::new(res.clone());
            Ok((
                Rc::clone(&typ),
                Expression::Call {
                    func: Box::new(func),
                    args,
                    span,
                    typ,
                },
            ))
        }
        ast::Expression::Var { name, span } => {
            let typ = ctx
                .variables
                .get(&name)
                .ok_or(TypeError::msg(format!("unknown variable {name}"), span))?;
            Ok((
                Rc::clone(typ),
                Expression::Var {
                    name,
                    span,
                    typ: Rc::clone(typ),
                },
            ))
        }
        ast::Expression::Int { value, span } => Ok((
            int(),
            Expression::Int {
                value,
                span,
                typ: int(),
            },
        )),
    }
}

pub fn infer(prog: ast::Program) -> Result<Program, TypeError> {
    let type_variables: HashMap<_, _> = prog
        .0
        .iter()
        .filter_map(|x| x.type_decl())
        .map(|ast::TypeDecl { name, typ, .. }| (name.to_owned(), Rc::new(typ.to_owned())))
        .collect();

    let variables: HashMap<_, _> = prog
        .0
        .iter()
        .filter_map(|x| x.func())
        .map(
            |ast::FuncDecl {
                 name,
                 args,
                 ret_typ,
                 ..
             }| {
                let args = args.iter().map(|arg| arg.typ.clone()).collect();
                let typ = Type::Fun(args, Box::new(ret_typ.to_owned()), Span::empty());
                (name.to_owned(), Rc::new(typ))
            },
        )
        .collect();

    let ctx = Ctx {
        type_variables,
        variables,
    };

    Ok(Program(
        prog.0
            .into_iter()
            .map(|x| match x {
                ast::Item::Type(type_decl) => Ok(Item::Type(type_decl)),
                ast::Item::Func(func) => Ok(Item::Func(infer_func(func, &ctx)?)),
            })
            .collect::<Result<_, _>>()?,
    ))
}

fn infer_func(
    ast::FuncDecl {
        name,
        args,
        ret_typ,
        body,
        span,
    }: ast::FuncDecl,
    ctx: &Ctx,
) -> Result<FuncDecl, TypeError> {
    let mut ctx = ctx.to_owned();

    for arg in &args {
        ctx.variables
            .insert(arg.var.to_owned(), Rc::new(arg.typ.to_owned()));
    }

    let body = infer_block(body, &ctx)?;
    Ok(FuncDecl {
        name,
        args,
        ret_typ,
        body,
        span,
    })
}

fn infer_block(body: Vec<ast::Statement>, ctx: &Ctx) -> Result<Vec<Statement>, TypeError> {
    let mut ctx = ctx.to_owned();
    body.into_iter()
        .map(|stmt| infer_statement(stmt, &mut ctx))
        .collect()
}

fn assign_type_to_pattern(p: &Pat, t: Rc<Type>, ctx: &mut Ctx) -> Result<(), TypeError> {
    match p {
        Pat::Single(v, ..) => {
            ctx.variables.insert(v.to_owned(), t);
            Ok(())
        }
        Pat::Tuple(_, span) => Err(TypeError::new("unhandled case: tuple pattern", *span)),
    }
}

fn infer_statement(stmt: ast::Statement, ctx: &mut Ctx) -> Result<Statement, TypeError> {
    match stmt {
        ast::Statement::Let {
            pattern,
            expr,
            typ,
            span,
        } => {
            let (t, expr) = infer_type_of_expr(expr, ctx)?;
            assign_type_to_pattern(&pattern, t, ctx)?;
            Ok(Statement::Let {
                pattern,
                expr,
                typ,
                span,
            })
        }
        ast::Statement::Return { expr, span } => {
            let (typ, expr) = infer_type_of_expr(expr, ctx)?;
            Ok(Statement::Return { expr, typ, span })
        }
    }
}
