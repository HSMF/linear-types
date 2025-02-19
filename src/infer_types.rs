use std::{borrow::Cow, collections::HashMap, fmt::Display, rc::Rc};

pub type Result<T> = std::result::Result<T, TypeError>;

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

impl Ctx {
    fn type_doesnt_contain_var(t: &Type, v: &str) -> Option<Span> {
        match t {
            Type::Var(var, span) => (var == v).then_some(*span),
            Type::Ref(inner, ..) => Ctx::type_doesnt_contain_var(inner, v),
            Type::Product(items, ..) => {
                for item in items {
                    if let Some(x) = Ctx::type_doesnt_contain_var(item, v) {
                        return Some(x);
                    }
                }
                None
            }
            Type::Fun(items, res, ..) => {
                for item in items {
                    if let Some(x) = Ctx::type_doesnt_contain_var(item, v) {
                        return Some(x);
                    }
                }
                Ctx::type_doesnt_contain_var(res, v)
            }
        }
    }

    /// resolves type variables
    fn normalize_type(&self, t: Rc<Type>) -> Result<Rc<Type>> {
        match &*t {
            Type::Var(v, ..) if v == "int" => Ok(t),
            Type::Var(v, ..) if v == "bool" => Ok(t),
            Type::Var(v, span) => {
                let var = self
                    .type_variables
                    .get(v)
                    .ok_or_else(|| TypeError::msg(format!("{v} not found"), *span))?;

                if let Some(span) = Ctx::type_doesnt_contain_var(var, v) {
                    return Err(TypeError::msg(
                        format!("type variable {v} occurs recursively"),
                        span,
                    ));
                }

                self.normalize_type(Rc::clone(var))
            }
            Type::Ref(inner, span) => {
                let new = Type::Ref(self.normalize_type(Rc::clone(inner))?, *span);
                Ok(Rc::new(new))
            }
            Type::Product(items, span) => {
                let new = Type::Product(
                    items
                        .iter()
                        .map(|i| {
                            self.normalize_type(Rc::new(i.clone()))
                                .map(|x| (*x).clone())
                        })
                        .collect::<Result<_>>()?,
                    *span,
                );
                Ok(Rc::new(new))
            }
            Type::Fun(items, ret, span) => {
                let new = Type::Fun(
                    items
                        .iter()
                        .map(|i| {
                            self.normalize_type(Rc::new(i.clone()))
                                .map(|x| (*x).clone())
                        })
                        .collect::<Result<_>>()?,
                    self.normalize_type(Rc::clone(ret))?,
                    *span,
                );
                Ok(Rc::new(new))
            }
        }
    }
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

fn infer_type_of_expr(e: ast::Expression, ctx: &Ctx) -> Result<(Rc<Type>, Expression)> {
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
        ast::Expression::Tuple { elems, span } => {
            let elems = elems
                .into_iter()
                .map(|x| infer_type_of_expr(x, ctx))
                .collect::<Result<Vec<_>>>()?;
            let (types, elems) = elems.into_iter().map(|(t, e)| ((*t).clone(), e)).unzip();
            let typ = Rc::new(Type::Product(types, Span::empty()));
            let typ = ctx.normalize_type(typ)?;

            Ok((Rc::clone(&typ), Expression::Tuple { elems, span, typ }))
        }
        ast::Expression::Call { func, args, span } => {
            let fspan = func.span();
            let (tf, func) = infer_type_of_expr(*func, ctx)?;
            let (_, res, _) = tf
                .as_func()
                .ok_or(TypeError::new("expression is not a string", fspan))?;

            let args = args
                .into_iter()
                .map(|e| infer_type_of_expr(e, ctx).map(|(_, x)| x))
                .collect::<Result<_>>()?;

            let typ = Rc::new(res.clone());
            let typ = ctx.normalize_type(typ)?;
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
            let typ = ctx.normalize_type(Rc::clone(typ))?;
            Ok((Rc::clone(&typ), Expression::Var { name, span, typ }))
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

pub fn infer(prog: ast::Program) -> Result<Program> {
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
                let typ = Type::Fun(args, Rc::new(ret_typ.to_owned()), Span::empty());
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
            .collect::<Result<_>>()?,
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
) -> Result<FuncDecl> {
    let mut ctx = ctx.to_owned();

    for arg in &args {
        ctx.variables
            .insert(arg.var.to_owned(), Rc::new(arg.typ.to_owned()));
    }

    let args = args
        .into_iter()
        .map(|ast::Arg { var, typ, span }| -> Result<_> {
            Ok(ast::Arg {
                var,
                typ: (*(ctx.normalize_type(Rc::new(typ))?)).to_owned(),
                span,
            })
        })
        .collect::<Result<_>>()?;

    let body = infer_block(body, &ctx)?;
    Ok(FuncDecl {
        name,
        args,
        ret_typ: (*(ctx.normalize_type(Rc::new(ret_typ))?)).clone(),
        body,
        span,
    })
}

fn infer_block(body: Vec<ast::Statement>, ctx: &Ctx) -> Result<Vec<Statement>> {
    let mut ctx = ctx.to_owned();
    body.into_iter()
        .map(|stmt| infer_statement(stmt, &mut ctx))
        .collect()
}

fn assign_type_to_pattern(p: &Pat, t: Rc<Type>, ctx: &mut Ctx) -> Result<()> {
    match p {
        Pat::Single(v, ..) => {
            ctx.variables.insert(v.to_owned(), t);
            Ok(())
        }
        Pat::Tuple(patterns, span) => {
            let Type::Product(types, span) = &*t else {
                return Err(TypeError::msg(format!("{t:?} is not a tuple"), *span));
            };
            if patterns.len() != types.len() {
                return Err(TypeError::msg(
                    format!(
                        "cannot assign a {} tuple to {} patterns",
                        types.len(),
                        patterns.len()
                    ),
                    *span,
                ));
            }
            for (pattern, typ) in patterns.iter().zip(types.iter()) {
                assign_type_to_pattern(pattern, Rc::new(typ.clone()), ctx)?;
            }

            Ok(())
        }
    }
}

fn infer_statement(stmt: ast::Statement, ctx: &mut Ctx) -> Result<Statement> {
    match stmt {
        ast::Statement::Let {
            pattern,
            expr,
            typ,
            span,
        } => {
            let (t, expr) = infer_type_of_expr(expr, ctx)?;
            assign_type_to_pattern(&pattern, t, ctx)?;
            let typ = ctx.normalize_type(Rc::new(typ))?;
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
        ast::Statement::IfElse {
            cond,
            then,
            otherwise,
            span,
        } => {
            let (_, cond) = infer_type_of_expr(cond, ctx)?;
            let then = infer_block(then, ctx)?;
            let otherwise = infer_block(otherwise, ctx)?;
            Ok(Statement::IfElse {
                cond,
                then,
                otherwise,
                span,
            })
        }
    }
}
