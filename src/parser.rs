//! Recursive-descent parser producing a statement list and expression AST.
//!
//! The parser mirrors the classic chibicc structure: we maintain a
//! precedence-climbing set of helpers and expose a thin statement layer so
//! sequencing lives outside the expression tree. This keeps the grammar easy
//! to extend with additional statement kinds later on.

use crate::error::{CompileError, CompileResult};
use crate::tokenizer::{Token, TokenKind, describe_token, token_text};
use crate::ty::Type;
use std::collections::HashMap;

const MAX_CALL_ARGS: usize = 6;

/// Binary operators recognised by the language.
#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge,
}

/// Expression tree produced by the parser.
#[derive(Debug, Clone)]
pub enum AstNode {
  Num {
    value: i64,
    ty: Type,
  },
  Var {
    obj: usize,
    ty: Type,
  },
  Return {
    value: Box<AstNode>,
    ty: Type,
  },
  Neg {
    operand: Box<AstNode>,
    ty: Type,
  },
  Addr {
    operand: Box<AstNode>,
    ty: Type,
  },
  Deref {
    operand: Box<AstNode>,
    ty: Type,
  },
  Binary {
    op: BinaryOp,
    lhs: Box<AstNode>,
    rhs: Box<AstNode>,
    ty: Type,
  },
  Assign {
    lhs: Box<AstNode>,
    rhs: Box<AstNode>,
    ty: Type,
  },
  Call {
    name: String,
    args: Vec<AstNode>,
    ty: Type,
  },
  Block {
    body: Option<Box<Stmt>>,
    ty: Type,
  },
  If {
    cond: Box<AstNode>,
    then_branch: Box<Stmt>,
    else_branch: Option<Box<Stmt>>,
    ty: Type,
  },
  For {
    init: Option<Box<AstNode>>,
    cond: Option<Box<AstNode>>,
    inc: Option<Box<AstNode>>,
    body: Box<Stmt>,
    ty: Type,
  },
}

impl AstNode {
  pub fn number(value: i64) -> Self {
    Self::Num {
      value,
      ty: Type::int(),
    }
  }

  pub fn var(obj: usize) -> Self {
    Self::Var {
      obj,
      ty: Type::int(),
    }
  }

  pub fn unary_neg(operand: AstNode) -> Self {
    Self::Neg {
      operand: Box::new(operand),
      ty: Type::int(),
    }
  }

  pub fn addr(operand: AstNode) -> Self {
    Self::Addr {
      operand: Box::new(operand),
      ty: Type::pointer_to(Type::int()),
    }
  }

  pub fn deref(operand: AstNode) -> Self {
    Self::Deref {
      operand: Box::new(operand),
      ty: Type::int(),
    }
  }

  pub fn binary(op: BinaryOp, lhs: AstNode, rhs: AstNode) -> Self {
    Self::Binary {
      op,
      lhs: Box::new(lhs),
      rhs: Box::new(rhs),
      ty: Type::int(),
    }
  }

  pub fn assign(lhs: AstNode, rhs: AstNode) -> Self {
    Self::Assign {
      lhs: Box::new(lhs),
      rhs: Box::new(rhs),
      ty: Type::int(),
    }
  }

  pub fn call(name: impl Into<String>, args: Vec<AstNode>) -> Self {
    Self::Call {
      name: name.into(),
      args,
      ty: Type::int(),
    }
  }

  pub fn ret(value: AstNode) -> Self {
    Self::Return {
      value: Box::new(value),
      ty: Type::int(),
    }
  }

  pub fn block(body: Option<Box<Stmt>>) -> Self {
    Self::Block {
      body,
      ty: Type::int(),
    }
  }

  pub fn if_stmt(cond: AstNode, then_branch: Box<Stmt>, else_branch: Option<Box<Stmt>>) -> Self {
    Self::If {
      cond: Box::new(cond),
      then_branch,
      else_branch,
      ty: Type::int(),
    }
  }

  pub fn for_stmt(
    init: Option<AstNode>,
    cond: Option<AstNode>,
    inc: Option<AstNode>,
    body: Box<Stmt>,
  ) -> Self {
    Self::For {
      init: init.map(Box::new),
      cond: cond.map(Box::new),
      inc: inc.map(Box::new),
      body,
      ty: Type::int(),
    }
  }

  pub fn while_stmt(cond: AstNode, body: Box<Stmt>) -> Self {
    Self::for_stmt(None, Some(cond), None, body)
  }

  pub fn ty(&self) -> &Type {
    match self {
      AstNode::Num { ty, .. }
      | AstNode::Var { ty, .. }
      | AstNode::Return { ty, .. }
      | AstNode::Neg { ty, .. }
      | AstNode::Addr { ty, .. }
      | AstNode::Deref { ty, .. }
      | AstNode::Binary { ty, .. }
      | AstNode::Assign { ty, .. }
      | AstNode::Call { ty, .. }
      | AstNode::Block { ty, .. }
      | AstNode::If { ty, .. }
      | AstNode::For { ty, .. } => ty,
    }
  }

  pub fn ty_mut(&mut self) -> &mut Type {
    match self {
      AstNode::Num { ty, .. }
      | AstNode::Var { ty, .. }
      | AstNode::Return { ty, .. }
      | AstNode::Neg { ty, .. }
      | AstNode::Addr { ty, .. }
      | AstNode::Deref { ty, .. }
      | AstNode::Binary { ty, .. }
      | AstNode::Assign { ty, .. }
      | AstNode::Call { ty, .. }
      | AstNode::Block { ty, .. }
      | AstNode::If { ty, .. }
      | AstNode::For { ty, .. } => ty,
    }
  }
}

/// Singly-linked list of statements. Each node holds exactly one expression
/// statement for now, but the structure leaves room to grow.
#[derive(Debug, Clone)]
pub struct Stmt {
  pub expr: AstNode,
  pub next: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct Obj {
  pub name: String,
  pub offset: i64,
  pub ty: Type,
}

impl Obj {
  pub fn new(name: impl Into<String>, ty: Type) -> Self {
    Self {
      name: name.into(),
      offset: 0,
      ty,
    }
  }
}

#[derive(Debug, Clone)]
pub struct Function {
  pub name: String,
  pub return_ty: Type,
  pub ty: Type,
  pub body: Option<Box<Stmt>>,
  pub params: Vec<usize>,
  pub locals: Vec<Obj>,
  pub stack_size: i64,
}

#[derive(Debug, Clone)]
pub struct Program {
  pub functions: Vec<Function>,
}

impl Stmt {
  /// Iterate statements in order. Useful for debugging and future passes.
  pub fn iter(&self) -> StmtIter<'_> {
    StmtIter {
      current: Some(self),
    }
  }
}

pub struct StmtIter<'a> {
  current: Option<&'a Stmt>,
}

impl<'a> Iterator for StmtIter<'a> {
  type Item = &'a AstNode;

  fn next(&mut self) -> Option<Self::Item> {
    let stmt = self.current?;
    self.current = stmt.next.as_deref();
    Some(&stmt.expr)
  }
}

/// Parse a sequence of statements from the token stream.
// Parse procedure overview:
// - `parse` initialises the token stream/context and hands off to `parse_function`.
// - `parse_function` consumes a function definition (`int` return type only for now).
// - Function bodies reuse `parse_stmt_sequence`, expressions still descend via the usual precedence ladder.
pub fn parse(tokens: Vec<Token>, source: &str) -> CompileResult<Program> {
  let mut stream = TokenStream::new(tokens, source);

  if stream.is_eof() {
    return Err(CompileError::at(source, 0, "program is empty"));
  }

  let mut functions = Vec::new();
  while !stream.is_eof() {
    functions.push(parse_function(&mut stream)?);
  }

  Ok(Program { functions })
}

fn parse_function(stream: &mut TokenStream) -> CompileResult<Function> {
  let return_ty = parse_declspec(stream)?;
  let (name, name_loc) = stream.get_ident()?;
  validate_ident(&name, stream.source, name_loc)?;

  let mut ctx = ParserContext::new();
  stream.skip("(")?;
  let mut params = Vec::new();
  if !stream.peek_is(")") {
    loop {
      let param_base = parse_declspec(stream)?;
      let param_index = parse_declarator(stream, &mut ctx, &param_base)?;
      ctx.promote_array_param(param_index);
      params.push(param_index);
      if params.len() > MAX_CALL_ARGS {
        return Err(CompileError::at(
          stream.source,
          stream.current_loc(),
          format!("functions support at most {MAX_CALL_ARGS} parameters"),
        ));
      }
      if !stream.equal(",") {
        break;
      }
    }
  }
  stream.skip(")")?;
  let mut body = parse_block_body(stream, &mut ctx)?;

  if let Some(stmt) = body.as_deref_mut() {
    ctx.annotate_stmt_list(Some(stmt));
  }

  let stack_size = ctx.assign_offsets();
  let param_indices = params;
  let locals = ctx.into_locals();
  let fn_ty = Type::func(return_ty.clone());

  Ok(Function {
    name,
    return_ty,
    ty: fn_ty,
    body,
    params: param_indices,
    locals,
    stack_size,
  })
}

// ----- Statement parsing -----
fn parse_stmt_sequence(
  stream: &mut TokenStream,
  ctx: &mut ParserContext,
  terminator: Option<&str>,
) -> CompileResult<Option<Box<Stmt>>> {
  let mut head: Option<Box<Stmt>> = None;
  let mut tail = &mut head;

  loop {
    if let Some(term) = terminator {
      if stream.peek_is(term) {
        stream.skip(term)?;
        break;
      }
      if stream.is_eof() {
        return Err(CompileError::at(
          stream.source,
          stream.source.len(),
          format!("expected '{term}'"),
        ));
      }
    } else if stream.is_eof() {
      break;
    }

    if stream.is_eof() {
      break;
    }

    if matches!(stream.peek_keyword(), Some("int")) {
      tail = parse_declaration_into(stream, ctx, tail)?;
      continue;
    }

    let stmt = parse_stmt(stream, ctx)?;
    *tail = Some(stmt);
    tail = &mut tail.as_mut().unwrap().next;
  }

  Ok(head)
}

fn parse_block_body(
  stream: &mut TokenStream,
  ctx: &mut ParserContext,
) -> CompileResult<Option<Box<Stmt>>> {
  stream.skip("{")?;
  parse_stmt_sequence(stream, ctx, Some("}"))
}

struct ParserContext {
  locals: Vec<Obj>,
  map: HashMap<String, usize>,
}

impl ParserContext {
  fn new() -> Self {
    Self {
      locals: Vec::new(),
      map: HashMap::new(),
    }
  }

  fn declare_local(
    &mut self,
    name: &str,
    ty: Type,
    source: &str,
    loc: usize,
  ) -> CompileResult<usize> {
    if self.map.contains_key(name) {
      return Err(CompileError::at(
        source,
        loc,
        format!("redeclaration of '{name}'"),
      ));
    }
    let index = self.locals.len();
    self.locals.push(Obj::new(name, ty));
    self.map.insert(name.to_string(), index);
    Ok(index)
  }

  fn promote_array_param(&mut self, index: usize) {
    if let Some(obj) = self.locals.get_mut(index)
      && obj.ty.is_array()
    {
      let decl = obj.ty.decl_token;
      let elem = obj.ty.base().cloned().unwrap_or_else(Type::int);
      let mut ptr = Type::pointer_to(elem);
      if let Some(token) = decl {
        ptr = ptr.with_decl_token(token);
      }
      obj.ty = ptr;
    }
  }

  fn lookup_local(&self, name: &str) -> Option<usize> {
    self.map.get(name).copied()
  }

  fn assign_offsets(&mut self) -> i64 {
    let mut offset: i64 = 0;
    for obj in self.locals.iter_mut().rev() {
      offset += obj.ty.size();
      obj.offset = offset;
    }
    align_to(offset, 16)
  }

  fn annotate_type(&self, node: &mut AstNode) {
    match node {
      AstNode::Num { ty, .. } => {
        *ty = Type::int();
      }
      AstNode::Var { obj, ty, .. } => {
        let t = self
          .locals
          .get(*obj)
          .map(|o| o.ty.clone())
          .unwrap_or_else(Type::int);
        *ty = t.decay();
      }
      AstNode::Neg { operand, ty } => {
        self.annotate_type(operand);
        *ty = Type::int();
      }
      AstNode::Addr { operand, ty } => {
        self.annotate_type(operand);
        let result_ty = match operand.as_ref() {
          AstNode::Var { obj, .. } => self
            .locals
            .get(*obj)
            .map(|o| Type::pointer_to(o.ty.clone()))
            .unwrap_or_else(|| Type::pointer_to(Type::int())),
          _ => Type::pointer_to(operand.ty().clone()),
        };
        *ty = result_ty;
      }
      AstNode::Deref { operand, ty } => {
        self.annotate_type(operand);
        *ty = operand.ty().base().cloned().unwrap_or_else(Type::int);
      }
      AstNode::Binary { op, lhs, rhs, ty } => {
        self.annotate_type(lhs);
        self.annotate_type(rhs);
        *ty = match op {
          BinaryOp::Add => {
            if lhs.ty().is_pointer_like() && rhs.ty().is_integer() {
              lhs.ty().decay()
            } else if lhs.ty().is_integer() && rhs.ty().is_pointer_like() {
              rhs.ty().decay()
            } else {
              Type::int()
            }
          }
          BinaryOp::Sub => {
            if lhs.ty().is_pointer_like() && rhs.ty().is_integer() {
              lhs.ty().decay()
            } else {
              Type::int()
            }
          }
          BinaryOp::Mul | BinaryOp::Div => Type::int(),
          _ => Type::int(),
        };
      }
      AstNode::Assign { lhs, rhs, ty } => {
        self.annotate_type(lhs);
        self.annotate_type(rhs);
        *ty = if lhs.ty().is_pointer_like() {
          lhs.ty().decay()
        } else {
          rhs.ty().clone()
        };
      }
      AstNode::Call { args, ty, .. } => {
        for arg in args.iter_mut() {
          self.annotate_type(arg);
        }
        *ty = Type::int();
      }
      AstNode::Block { body, ty } => {
        *ty = self.annotate_stmt_list(body.as_deref_mut());
      }
      AstNode::Return { value, ty } => {
        self.annotate_type(value);
        *ty = value.ty().clone();
      }
      AstNode::If {
        cond,
        then_branch,
        else_branch,
        ty,
      } => {
        self.annotate_type(cond);
        self.annotate_stmt_list(Some(then_branch.as_mut()));
        if let Some(else_branch) = else_branch.as_mut() {
          self.annotate_stmt_list(Some(else_branch.as_mut()));
        }
        *ty = Type::int();
      }
      AstNode::For {
        init,
        cond,
        inc,
        body,
        ty,
      } => {
        if let Some(init) = init.as_mut() {
          self.annotate_type(init);
        }
        if let Some(cond) = cond.as_mut() {
          self.annotate_type(cond);
        }
        if let Some(inc) = inc.as_mut() {
          self.annotate_type(inc);
        }
        self.annotate_stmt_list(Some(body.as_mut()));
        *ty = Type::int();
      }
    }
  }

  fn annotate_stmt_list(&self, mut stmt: Option<&mut Stmt>) -> Type {
    let mut ty = Type::int();
    while let Some(s) = stmt {
      self.annotate_type(&mut s.expr);
      ty = s.expr.ty().clone();
      stmt = s.next.as_deref_mut();
    }
    ty
  }

  fn into_locals(self) -> Vec<Obj> {
    self.locals
  }
}

fn align_to(n: i64, align: i64) -> i64 {
  if align == 0 {
    return n;
  }
  ((n + align - 1) / align) * align
}

const POINTER_SIZE: i64 = 8;

fn parse_stmt(stream: &mut TokenStream, ctx: &mut ParserContext) -> CompileResult<Box<Stmt>> {
  match stream.peek_keyword() {
    Some("if") => parse_if_stmt(stream, ctx),
    Some("for") => parse_for_stmt(stream, ctx),
    Some("while") => parse_while_stmt(stream, ctx),
    Some("return") => parse_return_stmt(stream, ctx),
    Some("else") => Err(CompileError::at(
      stream.source,
      stream.current_loc(),
      "unexpected 'else' without a matching 'if'",
    )),
    _ if stream.peek_is("{") => {
      let body = parse_block_body(stream, ctx)?;
      if stream.peek_is(";") {
        stream.skip(";")?;
      }
      Ok(Box::new(Stmt {
        expr: AstNode::block(body),
        next: None,
      }))
    }
    _ => parse_expr_stmt(stream, ctx),
  }
}

fn parse_expr_stmt(stream: &mut TokenStream, ctx: &mut ParserContext) -> CompileResult<Box<Stmt>> {
  if stream.equal(";") {
    let mut expr = AstNode::block(None);
    ctx.annotate_type(&mut expr);
    return Ok(Box::new(Stmt { expr, next: None }));
  }
  let expr = parse_expr(stream, ctx)?;
  stream.skip(";")?;

  Ok(Box::new(Stmt { expr, next: None }))
}

fn parse_return_stmt(
  stream: &mut TokenStream,
  ctx: &mut ParserContext,
) -> CompileResult<Box<Stmt>> {
  stream.skip("return")?;
  let expr = parse_expr(stream, ctx)?;
  stream.skip(";")?;
  Ok(Box::new(Stmt {
    expr: AstNode::ret(expr),
    next: None,
  }))
}

fn parse_if_stmt(stream: &mut TokenStream, ctx: &mut ParserContext) -> CompileResult<Box<Stmt>> {
  stream.skip("if")?;
  stream.skip("(")?;
  let cond = parse_expr(stream, ctx)?;
  stream.skip(")")?;
  let then_branch = parse_stmt(stream, ctx)?;
  let else_branch = if matches!(
    stream
      .peek()
      .filter(|token| token.kind == TokenKind::Keyword)
      .map(|token| token_text(token, stream.source)),
    Some("else")
  ) {
    stream.skip("else")?;
    Some(parse_stmt(stream, ctx)?)
  } else {
    None
  };

  Ok(Box::new(Stmt {
    expr: AstNode::if_stmt(cond, then_branch, else_branch),
    next: None,
  }))
}

fn parse_for_stmt(stream: &mut TokenStream, ctx: &mut ParserContext) -> CompileResult<Box<Stmt>> {
  stream.skip("for")?;
  stream.skip("(")?;

  let init = if stream.equal(";") {
    None
  } else {
    let init_expr = parse_expr(stream, ctx)?;
    stream.skip(";")?;
    Some(init_expr)
  };

  let cond = if stream.equal(";") {
    None
  } else {
    let cond_expr = parse_expr(stream, ctx)?;
    stream.skip(";")?;
    Some(cond_expr)
  };

  let inc = if stream.equal(")") {
    None
  } else {
    let inc_expr = parse_expr(stream, ctx)?;
    stream.skip(")")?;
    Some(inc_expr)
  };

  let body = parse_stmt(stream, ctx)?;

  Ok(Box::new(Stmt {
    expr: AstNode::for_stmt(init, cond, inc, body),
    next: None,
  }))
}

fn parse_while_stmt(stream: &mut TokenStream, ctx: &mut ParserContext) -> CompileResult<Box<Stmt>> {
  stream.skip("while")?;
  stream.skip("(")?;
  let cond = parse_expr(stream, ctx)?;
  stream.skip(")")?;
  let body = parse_stmt(stream, ctx)?;

  Ok(Box::new(Stmt {
    expr: AstNode::while_stmt(cond, body),
    next: None,
  }))
}

// ----- Declaration helpers -----
fn parse_declspec(stream: &mut TokenStream) -> CompileResult<Type> {
  if matches!(stream.peek_keyword(), Some("int")) {
    stream.skip("int")?;
    Ok(Type::int())
  } else {
    let loc = stream.current_loc();
    Err(CompileError::at(
      stream.source,
      loc,
      "expected type specifier",
    ))
  }
}

fn parse_declarator(
  stream: &mut TokenStream,
  ctx: &mut ParserContext,
  base_ty: &Type,
) -> CompileResult<usize> {
  let mut ty = base_ty.clone();
  while stream.equal("*") {
    ty = Type::pointer_to(ty);
  }
  let (name, loc) = stream.get_ident()?;
  validate_ident(&name, stream.source, loc)?;
  while stream.equal("[") {
    let (len, len_loc) = stream.get_number()?;
    if len <= 0 {
      return Err(CompileError::at(
        stream.source,
        len_loc,
        "array length must be positive",
      ));
    }
    stream.skip("]")?;
    ty = Type::array_of(ty, len as usize);
  }
  ctx.declare_local(&name, ty, stream.source, loc)
}

fn parse_declaration_into<'a>(
  stream: &mut TokenStream,
  ctx: &mut ParserContext,
  mut tail: &'a mut Option<Box<Stmt>>,
) -> CompileResult<&'a mut Option<Box<Stmt>>> {
  let base_ty = parse_declspec(stream)?;

  loop {
    let index = parse_declarator(stream, ctx, &base_ty)?;
    if stream.peek_is("=") {
      if ctx.locals[index].ty.is_array() {
        let loc = stream.current_loc();
        return Err(CompileError::at(
          stream.source,
          loc,
          "array initialisers are not supported",
        ));
      }
      stream.skip("=")?;
      let init = parse_expr(stream, ctx)?;
      let mut assign = AstNode::assign(AstNode::var(index), init);
      ctx.annotate_type(&mut assign);
      let stmt = Box::new(Stmt {
        expr: assign,
        next: None,
      });
      *tail = Some(stmt);
      tail = &mut tail.as_mut().unwrap().next;
    }

    if stream.equal(",") {
      continue;
    }

    break;
  }

  stream.skip(";")?;
  Ok(tail)
}

// ----- Expression parsing -----
fn parse_expr(stream: &mut TokenStream, ctx: &mut ParserContext) -> CompileResult<AstNode> {
  parse_assign(stream, ctx)
}

fn parse_assign(stream: &mut TokenStream, ctx: &mut ParserContext) -> CompileResult<AstNode> {
  let node = parse_equality(stream, ctx)?;

  if matches!(
    stream
      .peek()
      .filter(|token| token.kind == TokenKind::Punctuator)
      .map(|token| token_text(token, stream.source)),
    Some("=")
  ) {
    let assign_loc = stream.current_loc();
    stream.skip("=")?;
    let mut rhs = parse_assign(stream, ctx)?;
    ctx.annotate_type(&mut rhs);
    return match node {
      AstNode::Var { .. } => Ok(AstNode::assign(node, rhs)),
      AstNode::Deref { .. } => Ok(AstNode::assign(node, rhs)),
      _ => Err(CompileError::at(
        stream.source,
        assign_loc,
        "left-hand side of assignment is not assignable",
      )),
    };
  }

  Ok(node)
}

fn parse_equality(stream: &mut TokenStream, ctx: &mut ParserContext) -> CompileResult<AstNode> {
  let mut node = parse_relational(stream, ctx)?;

  loop {
    let op_str = match stream
      .peek()
      .filter(|token| token.kind == TokenKind::Punctuator)
      .map(|token| token_text(token, stream.source))
    {
      Some(symbol @ "==") => symbol,
      Some(symbol @ "!=") => symbol,
      _ => break,
    };

    let op = match op_str {
      "==" => BinaryOp::Eq,
      "!=" => BinaryOp::Ne,
      _ => unreachable!(),
    };

    stream.skip(op_str)?;
    let rhs = parse_relational(stream, ctx)?;
    node = AstNode::binary(op, node, rhs);
  }

  Ok(node)
}

fn parse_relational(stream: &mut TokenStream, ctx: &mut ParserContext) -> CompileResult<AstNode> {
  let mut node = parse_add(stream, ctx)?;

  loop {
    let op_str = match stream
      .peek()
      .filter(|token| token.kind == TokenKind::Punctuator)
      .map(|token| token_text(token, stream.source))
    {
      Some(symbol @ "<") => symbol,
      Some(symbol @ "<=") => symbol,
      Some(symbol @ ">") => symbol,
      Some(symbol @ ">=") => symbol,
      _ => break,
    };

    let op = match op_str {
      "<" => BinaryOp::Lt,
      "<=" => BinaryOp::Le,
      ">" => BinaryOp::Gt,
      ">=" => BinaryOp::Ge,
      _ => unreachable!(),
    };

    stream.skip(op_str)?;
    let rhs = parse_add(stream, ctx)?;
    node = AstNode::binary(op, node, rhs);
  }

  Ok(node)
}

fn parse_add(stream: &mut TokenStream, ctx: &mut ParserContext) -> CompileResult<AstNode> {
  let mut node = parse_mul(stream, ctx)?;

  loop {
    let op_str = match stream
      .peek()
      .filter(|token| token.kind == TokenKind::Punctuator)
      .map(|token| token_text(token, stream.source))
    {
      Some(symbol @ "+") => symbol,
      Some(symbol @ "-") => symbol,
      _ => break,
    };

    let op_loc = stream.current_loc();
    stream.skip(op_str)?;
    let rhs = parse_mul(stream, ctx)?;
    node = match op_str {
      "+" => build_add(ctx, node, rhs, stream.source, op_loc)?,
      "-" => build_sub(ctx, node, rhs, stream.source, op_loc)?,
      _ => unreachable!(),
    };
  }

  Ok(node)
}

fn parse_mul(stream: &mut TokenStream, ctx: &mut ParserContext) -> CompileResult<AstNode> {
  let mut node = parse_unary(stream, ctx)?;

  loop {
    let op_str = match stream
      .peek()
      .filter(|token| token.kind == TokenKind::Punctuator)
      .map(|token| token_text(token, stream.source))
    {
      Some(symbol @ "*") => symbol,
      Some(symbol @ "/") => symbol,
      _ => break,
    };

    let op = match op_str {
      "*" => BinaryOp::Mul,
      "/" => BinaryOp::Div,
      _ => unreachable!(),
    };

    stream.skip(op_str)?;
    let rhs = parse_unary(stream, ctx)?;
    node = AstNode::binary(op, node, rhs);
  }

  Ok(node)
}

fn parse_unary(stream: &mut TokenStream, ctx: &mut ParserContext) -> CompileResult<AstNode> {
  if stream.equal("+") {
    let operand = parse_unary(stream, ctx)?;
    return Ok(operand);
  }

  if stream.equal("-") {
    let operand = parse_unary(stream, ctx)?;
    return Ok(AstNode::unary_neg(operand));
  }

  if stream.equal("&") {
    let operand = parse_unary(stream, ctx)?;
    return Ok(AstNode::addr(operand));
  }

  if stream.equal("*") {
    let operand = parse_unary(stream, ctx)?;
    return Ok(AstNode::deref(operand));
  }

  parse_primary(stream, ctx)
}

fn parse_primary(stream: &mut TokenStream, ctx: &mut ParserContext) -> CompileResult<AstNode> {
  if stream.equal("(") {
    let node = parse_expr(stream, ctx)?;
    stream.skip(")")?;
    Ok(node)
  } else if stream.peek_is("{") {
    let body = parse_block_body(stream, ctx)?;
    Ok(AstNode::block(body))
  } else if matches!(
    stream.peek().map(|token| token.kind),
    Some(TokenKind::Ident)
  ) {
    let (name, loc) = stream.get_ident()?;
    validate_ident(&name, stream.source, loc)?;
    if stream.peek_is("(") {
      stream.skip("(")?;
      let mut args = Vec::new();
      if !stream.peek_is(")") {
        loop {
          let arg = parse_assign(stream, ctx)?;
          args.push(arg);
          if stream.equal(",") {
            continue;
          }
          break;
        }
      }
      stream.skip(")")?;
      if args.len() > MAX_CALL_ARGS {
        return Err(CompileError::at(
          stream.source,
          loc,
          format!("function calls support at most {MAX_CALL_ARGS} arguments"),
        ));
      }
      let mut call = AstNode::call(name, args);
      ctx.annotate_type(&mut call);
      return Ok(call);
    }
    let index = ctx.lookup_local(&name).ok_or_else(|| {
      CompileError::at(
        stream.source,
        loc,
        format!("use of undeclared identifier '{name}'"),
      )
    })?;
    Ok(AstNode::var(index))
  } else {
    let (value, _) = stream.get_number()?;
    Ok(AstNode::number(value))
  }
}

fn build_add(
  ctx: &ParserContext,
  mut lhs: AstNode,
  mut rhs: AstNode,
  source: &str,
  loc: usize,
) -> CompileResult<AstNode> {
  ctx.annotate_type(&mut lhs);
  ctx.annotate_type(&mut rhs);
  let lhs_ty = lhs.ty().clone();
  let rhs_ty = rhs.ty().clone();

  let mut result = if lhs_ty.is_integer() && rhs_ty.is_integer() {
    AstNode::binary(BinaryOp::Add, lhs, rhs)
  } else if lhs_ty.is_pointer_like() && rhs_ty.is_integer() {
    let scale = lhs_ty.base().map(|t| t.size()).unwrap_or(POINTER_SIZE);
    let mut scaled = AstNode::binary(BinaryOp::Mul, rhs, AstNode::number(scale));
    ctx.annotate_type(&mut scaled);
    AstNode::binary(BinaryOp::Add, lhs, scaled)
  } else if lhs_ty.is_integer() && rhs_ty.is_pointer_like() {
    let scale = rhs_ty.base().map(|t| t.size()).unwrap_or(POINTER_SIZE);
    let mut scaled = AstNode::binary(BinaryOp::Mul, lhs, AstNode::number(scale));
    ctx.annotate_type(&mut scaled);
    AstNode::binary(BinaryOp::Add, rhs, scaled)
  } else {
    return Err(CompileError::at(
      source,
      loc,
      "invalid operands to binary '+'",
    ));
  };

  ctx.annotate_type(&mut result);
  Ok(result)
}

fn build_sub(
  ctx: &ParserContext,
  mut lhs: AstNode,
  mut rhs: AstNode,
  source: &str,
  loc: usize,
) -> CompileResult<AstNode> {
  ctx.annotate_type(&mut lhs);
  ctx.annotate_type(&mut rhs);
  let lhs_ty = lhs.ty().clone();
  let rhs_ty = rhs.ty().clone();

  let mut result = if lhs_ty.is_integer() && rhs_ty.is_integer() {
    AstNode::binary(BinaryOp::Sub, lhs, rhs)
  } else if lhs_ty.is_pointer_like() && rhs_ty.is_integer() {
    let scale = lhs_ty.base().map(|t| t.size()).unwrap_or(POINTER_SIZE);
    let mut scaled = AstNode::binary(BinaryOp::Mul, rhs, AstNode::number(scale));
    ctx.annotate_type(&mut scaled);
    AstNode::binary(BinaryOp::Sub, lhs, scaled)
  } else if lhs_ty.is_pointer_like() && rhs_ty.is_pointer_like() {
    let scale = lhs_ty.base().map(|t| t.size()).unwrap_or(POINTER_SIZE);
    let mut diff = AstNode::binary(BinaryOp::Sub, lhs, rhs);
    ctx.annotate_type(&mut diff);
    AstNode::binary(BinaryOp::Div, diff, AstNode::number(scale))
  } else {
    return Err(CompileError::at(
      source,
      loc,
      "invalid operands to binary '-'",
    ));
  };

  ctx.annotate_type(&mut result);
  Ok(result)
}

fn validate_ident(name: &str, source: &str, loc: usize) -> CompileResult<()> {
  let mut chars = name.chars();
  let Some(first) = chars.next() else {
    return Err(CompileError::at(source, loc, "identifier is empty"));
  };

  let valid_start = first.is_ascii_alphabetic() || first == '_';
  if !valid_start {
    return Err(CompileError::at(
      source,
      loc,
      format!("identifier '{name}' must start with a letter or underscore"),
    ));
  }

  for (i, ch) in chars.enumerate() {
    if !(ch.is_ascii_alphanumeric() || ch == '_') {
      return Err(CompileError::at(
        source,
        loc + i + 1,
        format!("identifier contains invalid character '{ch}'"),
      ));
    }
  }

  Ok(())
}

/// Lightweight cursor over the token vector.
struct TokenStream<'a> {
  tokens: Vec<Token>,
  source: &'a str,
  pos: usize,
}

impl<'a> TokenStream<'a> {
  /// Take ownership of the token stream; the parser will advance `pos` as it consumes input.
  fn new(tokens: Vec<Token>, source: &'a str) -> Self {
    Self {
      tokens,
      source,
      pos: 0,
    }
  }

  fn peek(&self) -> Option<&Token> {
    self.tokens.get(self.pos)
  }

  fn current_loc(&self) -> usize {
    self
      .tokens
      .get(self.pos)
      .map(|token| token.loc)
      .unwrap_or(self.source.len())
  }

  fn peek_is(&self, symbol: &str) -> bool {
    self
      .peek()
      .filter(|token| {
        matches!(token.kind, TokenKind::Punctuator | TokenKind::Keyword)
          && token_text(token, self.source) == symbol
      })
      .is_some()
  }

  fn peek_keyword(&self) -> Option<&str> {
    self.peek().and_then(|token| {
      if token.kind == TokenKind::Keyword {
        Some(token_text(token, self.source))
      } else {
        None
      }
    })
  }

  /// Consume the current token if it matches the provided punctuator.
  fn equal(&mut self, op: &str) -> bool {
    if let Some(token) = self.peek()
      && matches!(token.kind, TokenKind::Punctuator | TokenKind::Keyword)
      && token.len == op.len()
      && token_text(token, self.source) == op
    {
      self.pos += 1;
      return true;
    }
    false
  }

  fn skip(&mut self, s: &str) -> CompileResult<()> {
    if self.equal(s) {
      Ok(())
    } else {
      let (loc, got) = match self.tokens.get(self.pos) {
        Some(token) => (token.loc, describe_token(Some(token), self.source)),
        None => (self.source.len(), "EOF".to_string()),
      };
      Err(CompileError::at(
        self.source,
        loc,
        format!("expected \"{s}\", but got \"{got}\""),
      ))
    }
  }

  /// Parse the current token as an integer literal returning its value and location.
  fn get_number(&mut self) -> CompileResult<(i64, usize)> {
    if self.pos >= self.tokens.len() {
      return Err(CompileError::at(
        self.source,
        self.source.len(),
        "expected a number, but reached end of input",
      ));
    }

    if let Some(token) = self.tokens.get(self.pos)
      && token.kind == TokenKind::Num
    {
      let value = token.value.ok_or_else(|| {
        CompileError::at(
          self.source,
          token.loc,
          "internal error: numeric token missing value",
        )
      })?;
      let loc = token.loc;
      self.pos += 1;
      return Ok((value, loc));
    }

    let Some(token) = self.tokens.get(self.pos) else {
      return Err(CompileError::at(
        self.source,
        self.source.len(),
        "unexpected end of input while parsing number",
      ));
    };
    let got = describe_token(Some(token), self.source);
    Err(CompileError::at(
      self.source,
      token.loc,
      format!("expected a number, but got \"{got}\""),
    ))
  }

  /// Parse the current token as an identifier.
  fn get_ident(&mut self) -> CompileResult<(String, usize)> {
    if let Some(token) = self.tokens.get(self.pos)
      && token.kind == TokenKind::Ident
    {
      let text = token_text(token, self.source);
      if text.is_empty() {
        return Err(CompileError::at(
          self.source,
          token.loc,
          "identifier is missing characters",
        ));
      }
      let loc = token.loc;
      self.pos += 1;
      return Ok((text.to_string(), loc));
    }

    let Some(token) = self.tokens.get(self.pos) else {
      return Err(CompileError::at(
        self.source,
        self.source.len(),
        "unexpected end of input while parsing identifier",
      ));
    };
    let got = describe_token(Some(token), self.source);
    Err(CompileError::at(
      self.source,
      token.loc,
      format!("expected an identifier, but got \"{got}\""),
    ))
  }

  fn is_eof(&self) -> bool {
    matches!(self.peek().map(|token| token.kind), Some(TokenKind::Eof))
  }
}
