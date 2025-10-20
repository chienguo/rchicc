//! Recursive-descent parser producing a statement list and expression AST.
//!
//! The parser mirrors the classic chibicc structure: we maintain a
//! precedence-climbing set of helpers and expose a thin statement layer so
//! sequencing lives outside the expression tree. This keeps the grammar easy
//! to extend with additional statement kinds later on.

use crate::error::{CompileError, CompileResult};
use crate::tokenizer::{Token, TokenKind, describe_token, token_text};
use std::collections::HashMap;

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
  },
  Var {
    obj: usize,
  },
  Neg {
    operand: Box<AstNode>,
  },
  Binary {
    op: BinaryOp,
    lhs: Box<AstNode>,
    rhs: Box<AstNode>,
  },
  Assign {
    lhs: Box<AstNode>,
    rhs: Box<AstNode>,
  },
}

impl AstNode {
  pub fn number(value: i64) -> Self {
    Self::Num { value }
  }

  pub fn var(obj: usize) -> Self {
    Self::Var { obj }
  }

  pub fn unary_neg(operand: AstNode) -> Self {
    Self::Neg {
      operand: Box::new(operand),
    }
  }

  pub fn binary(op: BinaryOp, lhs: AstNode, rhs: AstNode) -> Self {
    Self::Binary {
      op,
      lhs: Box::new(lhs),
      rhs: Box::new(rhs),
    }
  }

  pub fn assign(lhs: AstNode, rhs: AstNode) -> Self {
    Self::Assign {
      lhs: Box::new(lhs),
      rhs: Box::new(rhs),
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
}

impl Obj {
  pub fn new(name: impl Into<String>) -> Self {
    Self {
      name: name.into(),
      offset: 0,
    }
  }
}

#[derive(Debug, Clone)]
pub struct Function {
  pub body: Box<Stmt>,
  pub locals: Vec<Obj>,
  pub stack_size: i64,
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
pub fn parse(tokens: Vec<Token>, source: &str) -> CompileResult<Function> {
  let mut stream = TokenStream::new(tokens, source);

  if stream.is_eof() {
    return Err(CompileError::at(source, 0, "program is empty"));
  }

  let mut ctx = ParserContext::new();
  let body = parse_stmt(&mut stream, &mut ctx)?;

  if !stream.is_eof() {
    let token = stream.current().ok_or_else(|| {
      CompileError::at(
        source,
        source.len(),
        "unexpected end of input after statement",
      )
    })?;
    let got = describe_token(Some(token), source);
    return Err(CompileError::at(
      source,
      token.loc,
      format!("unexpected token \"{got}\""),
    ));
  }

  let stack_size = ctx.assign_offsets();

  Ok(Function {
    body,
    locals: ctx.locals,
    stack_size,
  })
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

  fn get_or_create_local(&mut self, name: &str) -> usize {
    if let Some(&index) = self.map.get(name) {
      return index;
    }
    let index = self.locals.len();
    self.locals.push(Obj::new(name));
    self.map.insert(name.to_string(), index);
    index
  }

  fn assign_offsets(&mut self) -> i64 {
    let mut offset: i64 = 0;
    for obj in &mut self.locals {
      offset += 8;
      obj.offset = offset;
    }
    align_to(offset, 16)
  }
}

fn align_to(n: i64, align: i64) -> i64 {
  if align == 0 {
    return n;
  }
  ((n + align - 1) / align) * align
}

fn parse_stmt(stream: &mut TokenStream, ctx: &mut ParserContext) -> CompileResult<Box<Stmt>> {
  parse_expr_stmt(stream, ctx)
}

fn parse_expr_stmt(stream: &mut TokenStream, ctx: &mut ParserContext) -> CompileResult<Box<Stmt>> {
  let expr = parse_expr(stream, ctx)?;
  stream.skip(";")?;

  let next = if stream.is_eof() {
    None
  } else {
    Some(parse_stmt(stream, ctx)?)
  };

  Ok(Box::new(Stmt { expr, next }))
}

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
    let rhs = parse_assign(stream, ctx)?;
    return match node {
      AstNode::Var { .. } => Ok(AstNode::assign(node, rhs)),
      _ => Err(CompileError::at(
        stream.source,
        assign_loc,
        "left-hand side of assignment is not a variable",
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

    let op = match op_str {
      "+" => BinaryOp::Add,
      "-" => BinaryOp::Sub,
      _ => unreachable!(),
    };

    stream.skip(op_str)?;
    let rhs = parse_mul(stream, ctx)?;
    node = AstNode::binary(op, node, rhs);
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

  parse_primary(stream, ctx)
}

fn parse_primary(stream: &mut TokenStream, ctx: &mut ParserContext) -> CompileResult<AstNode> {
  if stream.equal("(") {
    let node = parse_expr(stream, ctx)?;
    stream.skip(")")?;
    Ok(node)
  } else {
    if matches!(
      stream.peek().map(|token| token.kind),
      Some(TokenKind::Ident)
    ) {
      let (name, loc) = stream.get_ident()?;
      validate_ident(&name, stream.source, loc)?;
      let index = ctx.get_or_create_local(&name);
      return Ok(AstNode::var(index));
    }

    let (value, _) = stream.get_number()?;
    Ok(AstNode::number(value))
  }
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

  fn current(&self) -> Option<&Token> {
    self.peek()
  }

  fn current_loc(&self) -> usize {
    self
      .tokens
      .get(self.pos)
      .map(|token| token.loc)
      .unwrap_or(self.source.len())
  }

  /// Consume the current token if it matches the provided punctuator.
  fn equal(&mut self, op: &str) -> bool {
    if let Some(token) = self.peek()
      && token.kind == TokenKind::Punctuator
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
