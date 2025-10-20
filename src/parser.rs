//! Recursive-descent parser producing a statement list and expression AST.
//!
//! The parser mirrors the classic chibicc structure: we maintain a
//! precedence-climbing set of helpers and expose a thin statement layer so
//! sequencing lives outside the expression tree. This keeps the grammar easy
//! to extend with additional statement kinds later on.

use crate::error::{CompileError, CompileResult};
use crate::tokenizer::{Token, TokenKind, describe_token, token_text};

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
    name: char,
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
    name: Option<char>,
  },
}

impl AstNode {
  pub fn number(value: i64) -> Self {
    Self::Num { value }
  }

  pub fn var(name: char) -> Self {
    Self::Var { name }
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

  pub fn assign(lhs: AstNode, rhs: AstNode, name: Option<char>) -> Self {
    Self::Assign {
      lhs: Box::new(lhs),
      rhs: Box::new(rhs),
      name,
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
pub fn parse(tokens: Vec<Token>, source: &str) -> CompileResult<Box<Stmt>> {
  let mut stream = TokenStream::new(tokens, source);

  if stream.is_eof() {
    return Err(CompileError::at(source, 0, "program is empty"));
  }

  let stmts = parse_stmt(&mut stream)?;

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

  Ok(stmts)
}

fn parse_stmt(stream: &mut TokenStream) -> CompileResult<Box<Stmt>> {
  parse_expr_stmt(stream)
}

fn parse_expr_stmt(stream: &mut TokenStream) -> CompileResult<Box<Stmt>> {
  // The only statement form today is an expression followed by a semicolon.
  // Keeping this isolated makes it trivial to bolt on new statement kinds.
  let expr = parse_expr(stream)?;
  stream.skip(";")?;

  let next = if stream.is_eof() {
    None
  } else {
    Some(parse_stmt(stream)?)
  };

  Ok(Box::new(Stmt { expr, next }))
}

fn parse_expr(stream: &mut TokenStream) -> CompileResult<AstNode> {
  parse_assign(stream)
}

fn parse_assign(stream: &mut TokenStream) -> CompileResult<AstNode> {
  let node = parse_equality(stream)?;

  if stream.equal("=") {
    let lhs_name = match &node {
      AstNode::Var { name } => Some(*name),
      _ => None,
    };
    let rhs = parse_assign(stream)?;
    return Ok(AstNode::assign(node, rhs, lhs_name));
  }

  Ok(node)
}

fn parse_equality(stream: &mut TokenStream) -> CompileResult<AstNode> {
  let mut node = parse_relational(stream)?;

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
    let rhs = parse_relational(stream)?;
    node = AstNode::binary(op, node, rhs);
  }

  Ok(node)
}

fn parse_relational(stream: &mut TokenStream) -> CompileResult<AstNode> {
  let mut node = parse_add(stream)?;

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
    let rhs = parse_add(stream)?;
    node = AstNode::binary(op, node, rhs);
  }

  Ok(node)
}

fn parse_add(stream: &mut TokenStream) -> CompileResult<AstNode> {
  let mut node = parse_mul(stream)?;

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
    let rhs = parse_mul(stream)?;
    node = AstNode::binary(op, node, rhs);
  }

  Ok(node)
}

fn parse_mul(stream: &mut TokenStream) -> CompileResult<AstNode> {
  let mut node = parse_unary(stream)?;

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
    let rhs = parse_unary(stream)?;
    node = AstNode::binary(op, node, rhs);
  }

  Ok(node)
}

fn parse_unary(stream: &mut TokenStream) -> CompileResult<AstNode> {
  if stream.equal("+") {
    let operand = parse_unary(stream)?;
    return Ok(operand);
  }

  if stream.equal("-") {
    let operand = parse_unary(stream)?;
    return Ok(AstNode::unary_neg(operand));
  }

  parse_primary(stream)
}

fn parse_primary(stream: &mut TokenStream) -> CompileResult<AstNode> {
  if stream.equal("(") {
    let node = parse_expr(stream)?;
    stream.skip(")")?;
    Ok(node)
  } else {
    if matches!(
      stream.peek().map(|token| token.kind),
      Some(TokenKind::Ident)
    ) {
      let (name, _) = stream.get_ident()?;
      return Ok(AstNode::var(name));
    }

    let (value, _) = stream.get_number()?;
    Ok(AstNode::number(value))
  }
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
  fn get_ident(&mut self) -> CompileResult<(char, usize)> {
    if let Some(token) = self.tokens.get(self.pos)
      && token.kind == TokenKind::Ident
    {
      let Some(ident) = token_text(token, self.source).chars().next() else {
        return Err(CompileError::at(
          self.source,
          token.loc,
          "identifier is missing characters",
        ));
      };
      let loc = token.loc;
      self.pos += 1;
      return Ok((ident, loc));
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
