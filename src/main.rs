use snafu::Snafu;
use std::env;
use std::process;

type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug, Snafu)]
enum CompileError {
  #[snafu(display("{expr_line}\n{marker} {message}"))]
  WithLocation {
    expr_line: String,
    marker: String,
    message: String,
  },
}

impl CompileError {
  fn at(expr: &str, loc: usize, message: impl Into<String>) -> Self {
    let expr_line = format!("'{expr}'");
    let safe_loc = loc.min(expr.len());
    let char_offset = expr[..safe_loc].chars().count() + 1; // account for opening quote
    let marker = format!("{}^", " ".repeat(char_offset));
    Self::WithLocation {
      expr_line,
      marker,
      message: message.into(),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TokenKind {
  Punctuator,
  Num,
  Eof,
}

#[derive(Debug, Clone)]
struct Token {
  kind: TokenKind,
  next: Option<usize>,
  value: Option<i64>,
  loc: usize,
  len: usize,
}

impl Token {
  fn new(kind: TokenKind, loc: usize, len: usize, value: Option<i64>) -> Self {
    Self {
      kind,
      next: None,
      value,
      loc,
      len,
    }
  }
}

#[derive(Debug, Clone, Copy)]
enum AstKind {
  Add,
  Sub,
  Mul,
  Div,
  Neg,
  Num,
}

#[derive(Debug, Clone)]
struct AstNode {
  kind: AstKind,
  lhs: Option<Box<AstNode>>,
  rhs: Option<Box<AstNode>>,
  value: Option<i64>,
  loc: usize,
}

impl AstNode {
  fn new_binary(kind: AstKind, lhs: AstNode, rhs: AstNode, loc: usize) -> Self {
    Self {
      kind,
      lhs: Some(Box::new(lhs)),
      rhs: Some(Box::new(rhs)),
      value: None,
      loc,
    }
  }

  fn new_unary(kind: AstKind, operand: AstNode, loc: usize) -> Self {
    Self {
      kind,
      lhs: Some(Box::new(operand)),
      rhs: None,
      value: None,
      loc,
    }
  }

  fn new_num(value: i64, loc: usize) -> Self {
    Self {
      kind: AstKind::Num,
      lhs: None,
      rhs: None,
      value: Some(value),
      loc,
    }
  }
}

fn tokenize(input: &str) -> CompileResult<Vec<Token>> {
  let mut tokens = Vec::new();
  let mut prev_index: Option<usize> = None;
  let bytes = input.as_bytes();
  let mut i = 0;

  while i < bytes.len() {
    let c = bytes[i];
    if c.is_ascii_whitespace() {
      i += 1;
      continue;
    }

    if c.is_ascii_digit() {
      let start = i;
      i += 1;
      while i < bytes.len() && bytes[i].is_ascii_digit() {
        i += 1;
      }
      let text = &input[start..i];
      let value = text
        .parse::<i64>()
        .map_err(|err| CompileError::at(input, start, format!("invalid number: {err}")))?;
      let idx = tokens.len();
      tokens.push(Token::new(TokenKind::Num, start, i - start, Some(value)));
      if let Some(prev) = prev_index {
        tokens[prev].next = Some(idx);
      }
      prev_index = Some(idx);
      continue;
    }

    if matches!(c, b'+' | b'-' | b'*' | b'/' | b'(' | b')') {
      let idx = tokens.len();
      tokens.push(Token::new(TokenKind::Punctuator, i, 1, None));
      if let Some(prev) = prev_index {
        tokens[prev].next = Some(idx);
      }
      prev_index = Some(idx);
      i += 1;
      continue;
    }

    let invalid_char = input[i..].chars().next().unwrap_or('\0');
    let message = if invalid_char.is_ascii_alphabetic() {
      "expect a number".to_string()
    } else if invalid_char == '\0' {
      "unexpected end of input".to_string()
    } else {
      format!("invalid token: '{invalid_char}'")
    };
    return Err(CompileError::at(input, i, message));
  }

  let eof_index = tokens.len();
  tokens.push(Token::new(TokenKind::Eof, input.len(), 0, None));
  if let Some(prev) = prev_index {
    tokens[prev].next = Some(eof_index);
  }
  Ok(tokens)
}

fn token_text<'a>(token: &Token, source: &'a str) -> &'a str {
  let end = token.loc + token.len;
  &source[token.loc..end]
}

fn describe_token(token: Option<&Token>, source: &str) -> String {
  match token {
    Some(t) => match t.kind {
      TokenKind::Eof => "EOF".to_string(),
      _ => token_text(t, source).to_string(),
    },
    None => "EOF".to_string(),
  }
}

struct TokenStream<'a> {
  tokens: Vec<Token>,
  source: &'a str,
  pos: usize,
}

impl<'a> TokenStream<'a> {
  fn new(tokens: Vec<Token>, source: &'a str) -> Self {
    Self {
      tokens,
      source,
      pos: 0,
    }
  }

  fn current(&self) -> Option<&Token> {
    self.tokens.get(self.pos)
  }

  fn equal(&mut self, op: &str) -> Option<Token> {
    if let Some(token) = self.tokens.get(self.pos).cloned()
      && token.kind == TokenKind::Punctuator
      && token.len == op.len()
      && token_text(&token, self.source) == op
    {
      self.pos += 1;
      return Some(token);
    }
    None
  }

  fn skip(&mut self, s: &str) -> CompileResult<()> {
    if self.equal(s).is_some() {
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

  fn get_number(&mut self) -> CompileResult<(i64, usize)> {
    if self.pos >= self.tokens.len() {
      return Err(CompileError::at(
        self.source,
        self.source.len(),
        "expected a number, but reached end of input",
      ));
    }

    if self.tokens[self.pos].kind == TokenKind::Num {
      let token = self.tokens[self.pos].clone();
      self.pos += 1;
      let value = token.value.ok_or_else(|| {
        CompileError::at(
          self.source,
          token.loc,
          "internal error: numeric token missing value",
        )
      })?;
      return Ok((value, token.loc));
    }

    let token = &self.tokens[self.pos];
    let got = describe_token(Some(token), self.source);
    Err(CompileError::at(
      self.source,
      token.loc,
      format!("expected a number, but got \"{got}\""),
    ))
  }

  fn is_eof(&self) -> bool {
    matches!(
      self.tokens.get(self.pos).map(|token| token.kind),
      Some(TokenKind::Eof)
    )
  }
}

fn parse_expr(stream: &mut TokenStream) -> CompileResult<AstNode> {
  parse_add(stream)
}

fn parse_add(stream: &mut TokenStream) -> CompileResult<AstNode> {
  let mut node = parse_mul(stream)?;

  loop {
    if let Some(op_token) = stream.equal("+") {
      let rhs = parse_mul(stream)?;
      node = AstNode::new_binary(AstKind::Add, node, rhs, op_token.loc);
    } else if let Some(op_token) = stream.equal("-") {
      let rhs = parse_mul(stream)?;
      node = AstNode::new_binary(AstKind::Sub, node, rhs, op_token.loc);
    } else {
      break;
    }
  }

  Ok(node)
}

fn parse_mul(stream: &mut TokenStream) -> CompileResult<AstNode> {
  let mut node = parse_unary(stream)?;

  loop {
    if let Some(op_token) = stream.equal("*") {
      let rhs = parse_unary(stream)?;
      node = AstNode::new_binary(AstKind::Mul, node, rhs, op_token.loc);
    } else if let Some(op_token) = stream.equal("/") {
      let rhs = parse_unary(stream)?;
      node = AstNode::new_binary(AstKind::Div, node, rhs, op_token.loc);
    } else {
      break;
    }
  }

  Ok(node)
}

fn parse_unary(stream: &mut TokenStream) -> CompileResult<AstNode> {
  if stream.equal("+").is_some() {
    let operand = parse_unary(stream)?;
    return Ok(operand);
  }

  if let Some(op_token) = stream.equal("-") {
    let operand = parse_unary(stream)?;
    return Ok(AstNode::new_unary(AstKind::Neg, operand, op_token.loc));
  }

  parse_primary(stream)
}

fn parse_primary(stream: &mut TokenStream) -> CompileResult<AstNode> {
  if stream.equal("(").is_some() {
    let node = parse_expr(stream)?;
    stream.skip(")")?;
    Ok(node)
  } else {
    let (value, loc) = stream.get_number()?;
    Ok(AstNode::new_num(value, loc))
  }
}

fn gen_expr(node: &AstNode, asm: &mut String, expr: &str) -> CompileResult<()> {
  match node.kind {
    AstKind::Num => {
      let value = node.value.ok_or_else(|| {
        CompileError::at(expr, node.loc, "internal error: number node missing value")
      })?;
      asm.push_str(&format!("    mov ${value}, %rax\n"));
      asm.push_str("    push %rax\n");
    }
    AstKind::Add | AstKind::Sub | AstKind::Mul | AstKind::Div => {
      let lhs = node
        .lhs
        .as_ref()
        .ok_or_else(|| CompileError::at(expr, node.loc, "internal error: missing left operand"))?;
      let rhs = node
        .rhs
        .as_ref()
        .ok_or_else(|| CompileError::at(expr, node.loc, "internal error: missing right operand"))?;
      gen_expr(lhs, asm, expr)?;
      gen_expr(rhs, asm, expr)?;
      asm.push_str("    pop %rdi\n");
      asm.push_str("    pop %rax\n");
      match node.kind {
        AstKind::Add => asm.push_str("    add %rdi, %rax\n"),
        AstKind::Sub => asm.push_str("    sub %rdi, %rax\n"),
        AstKind::Mul => asm.push_str("    imul %rdi, %rax\n"),
        AstKind::Div => {
          asm.push_str("    cqo\n");
          asm.push_str("    idiv %rdi\n");
        }
        _ => unreachable!(),
      }
      asm.push_str("    push %rax\n");
    }
    AstKind::Neg => {
      let operand = node
        .lhs
        .as_ref()
        .ok_or_else(|| CompileError::at(expr, node.loc, "internal error: missing unary operand"))?;
      gen_expr(operand, asm, expr)?;
      asm.push_str("    pop %rax\n");
      asm.push_str("    neg %rax\n");
      asm.push_str("    push %rax\n");
    }
  }

  Ok(())
}

fn generate_assembly(expr: &str) -> CompileResult<String> {
  let tokens = tokenize(expr)?;
  let mut stream = TokenStream::new(tokens, expr);

  if stream.is_eof() {
    return Err(CompileError::at(expr, 0, "expression is empty"));
  }

  let node = parse_expr(&mut stream)?;
  if !stream.is_eof() {
    let token = stream.current().ok_or_else(|| {
      CompileError::at(expr, expr.len(), "unexpected end of input after expression")
    })?;
    let got = describe_token(Some(token), expr);
    return Err(CompileError::at(
      expr,
      token.loc,
      format!("unexpected token \"{got}\""),
    ));
  }

  let mut asm = String::new();
  asm.push_str(".global main\n");
  asm.push_str("main:\n");

  gen_expr(&node, &mut asm, expr)?;
  asm.push_str("    pop %rax\n");
  asm.push_str("    ret\n");
  Ok(asm)
}

fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() != 2 {
    let program = args.first().map(String::as_str).unwrap_or("rchicc");
    eprintln!("usage: {program} <expr>");
    process::exit(1);
  }

  match generate_assembly(&args[1]) {
    Ok(asm) => print!("{asm}"),
    Err(err) => {
      eprintln!("{err}");
      process::exit(1);
    }
  }
}
