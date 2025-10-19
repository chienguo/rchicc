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
  value: Option<i64>,
  loc: usize,
  len: usize,
}

impl Token {
  fn new(kind: TokenKind, loc: usize, len: usize, value: Option<i64>) -> Self {
    Self {
      kind,
      value,
      loc,
      len,
    }
  }
}

#[derive(Debug, Clone, Copy)]
enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
}

#[derive(Debug, Clone)]
enum AstNode {
  Num {
    value: i64,
  },
  Neg {
    operand: Box<AstNode>,
  },
  Binary {
    op: BinaryOp,
    lhs: Box<AstNode>,
    rhs: Box<AstNode>,
  },
}

impl AstNode {
  fn number(value: i64) -> Self {
    Self::Num { value }
  }

  fn neg(operand: AstNode) -> Self {
    Self::Neg {
      operand: Box::new(operand),
    }
  }

  fn binary(op: BinaryOp, lhs: AstNode, rhs: AstNode) -> Self {
    Self::Binary {
      op,
      lhs: Box::new(lhs),
      rhs: Box::new(rhs),
    }
  }
}

fn tokenize(input: &str) -> CompileResult<Vec<Token>> {
  let mut tokens = Vec::new();
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
      tokens.push(Token::new(TokenKind::Num, start, i - start, Some(value)));
      continue;
    }

    if matches!(c, b'+' | b'-' | b'*' | b'/' | b'(' | b')') {
      tokens.push(Token::new(TokenKind::Punctuator, i, 1, None));
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

  tokens.push(Token::new(TokenKind::Eof, input.len(), 0, None));
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

  fn equal(&mut self, op: &str) -> Option<&Token> {
    if let Some(token) = self.tokens.get(self.pos)
      && token.kind == TokenKind::Punctuator
      && token.len == op.len()
      && token_text(token, self.source) == op
    {
      self.pos += 1;
      return Some(token);
    }
    None
  }

  fn peek(&self) -> Option<&Token> {
    self.tokens.get(self.pos)
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
  if stream.equal("+").is_some() {
    let operand = parse_unary(stream)?;
    return Ok(operand);
  }

  if stream.equal("-").is_some() {
    let operand = parse_unary(stream)?;
    return Ok(AstNode::neg(operand));
  }

  parse_primary(stream)
}

fn parse_primary(stream: &mut TokenStream) -> CompileResult<AstNode> {
  if stream.equal("(").is_some() {
    let node = parse_expr(stream)?;
    stream.skip(")")?;
    Ok(node)
  } else {
    let (value, _) = stream.get_number()?;
    Ok(AstNode::number(value))
  }
}

fn gen_expr(node: &AstNode, asm: &mut String, _expr: &str) -> CompileResult<()> {
  match node {
    AstNode::Num { value } => {
      asm.push_str(&format!("    mov ${value}, %rax\n"));
      asm.push_str("    push %rax\n");
    }
    AstNode::Binary { op, lhs, rhs } => {
      gen_expr(lhs, asm, _expr)?;
      gen_expr(rhs, asm, _expr)?;
      asm.push_str("    pop %rdi\n");
      asm.push_str("    pop %rax\n");
      match op {
        BinaryOp::Add => asm.push_str("    add %rdi, %rax\n"),
        BinaryOp::Sub => asm.push_str("    sub %rdi, %rax\n"),
        BinaryOp::Mul => asm.push_str("    imul %rdi, %rax\n"),
        BinaryOp::Div => {
          asm.push_str("    cqo\n");
          asm.push_str("    idiv %rdi\n");
        }
      }
      asm.push_str("    push %rax\n");
    }
    AstNode::Neg { operand } => {
      gen_expr(operand, asm, _expr)?;
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
