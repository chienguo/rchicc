use std::env;
use std::process;

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

fn tokenize(input: &str) -> Result<Vec<Token>, String> {
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
        .map_err(|err| format!("invalid number '{text}': {err}"))?;
      let idx = tokens.len();
      tokens.push(Token::new(TokenKind::Num, start, i - start, Some(value)));
      if let Some(prev) = prev_index {
        tokens[prev].next = Some(idx);
      }
      prev_index = Some(idx);
      continue;
    }

    if c == b'+' || c == b'-' {
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
    return Err(format!("invalid token: '{invalid_char}' at byte {i}"));
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

  fn equal(&mut self, op: &str) -> bool {
    if let Some(token) = self.tokens.get(self.pos).cloned()
      && token.kind == TokenKind::Punctuator
      && token.len == op.len()
      && token_text(&token, self.source) == op
    {
      self.pos += 1;
      return true;
    }
    false
  }

  fn skip(&mut self, s: &str) -> Result<(), String> {
    if self.equal(s) {
      Ok(())
    } else {
      let got = describe_token(self.tokens.get(self.pos), self.source);
      Err(format!("expected \"{s}\", but got \"{got}\""))
    }
  }

  fn get_number(&mut self) -> Result<i64, String> {
    let token = match self.tokens.get(self.pos) {
      Some(token) => token.clone(),
      None => return Err("expected a number, but reached EOF".to_string()),
    };

    if token.kind == TokenKind::Num {
      self.pos += 1;
      Ok(token.value.expect("number token must have a value"))
    } else {
      let got = describe_token(Some(&token), self.source);
      Err(format!("expected a number, but got \"{got}\""))
    }
  }

  fn is_eof(&self) -> bool {
    matches!(
      self.tokens.get(self.pos).map(|token| token.kind),
      Some(TokenKind::Eof)
    )
  }
}

fn generate_assembly(expr: &str) -> Result<String, String> {
  let tokens = tokenize(expr)?;
  let mut stream = TokenStream::new(tokens, expr);

  if stream.is_eof() {
    return Err("expression is empty".to_string());
  }

  let mut asm = String::new();
  asm.push_str(".global main\n");
  asm.push_str("main:\n");

  let first = stream.get_number()?;
  asm.push_str(&format!("    mov ${first}, %rax\n"));

  while !stream.is_eof() {
    if stream.equal("+") {
      let value = stream.get_number()?;
      asm.push_str(&format!("    add ${value}, %rax\n"));
      continue;
    }

    stream.skip("-")?;
    let value = stream.get_number()?;
    asm.push_str(&format!("    sub ${value}, %rax\n"));
  }

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
