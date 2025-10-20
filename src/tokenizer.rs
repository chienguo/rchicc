//! Lexical analysis: turns the raw input string into a vector of tokens.
//!
//! The tokenizer is intentionally tiny – it knows nothing about semantics
//! beyond recognising operators and numeric literals. Multi-character
//! punctuators are matched before single-character ones to avoid ambiguity.

use crate::error::{CompileError, CompileResult};

/// Kinds of tokens recognised by the front-end.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
  Punctuator,
  Num,
  Eof,
}

/// Thin wrapper for lexical information needed by later stages.
#[derive(Debug, Clone)]
pub struct Token {
  pub kind: TokenKind,
  pub value: Option<i64>,
  pub loc: usize,
  pub len: usize,
}

impl Token {
  /// Convenience constructor to keep the `tokenize` loop readable.
  pub fn new(kind: TokenKind, loc: usize, len: usize, value: Option<i64>) -> Self {
    Self {
      kind,
      value,
      loc,
      len,
    }
  }
}

/// Lex the input into a flat vector of tokens terminated by an `Eof` marker.
pub fn tokenize(input: &str) -> CompileResult<Vec<Token>> {
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

    if let Some(op) = ["==", "!=", "<=", ">="]
      .into_iter()
      .find(|op| input[i..].starts_with(op))
    {
      tokens.push(Token::new(TokenKind::Punctuator, i, op.len(), None));
      i += op.len();
      continue;
    }

    if matches!(
      c,
      b'+' | b'-' | b'*' | b'/' | b'(' | b')' | b'<' | b'>' | b';'
    ) {
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

/// Return the slice from the source that produced this token.
pub fn token_text<'a>(token: &Token, source: &'a str) -> &'a str {
  let end = token.loc + token.len;
  &source[token.loc..end]
}

/// Human-friendly description used in diagnostics.
pub fn describe_token(token: Option<&Token>, source: &str) -> String {
  match token {
    Some(t) => match t.kind {
      TokenKind::Eof => "EOF".to_string(),
      _ => token_text(t, source).to_string(),
    },
    None => "EOF".to_string(),
  }
}
