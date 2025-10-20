//! Shared error utilities used across the compilation pipeline.
//!
//! Diagnostics are kept lightweight on purpose – these routines format
//! messages in a style reminiscent of chibicc, pointing at the offending
//! byte with a caret.

use snafu::Snafu;

pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug, Snafu)]
pub enum CompileError {
  #[snafu(display("{expr_line}\n{marker} {message}"))]
  WithLocation {
    expr_line: String,
    marker: String,
    message: String,
  },
}

impl CompileError {
  /// Construct an error anchored at a specific byte offset in the source.
  pub fn at(expr: &str, loc: usize, message: impl Into<String>) -> Self {
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
