//! Crate root: wires together the compilation pipeline.
//!
//! The stages are intentionally small and composable so they can be evolved
//! independently:
//! - `tokenizer` performs lexical analysis and produces a flat token stream.
//! - `parser` owns all syntactic knowledge and returns a function AST with locals.
//! - `codegen` lowers the parsed function into x86-64 AT&T assembly.
//! - `error` centralises reporting utilities shared by the other modules.

pub mod error;
pub mod parser;
pub mod tokenizer;

mod codegen;

pub use error::{CompileError, CompileResult};

/// Compile a source string into AT&T assembly.
pub fn generate_assembly(expr: &str) -> CompileResult<String> {
  let tokens = tokenizer::tokenize(expr)?;
  let program = parser::parse(tokens, expr)?;
  Ok(codegen::generate(&program))
}
