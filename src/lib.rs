pub mod error;
pub mod parser;
pub mod tokenizer;

mod codegen;

pub use error::{CompileError, CompileResult};

pub fn generate_assembly(expr: &str) -> CompileResult<String> {
  let tokens = tokenizer::tokenize(expr)?;
  let ast = parser::parse(tokens, expr)?;
  Ok(codegen::generate(&ast))
}
