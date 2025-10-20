//! Code generation: lower the parsed AST into AT&T x86-64 assembly.
//!
//! The emitter currently uses a simple stack machine: every expression leaves
//! a single value on the stack and statements pop intermediate results as we
//! chain them. This mirrors chibicc's early stages and keeps the code easy to
//! audit.

use crate::parser::{AstNode, BinaryOp, Stmt};

/// Emit assembly for a statement list.
pub fn generate(program: &Stmt) -> String {
  let mut asm = String::new();
  asm.push_str(".global main\n");
  asm.push_str("main:\n");
  asm.push_str("    push %rbp\n");
  asm.push_str("    mov %rsp, %rbp\n");
  asm.push_str("    sub $208, %rsp\n");

  emit_stmt(program, &mut asm);

  asm.push_str("    pop %rax\n");
  asm.push_str("    mov %rbp, %rsp\n");
  asm.push_str("    pop %rbp\n");
  asm.push_str("    ret\n");

  asm
}

/// Walk the statement list, emitting code for each expression and discarding
/// intermediate results to keep stack balance intact.
fn emit_stmt(stmt: &Stmt, asm: &mut String) {
  emit_expr(&stmt.expr, asm);

  if let Some(next) = stmt.next.as_deref() {
    asm.push_str("    pop %rax\n");
    emit_stmt(next, asm);
  }
}

/// Emit stack-based code for a single expression node.
fn emit_expr(node: &AstNode, asm: &mut String) {
  match node {
    AstNode::Num { value } => {
      asm.push_str(&format!("    mov ${value}, %rax\n"));
      asm.push_str("    push %rax\n");
    }
    AstNode::Var { name } => {
      let offset = var_offset(*name);
      asm.push_str(&format!("    mov -{offset}(%rbp), %rax\n"));
      asm.push_str("    push %rax\n");
    }
    AstNode::Binary { op, lhs, rhs } => {
      emit_expr(lhs, asm);
      emit_expr(rhs, asm);
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
        BinaryOp::Eq => {
          asm.push_str("    cmp %rdi, %rax\n");
          asm.push_str("    sete %al\n");
          asm.push_str("    movzbl %al, %eax\n");
        }
        BinaryOp::Ne => {
          asm.push_str("    cmp %rdi, %rax\n");
          asm.push_str("    setne %al\n");
          asm.push_str("    movzbl %al, %eax\n");
        }
        BinaryOp::Lt => {
          asm.push_str("    cmp %rdi, %rax\n");
          asm.push_str("    setl %al\n");
          asm.push_str("    movzbl %al, %eax\n");
        }
        BinaryOp::Le => {
          asm.push_str("    cmp %rdi, %rax\n");
          asm.push_str("    setle %al\n");
          asm.push_str("    movzbl %al, %eax\n");
        }
        BinaryOp::Gt => {
          asm.push_str("    cmp %rax, %rdi\n");
          asm.push_str("    setl %al\n");
          asm.push_str("    movzbl %al, %eax\n");
        }
        BinaryOp::Ge => {
          asm.push_str("    cmp %rax, %rdi\n");
          asm.push_str("    setle %al\n");
          asm.push_str("    movzbl %al, %eax\n");
        }
      }
      asm.push_str("    push %rax\n");
    }
    AstNode::Assign { lhs, rhs, .. } => {
      emit_addr(lhs, asm);
      emit_expr(rhs, asm);
      asm.push_str("    pop %rdi\n");
      asm.push_str("    pop %rax\n");
      asm.push_str("    mov %rdi, (%rax)\n");
      asm.push_str("    push %rdi\n");
    }
    AstNode::Neg { operand } => {
      emit_expr(operand, asm);
      asm.push_str("    pop %rax\n");
      asm.push_str("    neg %rax\n");
      asm.push_str("    push %rax\n");
    }
  }
}

fn emit_addr(node: &AstNode, asm: &mut String) {
  match node {
    AstNode::Var { name } => {
      let offset = var_offset(*name);
      asm.push_str(&format!("    lea -{offset}(%rbp), %rax\n"));
      asm.push_str("    push %rax\n");
    }
    _ => panic!("not an lvalue"),
  }
}

fn var_offset(name: char) -> i64 {
  if !name.is_ascii_lowercase() {
    panic!("unsupported variable name: {name}");
  }

  let index = (name as u8 - b'a') as i64 + 1;
  index * 8
}
