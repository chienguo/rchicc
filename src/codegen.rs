//! Code generation: lower the parsed AST into AT&T x86-64 assembly.
//!
//! The emitter currently uses a simple stack machine: every expression leaves
//! a single value on the stack and statements pop intermediate results as we
//! chain them. Locals live on the stack frame and are addressed relative to
//! `%rbp`.

use crate::parser::{AstNode, BinaryOp, Function, Stmt};

/// Emit assembly for a function.
pub fn generate(func: &Function) -> String {
  let mut asm = String::new();
  asm.push_str(".global main\n");
  asm.push_str("main:\n");
  asm.push_str("    push %rbp\n");
  asm.push_str("    mov %rsp, %rbp\n");
  if func.stack_size > 0 {
    asm.push_str(&format!("    sub ${}, %rsp\n", func.stack_size));
  }

  emit_stmt_list(func.body.as_deref(), func, &mut asm, true);

  asm.push_str("    pop %rax\n");
  asm.push_str(".L.return:\n");
  asm.push_str("    mov %rbp, %rsp\n");
  asm.push_str("    pop %rbp\n");
  asm.push_str("    ret\n");

  asm
}

/// Walk a statement list, optionally keeping the final value on the stack.
fn emit_stmt_list(mut current: Option<&Stmt>, func: &Function, asm: &mut String, keep_final: bool) {
  if current.is_none() {
    if keep_final {
      asm.push_str("    mov $0, %rax\n");
      asm.push_str("    push %rax\n");
    }
    return;
  }

  while let Some(stmt) = current {
    let is_return = matches!(stmt.expr, AstNode::Return { .. });
    let next = stmt.next.as_deref();
    emit_expr(&stmt.expr, func, asm);

    if is_return {
      return;
    }

    current = next;
    if current.is_some() || !keep_final {
      asm.push_str("    pop %rax\n");
    }
  }
}

/// Emit stack-based code for a single expression node.
fn emit_expr(node: &AstNode, func: &Function, asm: &mut String) {
  match node {
    AstNode::Num { value } => {
      asm.push_str(&format!("    mov ${value}, %rax\n"));
      asm.push_str("    push %rax\n");
    }
    AstNode::Var { obj } => {
      let offset = func.locals[*obj].offset;
      asm.push_str(&format!("    mov -{offset}(%rbp), %rax\n"));
      asm.push_str("    push %rax\n");
    }
    AstNode::Binary { op, lhs, rhs } => {
      emit_expr(lhs, func, asm);
      emit_expr(rhs, func, asm);
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
    AstNode::Assign { lhs, rhs } => {
      emit_addr(lhs, func, asm);
      emit_expr(rhs, func, asm);
      asm.push_str("    pop %rdi\n");
      asm.push_str("    pop %rax\n");
      asm.push_str("    mov %rdi, (%rax)\n");
      asm.push_str("    push %rdi\n");
    }
    AstNode::Block { body } => {
      emit_stmt_list(body.as_deref(), func, asm, true);
    }
    AstNode::Return { value } => {
      emit_expr(value, func, asm);
      asm.push_str("    pop %rax\n");
      asm.push_str("    jmp .L.return\n");
    }
    AstNode::Neg { operand } => {
      emit_expr(operand, func, asm);
      asm.push_str("    pop %rax\n");
      asm.push_str("    neg %rax\n");
      asm.push_str("    push %rax\n");
    }
  }
}

fn emit_addr(node: &AstNode, func: &Function, asm: &mut String) {
  match node {
    AstNode::Var { obj } => {
      let offset = func.locals[*obj].offset;
      asm.push_str(&format!("    lea -{offset}(%rbp), %rax\n"));
      asm.push_str("    push %rax\n");
    }
    _ => panic!("not an lvalue"),
  }
}
