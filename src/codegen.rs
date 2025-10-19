use crate::parser::{AstNode, BinaryOp};

pub fn generate(ast: &AstNode) -> String {
  let mut asm = String::new();
  asm.push_str(".global main\n");
  asm.push_str("main:\n");

  emit_expr(ast, &mut asm);

  asm.push_str("    pop %rax\n");
  asm.push_str("    ret\n");

  asm
}

fn emit_expr(node: &AstNode, asm: &mut String) {
  match node {
    AstNode::Num { value } => {
      asm.push_str(&format!("    mov ${value}, %rax\n"));
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
    AstNode::Neg { operand } => {
      emit_expr(operand, asm);
      asm.push_str("    pop %rax\n");
      asm.push_str("    neg %rax\n");
      asm.push_str("    push %rax\n");
    }
  }
}
