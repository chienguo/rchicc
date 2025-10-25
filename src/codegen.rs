//! Code generation: lower the parsed AST into AT&T x86-64 assembly.
//!
//! Expressions use a simple stack machine: each node pushes exactly one value.
//! Statements consume their results unless explicitly requested (e.g. block
//! expressions). Locals are addressed relative to `%rbp`.

use crate::parser::{AstNode, BinaryOp, Function, Program, Stmt};

const ARG_REGISTERS: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

struct Codegen {
  next_label: usize,
}

impl Codegen {
  fn new() -> Self {
    Self { next_label: 0 }
  }

  fn new_label(&mut self) -> usize {
    let id = self.next_label;
    self.next_label += 1;
    id
  }
}

/// Emit assembly for every function in the program.
pub fn generate(program: &Program) -> String {
  let mut asm = String::new();
  let mut cg = Codegen::new();
  for func in &program.functions {
    emit_function(func, &mut asm, &mut cg);
    asm.push('\n');
  }
  asm
}

fn emit_function(func: &Function, asm: &mut String, cg: &mut Codegen) {
  let return_label = format!(".L.return.{}", func.name);

  asm.push_str(&format!(".global {}\n", func.name));
  asm.push_str(&format!("{}:\n", func.name));
  asm.push_str("    push %rbp\n");
  asm.push_str("    mov %rsp, %rbp\n");
  if func.stack_size > 0 {
    asm.push_str(&format!("    sub ${}, %rsp\n", func.stack_size));
  }

  for (i, &local_index) in func.params.iter().enumerate() {
    let offset = func.locals[local_index].offset;
    let reg = ARG_REGISTERS[i];
    asm.push_str(&format!("    mov {reg}, -{offset}(%rbp)\n"));
  }

  emit_stmt_list(func.body.as_deref(), func, asm, true, &return_label, cg);

  asm.push_str("    pop %rax\n");
  asm.push_str(&format!("{}:\n", return_label));
  asm.push_str("    mov %rbp, %rsp\n");
  asm.push_str("    pop %rbp\n");
  asm.push_str("    ret\n");
}

/// Emit a statement list, optionally keeping the final value on the stack.
fn emit_stmt_list(
  mut current: Option<&Stmt>,
  func: &Function,
  asm: &mut String,
  keep_final: bool,
  return_label: &str,
  cg: &mut Codegen,
) {
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
    emit_expr(&stmt.expr, func, asm, return_label, cg);

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
fn emit_expr(
  node: &AstNode,
  func: &Function,
  asm: &mut String,
  return_label: &str,
  cg: &mut Codegen,
) {
  match node {
    AstNode::Num { value, .. } => {
      asm.push_str(&format!("    mov ${value}, %rax\n"));
      asm.push_str("    push %rax\n");
    }
    AstNode::Var { obj, .. } => {
      let offset = func.locals[*obj].offset;
      asm.push_str(&format!("    mov -{offset}(%rbp), %rax\n"));
      asm.push_str("    push %rax\n");
    }
    AstNode::Binary { op, lhs, rhs, .. } => {
      emit_expr(lhs, func, asm, return_label, cg);
      emit_expr(rhs, func, asm, return_label, cg);
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
      emit_addr(lhs, func, asm, return_label, cg);
      emit_expr(rhs, func, asm, return_label, cg);
      asm.push_str("    pop %rdi\n");
      asm.push_str("    pop %rax\n");
      asm.push_str("    mov %rdi, (%rax)\n");
      asm.push_str("    push %rdi\n");
    }
    AstNode::Block { body, .. } => {
      emit_stmt_list(body.as_deref(), func, asm, true, return_label, cg);
    }
    AstNode::Addr { operand, .. } => {
      emit_addr(operand, func, asm, return_label, cg);
    }
    AstNode::Deref { operand, .. } => {
      emit_expr(operand, func, asm, return_label, cg);
      asm.push_str("    pop %rax\n");
      asm.push_str("    mov (%rax), %rax\n");
      asm.push_str("    push %rax\n");
    }
    AstNode::Call { name, args, .. } => {
      debug_assert!(
        args.len() <= ARG_REGISTERS.len(),
        "codegen received call with too many arguments"
      );
      for arg in args.iter() {
        emit_expr(arg, func, asm, return_label, cg);
      }
      for reg in ARG_REGISTERS.iter().take(args.len()).rev() {
        asm.push_str(&format!("    pop {}\n", reg));
      }
      asm.push_str("    mov $0, %rax\n");
      asm.push_str(&format!("    call {name}\n"));
      asm.push_str("    push %rax\n");
    }
    AstNode::Return { value, .. } => {
      emit_expr(value, func, asm, return_label, cg);
      asm.push_str("    pop %rax\n");
      asm.push_str(&format!("    jmp {return_label}\n"));
    }
    AstNode::If {
      cond,
      then_branch,
      else_branch,
      ..
    } => {
      emit_expr(cond, func, asm, return_label, cg);
      asm.push_str("    pop %rax\n");
      asm.push_str("    cmp $0, %rax\n");

      let else_label = else_branch.as_ref().map(|_| cg.new_label());
      let end_label = cg.new_label();
      match else_label {
        Some(label) => asm.push_str(&format!("    je .L.else.{label}\n")),
        None => asm.push_str(&format!("    je .L.end.{end_label}\n")),
      }

      emit_stmt_list(
        Some(then_branch.as_ref()),
        func,
        asm,
        false,
        return_label,
        cg,
      );
      if else_branch.is_some() {
        asm.push_str(&format!("    jmp .L.end.{end_label}\n"));
      }

      if let Some(label) = else_label {
        asm.push_str(&format!(".L.else.{label}:\n"));
        emit_stmt_list(else_branch.as_deref(), func, asm, false, return_label, cg);
      }

      asm.push_str(&format!(".L.end.{end_label}:\n"));
    }
    AstNode::For {
      init,
      cond,
      inc,
      body,
      ..
    } => {
      if let Some(init) = init {
        emit_expr(init, func, asm, return_label, cg);
        asm.push_str("    pop %rax\n");
      }

      let begin_label = cg.new_label();
      let end_label = cg.new_label();
      let cont_label = cg.new_label();

      asm.push_str(&format!(".L.begin.{begin_label}:\n"));
      if let Some(cond) = cond {
        emit_expr(cond, func, asm, return_label, cg);
        asm.push_str("    pop %rax\n");
        asm.push_str("    cmp $0, %rax\n");
        asm.push_str(&format!("    je .L.end.{end_label}\n"));
      }

      emit_stmt_list(Some(body.as_ref()), func, asm, false, return_label, cg);
      asm.push_str(&format!(".L.continue.{cont_label}:\n"));

      if let Some(inc) = inc {
        emit_expr(inc, func, asm, return_label, cg);
        asm.push_str("    pop %rax\n");
      }

      asm.push_str(&format!("    jmp .L.begin.{begin_label}\n"));
      asm.push_str(&format!(".L.end.{end_label}:\n"));
    }
    AstNode::Neg { operand, .. } => {
      emit_expr(operand, func, asm, return_label, cg);
      asm.push_str("    pop %rax\n");
      asm.push_str("    neg %rax\n");
      asm.push_str("    push %rax\n");
    }
  }
}

fn emit_addr(
  node: &AstNode,
  func: &Function,
  asm: &mut String,
  return_label: &str,
  cg: &mut Codegen,
) {
  match node {
    AstNode::Var { obj, .. } => {
      let offset = func.locals[*obj].offset;
      asm.push_str(&format!("    lea -{offset}(%rbp), %rax\n"));
      asm.push_str("    push %rax\n");
    }
    AstNode::Deref { operand, .. } => {
      emit_expr(operand, func, asm, return_label, cg);
    }
    _ => panic!("not an lvalue"),
  }
}
