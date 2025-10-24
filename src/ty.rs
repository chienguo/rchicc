#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
  Int,
  Ptr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
  pub kind: TypeKind,
  pub base: Option<Box<Type>>,
  pub decl_token: Option<usize>,
}

impl Type {
  pub fn int() -> Self {
    Self {
      kind: TypeKind::Int,
      base: None,
      decl_token: None,
    }
  }

  pub fn pointer_to(base: Type) -> Self {
    Self {
      kind: TypeKind::Ptr,
      base: Some(Box::new(base)),
      decl_token: None,
    }
  }

  pub fn with_decl_token(mut self, token: usize) -> Self {
    self.decl_token = Some(token);
    self
  }

  pub fn is_integer(&self) -> bool {
    matches!(self.kind, TypeKind::Int)
  }

  pub fn is_pointer(&self) -> bool {
    matches!(self.kind, TypeKind::Ptr)
  }

  pub fn base(&self) -> Option<&Type> {
    self.base.as_deref()
  }

  pub fn size(&self) -> i64 {
    match self.kind {
      TypeKind::Int => 8,
      TypeKind::Ptr => 8,
    }
  }
}

pub fn pointer_to(base: Type) -> Type {
  Type::pointer_to(base)
}
