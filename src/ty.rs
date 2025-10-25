#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
  Int,
  Ptr,
  Array,
  Func,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
  pub kind: TypeKind,
  pub base: Option<Box<Type>>,
  pub ret: Option<Box<Type>>,
  pub array_len: Option<usize>,
  pub size: i64,
  pub decl_token: Option<usize>,
}

impl Type {
  pub fn int() -> Self {
    Self {
      kind: TypeKind::Int,
      base: None,
      ret: None,
      array_len: None,
      size: 8,
      decl_token: None,
    }
  }

  pub fn pointer_to(base: Type) -> Self {
    Self {
      kind: TypeKind::Ptr,
      base: Some(Box::new(base)),
      ret: None,
      array_len: None,
      size: 8,
      decl_token: None,
    }
  }

  pub fn array_of(base: Type, len: usize) -> Self {
    let size = base.size() * len as i64;
    Self {
      kind: TypeKind::Array,
      base: Some(Box::new(base)),
      ret: None,
      array_len: Some(len),
      size,
      decl_token: None,
    }
  }

  pub fn func(ret: Type) -> Self {
    Self {
      kind: TypeKind::Func,
      base: None,
      ret: Some(Box::new(ret)),
      array_len: None,
      size: 8,
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

  pub fn is_array(&self) -> bool {
    matches!(self.kind, TypeKind::Array)
  }

  pub fn is_pointer_like(&self) -> bool {
    self.is_pointer() || self.is_array()
  }

  pub fn is_function(&self) -> bool {
    matches!(self.kind, TypeKind::Func)
  }

  pub fn base(&self) -> Option<&Type> {
    self.base.as_deref()
  }

  pub fn return_type(&self) -> Option<&Type> {
    self.ret.as_deref()
  }

  pub fn size(&self) -> i64 {
    self.size
  }

  pub fn array_len(&self) -> Option<usize> {
    self.array_len
  }

  pub fn decay(&self) -> Type {
    if self.is_array() {
      self
        .base()
        .map(|t| Type::pointer_to(t.clone()))
        .unwrap_or_else(Type::int)
    } else {
      self.clone()
    }
  }
}

pub fn pointer_to(base: Type) -> Type {
  Type::pointer_to(base)
}

pub fn func(ret: Type) -> Type {
  Type::func(ret)
}

pub fn array_of(base: Type, len: usize) -> Type {
  Type::array_of(base, len)
}
