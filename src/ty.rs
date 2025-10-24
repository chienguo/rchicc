#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeKind {
  Int,
  Ptr,
}

impl TypeKind {
  pub fn is_integer(self) -> bool {
    matches!(self, TypeKind::Int)
  }
}

pub fn promote_ptr_arith(lhs: TypeKind, rhs: TypeKind) -> (TypeKind, TypeKind) {
  match (lhs, rhs) {
    (TypeKind::Ptr, TypeKind::Int) => (lhs, rhs),
    (TypeKind::Int, TypeKind::Ptr) => (TypeKind::Ptr, TypeKind::Int),
    _ => (lhs, rhs),
  }
}
