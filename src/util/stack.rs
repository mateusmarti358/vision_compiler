use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Stack<T> {
  stack: VecDeque<T>
}
impl<T> Stack<T> {
  pub fn new() -> Self {
    Self { stack: VecDeque::new() }
  }

  pub fn push(&mut self, t: T) {
    self.stack.push_back(t);
  }
  pub fn pop(&mut self) -> Option<T> {
    self.stack.pop_back()
  }

  pub fn len(&self) -> usize {
    self.stack.len()
  }
  pub fn _is_empty(&self) -> bool {
    self.stack.is_empty()
  }
}
