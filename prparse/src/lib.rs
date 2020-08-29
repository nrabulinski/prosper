#![feature(or_patterns, type_alias_impl_trait, drain_filter, box_patterns)]
use std::borrow::Cow;
use std::ops::{Deref, DerefMut};

pub mod lexer;
pub mod token;
pub mod span;
pub mod ast;
pub mod parse;
pub mod validate;
pub mod codegen;

use span::Span;

#[derive(Debug)]
pub struct Message {
    pub message: Cow<'static, str>,
    pub span: Span,
}

impl Message {
    pub fn new<M: Into<Cow<'static, str>>>(msg: M, span: Span) -> Self {
        let message = msg.into();
        Message { message, span }
    }
}

#[derive(Debug)]
pub struct CResult<T> {
    output: T,
    warnings: Vec<Message>,
    errors: Vec<Message>,
}

impl<T> CResult<T> {
    pub fn new(output: T) -> Self {
        CResult { output, warnings: Vec::new(), errors: Vec::new() }
    }

    pub fn with_messages(output: T, warnings: Vec<Message>, errors: Vec<Message>) -> Self {
        CResult { output, warnings, errors }
    }

    pub fn warn<M: Into<Cow<'static, str>>>(&mut self, msg: M, span: Span) {
        let message = msg.into();
        self.warnings.push(Message { message, span })
    }

    pub fn err<M: Into<Cow<'static, str>>>(mut self, msg: M, span: Span) -> Self {
        let message = msg.into();
        self.errors.push(Message { message, span });
        self
    }

    pub fn prepend<U>(output: T, other: CResult<U>) -> Self {
        CResult {
            output,
            warnings: other.warnings,
            errors: other.errors
        }
    }

    pub fn into(self) -> T {
        self.output
    }

    pub fn join<U>(&mut self, mut other: CResult<U>) -> U {
        self.warnings.append(&mut other.warnings);
        self.errors.append(&mut other.errors);
        other.output
    }

    pub fn warns(&self) -> &[Message] { &self.warnings }
    pub fn errs(&self) -> &[Message] { &self.errors }

    pub fn errored(&self) -> bool { self.errors.len() > 0 }
}

impl<T> CResult<Option<T>> {
    pub fn unwrap(self) -> CResult<T> {
        CResult {
            output: self.output.unwrap(),
            warnings: self.warnings,
            errors: self.errors
        }
    }

    pub fn set(self, output: T) -> CResult<T> {
        CResult {
            output,
            warnings: self.warnings,
            errors: self.errors,
        }
    }

    pub fn new_none() -> Self {
        CResult {
            output: None,
            warnings: Vec::new(),
            errors: Vec::new()
        }
    }
}

impl<T: Default> Default for CResult<T> {
    fn default() -> Self {
        CResult::new(Default::default())
    }
}

impl<T> Deref for CResult<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.output
    }
}

impl<T> DerefMut for CResult<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.output
    }
}
