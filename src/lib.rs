use std::fmt::{Debug, Display};

mod lexer;
pub mod parser;

pub trait ErroExpr: Debug + Display {}
