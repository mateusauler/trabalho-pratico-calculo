use crate::lexer::Lexer;

pub struct Parser {
    lexer: Lexer
}

impl Parser {
    pub fn new<T: ToString + ?Sized>(texto: &T) -> Self {
        Self {
            lexer: Lexer::new(texto)
        }
    }
}
