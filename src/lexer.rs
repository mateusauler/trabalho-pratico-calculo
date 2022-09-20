use crate::ErroExpr;
use std::{collections::HashMap, fmt::Display};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Clone, Copy, Debug, EnumIter, Eq, PartialEq)]
pub enum TipoToken {
	AbreParenteses,  // (
	FechaParenteses, // )
	Mais,            // +
	Menos,           // -
	Asterisco,       // *
	Barra,           // /
	Potencia,        // ^
	Virgula,         // ,

	Raiz,       // raiz
	Log,        // log
	LogNatural, // ln
	Seno,       // sen
	Cosseno,    // cos
	Tangente,   // tan
	Integral,   // int

	X,       // variável x
	Theta,   // θ
	ConstPI, // pi
	ConstE,  // e

	Numero, // número

	Eof, // fim de arquivo
}

impl TipoToken {
	fn lexema_token(&self) -> Option<String> {
		use TipoToken::*;
		match self {
			AbreParenteses => Some("("),
			FechaParenteses => Some(")"),
			Mais => Some("+"),
			Menos => Some("-"),
			Asterisco => Some("*"),
			Barra => Some("/"),
			Potencia => Some("^"),
			Virgula => Some(","),

			Raiz => Some("raiz"),
			Log => Some("log"),
			LogNatural => Some("ln"),
			Seno => Some("sen"),
			Cosseno => Some("cos"),
			Tangente => Some("tan"),
			Integral => Some("int"),

			X => Some("x"),
			Theta => Some("theta"),
			ConstPI => Some("pi"),
			ConstE => Some("e"),

			Numero => None,

			Eof => None,
		}
		.map(|s| s.to_string())
	}
}

#[derive(Clone, Debug)]
pub struct Token {
	tipo: TipoToken,
	lexema: String,
}

impl Token {
	pub fn tipo(&self) -> TipoToken {
		self.tipo
	}

	pub fn lexema(&self) -> &String {
		&self.lexema
	}
}

#[derive(Debug)]
pub enum ErroLex {
	CaractereInesperado { c: char },
	LexemaNaoReconhecido { lex: String },
}

impl ErroExpr for ErroLex {}

impl Display for ErroLex {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ErroLex::CaractereInesperado { c } => write!(f, "Caractere inesperado: '{c}'."),
			ErroLex::LexemaNaoReconhecido { lex } => write!(f, "Token não reconhecido: \"{lex}\"."),
		}
	}
}

enum Estados {
	Inicial,

	NumeroInteiro,
	Ponto,
	NumeroFracionario,

	Identificador,
}

pub struct Lexer {
	caracteres: Vec<char>,
	caractere_atual: usize,
	tokens_com_texto_definido: HashMap<String, TipoToken>,
}

impl Lexer {
	pub fn new<T: ToString + ?Sized>(texto: &T) -> Self {
		let mut tokens_com_texto_definido = HashMap::new();

		for tipo in TipoToken::iter() {
			if let Some(lexema) = tipo.lexema_token() {
				tokens_com_texto_definido.insert(lexema, tipo);
			}
		}

		Self {
			caracteres: texto.to_string().chars().collect(),
			caractere_atual: 0,
			tokens_com_texto_definido,
		}
	}

	pub fn proximo_token(&mut self) -> Result<Token, Box<dyn ErroExpr>> {
		use Estados::*;
		use TipoToken::*;

		let mut c = self.proximo_caractere();

		if c.is_none() {
			return Ok(Token {
				tipo: Eof,
				lexema: String::new(),
			});
		}

		let mut estado = Estados::Inicial;
		let mut backtrack = false;
		let mut fim = false;

		let mut tipo_token = None;
		let mut inicio_lex = self.caractere_atual - 1;

		while !fim {
			match estado {
				Inicial => {
					if let Some(t) = self
						.tokens_com_texto_definido
						.get(&self.get_lexema(inicio_lex))
					{
						tipo_token = Some(*t);
						fim = true;
					} else {
						match c {
							Some('0'..='9') => {
								estado = NumeroInteiro;
								tipo_token = Some(Numero);
								backtrack = true;
							}

							Some('a'..='z') => {
								estado = Identificador;
								backtrack = true;
							}

							Some(' ' | '\n' | '\r' | '\t') => inicio_lex = self.caractere_atual,

							Some(c) => return caractere_inesperado(c),

							None => {
								tipo_token = Some(Eof);
								fim = true;
							}
						}
					}
				}

				NumeroInteiro => match c {
					Some('0'..='9') => (),
					Some('.') => estado = Ponto,
					_ => fim = true,
				},

				Ponto => match c {
					Some('0'..='9') => estado = NumeroFracionario,
					_ => return token_nao_reconhecido(self.get_lexema(inicio_lex)),
				},

				NumeroFracionario => match c {
					Some('0'..='9') => (),
					_ => fim = true,
				},

				Identificador => match c {
					Some('a'..='z') => (),
					_ => fim = true,
				},
			}

			if !fim {
				c = self.proximo_caractere();
			} else if backtrack {
				self.caractere_atual -= 1;
			}
		}

		let lexema = self.get_lexema(inicio_lex);
		match tipo_token.or_else(|| self.tokens_com_texto_definido.get(&lexema).cloned()) {
			Some(tipo) => Ok(Token { tipo, lexema }),
			None => token_nao_reconhecido(lexema),
		}
	}

	fn proximo_caractere(&mut self) -> Option<char> {
		let caractere = match self.caractere_atual < self.caracteres.len() {
			true => Some(self.caracteres[self.caractere_atual]),
			false => None,
		};
		self.caractere_atual += 1;
		caractere
	}

	fn get_lexema(&self, inicio: usize) -> String {
		let fim = self.caractere_atual - (self.caractere_atual > self.caracteres.len()) as usize;
		self.caracteres[inicio..fim].iter().collect()
	}
}

fn token_nao_reconhecido(lexema: String) -> Result<Token, Box<dyn ErroExpr>> {
	Err(Box::new(ErroLex::LexemaNaoReconhecido { lex: lexema }))
}

fn caractere_inesperado(caractere: char) -> Result<Token, Box<dyn ErroExpr>> {
	Err(Box::new(ErroLex::CaractereInesperado { c: caractere }))
}

#[cfg(test)]
mod testes {
	use super::{
		Lexer,
		TipoToken::{self, *},
	};

	#[test]
	fn ignorar_espacos() {
		let mut l = Lexer::new("  \t \n  \r  \r\n\t ");
		test_token(&mut l, Eof, None)
	}

	#[test]
	fn numeros() {
		let mut l = Lexer::new("0 1 2.3 0.4 005 67.89");

		test_token(&mut l, Numero, Some("0"));
		test_token(&mut l, Numero, Some("1"));
		test_token(&mut l, Numero, Some("2.3"));
		test_token(&mut l, Numero, Some("0.4"));
		test_token(&mut l, Numero, Some("005"));
		test_token(&mut l, Numero, Some("67.89"));
		test_token(&mut l, Eof, None);
	}

	#[test]
	fn operadores_basicos() {
		let mut l = Lexer::new("1 + 2 - 3 * 4 / 5 ^ 6");

		test_token(&mut l, Numero, Some("1"));
		test_token(&mut l, Mais, Some("+"));
		test_token(&mut l, Numero, Some("2"));
		test_token(&mut l, Menos, Some("-"));
		test_token(&mut l, Numero, Some("3"));
		test_token(&mut l, Asterisco, Some("*"));
		test_token(&mut l, Numero, Some("4"));
		test_token(&mut l, Barra, Some("/"));
		test_token(&mut l, Numero, Some("5"));
		test_token(&mut l, Potencia, Some("^"));
		test_token(&mut l, Numero, Some("6"));
		test_token(&mut l, Eof, None);
	}

	fn test_token(l: &mut Lexer, tipo: TipoToken, lexema: Option<&str>) {
		let token = l.proximo_token().unwrap();
		assert_eq!(token.tipo(), tipo);
		if let Some(l) = lexema {
			assert_eq!(token.lexema(), l);
		}
	}
}
