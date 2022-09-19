use std::collections::HashMap;
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
	Integral,   // int

	X,  // variável x
	PI, // pi
	E,  // e

	Numero, // número

	EOF, // fim de arquivo
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
			Integral => Some("int"),

			X => Some("x"),
			PI => Some("pi"),
			E => Some("e"),

			Numero => None,

			EOF => None,
		}
		.map(|s| s.to_string())
	}
}

#[derive(Debug)]
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

#[derive(PartialEq)]
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

	pub fn proximo_token(&mut self) -> Token {
		use Estados::*;
		use TipoToken::*;

		let mut c = self.proximo_caractere();

		if c.is_none() {
			return Token {
				tipo: EOF,
				lexema: String::new(),
			};
		}

		let mut estado = Estados::Inicial;
		let mut backtrack = false;
		let mut fim = false;

		let mut tipo_token = None;
		let mut lexema = String::from(c.unwrap());

		while !fim {
			match estado {
				Inicial => {
					if let Some(t) = self.tokens_com_texto_definido.get(&lexema) {
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

							Some(' ' | '\n' | '\r' | '\t') => lexema.clear(),

							Some(c) => caractere_inesperado(c),

							None => {
								tipo_token = Some(EOF);
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
					_ => token_nao_reconhecido(&lexema),
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
				if let Some(c) = c {
					lexema.push(c);
				}
			} else if backtrack {
				lexema.pop();
				self.caractere_atual -= 1;
			}
		}

		if tipo_token.is_none() {
			tipo_token = self.tokens_com_texto_definido.get(&lexema).cloned();
			if tipo_token.is_none() {
				token_nao_reconhecido(&lexema);
			}
		}

		Token {
			tipo: tipo_token.unwrap(),
			lexema,
		}
	}

	fn proximo_caractere(&mut self) -> Option<char> {
		let caractere = if self.caractere_atual >= self.caracteres.len() {
			None
		} else {
			Some(self.caracteres[self.caractere_atual])
		};
		self.caractere_atual += 1;
		return caractere;
	}
}

fn token_nao_reconhecido(lexema: &String) {
	panic!("Token não reconhecido: \"{lexema}\"");
}

fn caractere_inesperado(caractere: char) {
	panic!("Caractere inesperado: '{caractere}'");
}

#[test]
fn ignorar_espacos() {
	use TipoToken::*;
	let mut l = Lexer::new("  \t \n  \r  \r\n\t ");
	assert_eq!(l.proximo_token().tipo(), EOF);
}

#[test]
fn numeros() {
	use TipoToken::*;

	let mut l = Lexer::new("0 1 2.3 0.4 005 67.89");

	assert_eq!(l.proximo_token().tipo(), Numero);
	assert_eq!(l.proximo_token().tipo(), Numero);
	assert_eq!(l.proximo_token().tipo(), Numero);
	assert_eq!(l.proximo_token().tipo(), Numero);
	assert_eq!(l.proximo_token().tipo(), Numero);
	assert_eq!(l.proximo_token().tipo(), Numero);
	assert_eq!(l.proximo_token().tipo(), EOF);
}

#[test]
fn operadores_basicos() {
	use TipoToken::*;

	let mut l = Lexer::new("1 + 2 - 3 * 4 / 5 ^ 6");

	assert_eq!(l.proximo_token().tipo(), Numero);
	assert_eq!(l.proximo_token().tipo(), Mais);
	assert_eq!(l.proximo_token().tipo(), Numero);
	assert_eq!(l.proximo_token().tipo(), Menos);
	assert_eq!(l.proximo_token().tipo(), Numero);
	assert_eq!(l.proximo_token().tipo(), Asterisco);
	assert_eq!(l.proximo_token().tipo(), Numero);
	assert_eq!(l.proximo_token().tipo(), Barra);
	assert_eq!(l.proximo_token().tipo(), Numero);
	assert_eq!(l.proximo_token().tipo(), Potencia);
	assert_eq!(l.proximo_token().tipo(), Numero);
	assert_eq!(l.proximo_token().tipo(), EOF);
}
