use std::collections::HashMap;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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
		use TipoToken::*;

		let mut tokens_com_texto_definido = HashMap::new();

		tokens_com_texto_definido.insert("(".to_string(), AbreParenteses);
		tokens_com_texto_definido.insert(")".to_string(), FechaParenteses);
		tokens_com_texto_definido.insert("+".to_string(), Mais);
		tokens_com_texto_definido.insert("-".to_string(), Menos);
		tokens_com_texto_definido.insert("*".to_string(), Asterisco);
		tokens_com_texto_definido.insert("/".to_string(), Barra);
		tokens_com_texto_definido.insert("^".to_string(), Potencia);
		tokens_com_texto_definido.insert(",".to_string(), Virgula);
		tokens_com_texto_definido.insert("int".to_string(), Integral); //
		tokens_com_texto_definido.insert("raiz".to_string(), Raiz); //
		tokens_com_texto_definido.insert("log".to_string(), Log);
		tokens_com_texto_definido.insert("ln".to_string(), LogNatural);
		tokens_com_texto_definido.insert("sen".to_string(), Seno);
		tokens_com_texto_definido.insert("cos".to_string(), Cosseno);
		tokens_com_texto_definido.insert("x".to_string(), X);
		tokens_com_texto_definido.insert("pi".to_string(), PI);
		tokens_com_texto_definido.insert("e".to_string(), E);

		Self {
			caracteres: texto.to_string().chars().collect(),
			caractere_atual: 0,
			tokens_com_texto_definido,
		}
	}

	pub fn proximo_token(&mut self) -> Token {
		use Estados::*;
		use TipoToken::*;
		let c = self.proximo_caractere();

		if c.is_none() {
			return Token {
				tipo: EOF,
				lexema: String::new(),
			};
		}

		let mut c = c.unwrap();

		let mut estado = Estados::Inicial;
		let mut tipo_token = None;
		let mut backtrack = false;
		let mut lexema = String::from(c);
		let mut fim = false;

		while !fim {
			match estado {
				Inicial => {
					if let Some(t) = self.tokens_com_texto_definido.get(&lexema) {
						tipo_token = Some(*t);
						fim = true;
					} else {
						match c {
							'0'..='9' => {
								estado = NumeroInteiro;
								tipo_token = Some(Numero);
								backtrack = true;
							}

							'a'..='z' => {
								estado = Identificador;
								backtrack = true;
							}

							' ' | '\n' | '\r' | '\t' => lexema.clear(),

							_ => Lexer::caractere_inesperado(c),
						}
					}
				}

				NumeroInteiro => match c {
					'0'..='9' => (),
					'.' => estado = Ponto,
					_ => fim = true,
				},

				Ponto => match c {
					'0'..='9' => estado = NumeroFracionario,
					_ => Lexer::token_nao_reconhecido(&lexema),
				},

				NumeroFracionario => match c {
					'0'..='9' => (),
					_ => fim = true,
				},

				Identificador => match c {
					'a'..='z' => (),
					_ => fim = true,
				},
			}
			if fim {
				if backtrack {
					lexema.pop();
					self.caractere_atual -= 1;
				}
			} else {
				match self.proximo_caractere() {
					Some(ch) => c = ch,
					None => {
						c = ' ';
						backtrack = true;
					}
				}
				lexema.push(c);
			}
		}

		if tipo_token.is_none() {
			tipo_token = self.tokens_com_texto_definido.get(&lexema).cloned();
			if tipo_token.is_none() {
				Lexer::token_nao_reconhecido(&lexema);
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

	fn token_nao_reconhecido(lexema: &String) {
		panic!("Token não reconhecido: \"{lexema}\"");
	}

	fn caractere_inesperado(caractere: char) {
		panic!("Caractere inesperado: '{caractere}'");
	}
}
