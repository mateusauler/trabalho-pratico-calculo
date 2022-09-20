use crate::{
	lexer::{
		Lexer,
		TipoToken::{self, *},
		Token,
	},
	ErroExpr,
};
use std::{
	f64::consts::{E, PI},
	fmt::{Debug, Display},
};

pub trait ElementoGramatica: Debug {
	fn calcular_valor(&self, x: Option<f64>) -> f64;
}

impl ElementoGramatica for Token {
	fn calcular_valor(&self, x: Option<f64>) -> f64 {
		if x.is_none() {
			unreachable!();
		}

		match self.tipo() {
			X => x.unwrap(),
			Theta => x.unwrap(),
			ConstPI => PI,
			ConstE => E,
			Numero => self.lexema().parse().unwrap(),
			_ => unreachable!(),
		}
	}
}

#[derive(Debug)]
pub enum Producao {
	ExpBinaria(
		Box<dyn ElementoGramatica>,
		TipoToken,
		Box<dyn ElementoGramatica>,
	),
	ExpUnaria(TipoToken, Box<dyn ElementoGramatica>),
	ExpLog(Box<dyn ElementoGramatica>, Box<dyn ElementoGramatica>),
	ExpLogNatural(Box<dyn ElementoGramatica>),
	ExpSeno(Box<dyn ElementoGramatica>),
	ExpCosseno(Box<dyn ElementoGramatica>),
	ExpTangente(Box<dyn ElementoGramatica>),
	ExpRaiz(Box<dyn ElementoGramatica>, Box<dyn ElementoGramatica>),
	ExpIntegral(
		Box<dyn ElementoGramatica>,
		Box<dyn ElementoGramatica>,
		Box<dyn ElementoGramatica>,
	),
	Final(Token),
}

impl ElementoGramatica for Producao {
	fn calcular_valor(&self, x: Option<f64>) -> f64 {
		match self {
			Producao::ExpBinaria(esq, operador, dir) => {
				let esq = esq.calcular_valor(x);
				let dir = dir.calcular_valor(x);

				match operador {
					Mais => esq + dir,
					Menos => esq - dir,
					Asterisco => esq * dir,
					Barra => esq / dir,
					Potencia => esq.powf(dir),
					_ => unreachable!(),
				}
			}
			Producao::ExpUnaria(operador, operando) => {
				let operando = operando.calcular_valor(x);

				match operador {
					Mais => operando,
					Menos => -operando,
					_ => unreachable!(),
				}
			}
			Producao::ExpLog(base, logaritmando) => {
				logaritmando.calcular_valor(x).log(base.calcular_valor(x))
			}
			Producao::ExpLogNatural(logaritmando) => logaritmando.calcular_valor(x).ln(),
			Producao::ExpSeno(v) => v.calcular_valor(x).sin(),
			Producao::ExpCosseno(v) => v.calcular_valor(x).cos(),
			Producao::ExpTangente(v) => v.calcular_valor(x).tan(),
			Producao::ExpRaiz(indice, radicando) => radicando
				.calcular_valor(x)
				.powf(1.0 / indice.calcular_valor(x)),
			Producao::ExpIntegral(_, _, _) => todo!(),
			Producao::Final(t) => t.calcular_valor(x),
		}
	}
}

#[derive(Debug)]
enum ErroParser {
	TokenInesperado { lex: String },
	FinalInesperado,
}

impl ErroExpr for ErroParser {}

impl Display for ErroParser {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ErroParser::TokenInesperado { lex } => write!(f, "Token inesperado: \"{lex}\"."),
			ErroParser::FinalInesperado => write!(f, "Final inesperado."),
		}
	}
}

pub struct Parser {
	lexer: Lexer,
	proximo_token: Token,
}

impl Parser {
	pub fn new<T: ToString + ?Sized>(texto: &T) -> Result<Self, Box<dyn ErroExpr>> {
		let mut lexer = Lexer::new(texto);
		let proximo_token = (&lexer.proximo_token()?).to_owned();
		Ok(Self {
			lexer,
			proximo_token,
		})
	}

	pub fn parse(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		let ast = match self.proximo_token.tipo() {
			FechaParenteses | Asterisco | Barra | Potencia | Virgula | EOF => {
				self.token_inesperado()?
			}
			_ => self.exp()?,
		};
		self.consome_token(EOF)?;
		Ok(ast)
	}

	fn consome_token(&mut self, token_esperado: TipoToken) -> Result<Producao, Box<dyn ErroExpr>> {
		if self.proximo_token.tipo() != token_esperado {
			self.token_inesperado()?;
		}

		let ast = Producao::Final(self.proximo_token.clone());
		self.proximo_token = self.lexer.proximo_token()?;
		Ok(ast)
	}

	fn exp(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		let mut esq = self.exp_mul()?;

		let mut tipo = self.proximo_token.tipo();
		while let Mais | Menos = tipo {
			self.consome_token(tipo)?;
			let dir = self.exp_mul()?;
			esq = Producao::ExpBinaria(Box::from(esq), tipo, Box::from(dir));
			tipo = self.proximo_token.tipo();
		}

		Ok(esq)
	}

	fn exp_mul(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		let mut esq = self.exp_pot()?;

		let mut tipo = self.proximo_token.tipo();
		while let Asterisco | Barra = tipo {
			self.consome_token(tipo)?;
			let dir = self.exp_pot()?;
			esq = Producao::ExpBinaria(Box::from(esq), tipo, Box::from(dir));
			tipo = self.proximo_token.tipo();
		}

		Ok(esq)
	}

	fn exp_pot(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		let base = self.exp_final()?;

		if self.proximo_token.tipo() == Potencia {
			self.consome_token(Potencia)?;
			let expoente = self.exp_pot()?;
			Ok(Producao::ExpBinaria(
				Box::from(base),
				Potencia,
				Box::from(expoente),
			))
		} else {
			Ok(base)
		}
	}

	fn exp_final(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		match self.proximo_token.tipo() {
			X => self.consome_token(X),
			Theta => self.consome_token(Theta),
			ConstPI => self.consome_token(ConstPI),
			ConstE => self.consome_token(ConstE),
			Numero => self.consome_token(Numero),

			Mais | Menos => self.exp_unaria(),

			AbreParenteses => self.exp_parenteses(),

			Raiz => self.raiz(),
			Log => self.log(),
			LogNatural => self.log_natural(),
			Seno | Cosseno | Tangente => self.trig(),
			Integral => self.integral(),

			FechaParenteses | Asterisco | Barra | Potencia | Virgula => self.token_inesperado(),
			EOF => Parser::final_inesperado(),
		}
	}

	fn exp_parenteses(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		self.consome_token(AbreParenteses)?;
		let exp = self.exp()?;
		self.consome_token(FechaParenteses)?;
		Ok(exp)
	}

	fn exp_unaria(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		let operador = self.proximo_token.tipo();
		match operador {
			Mais => self.consome_token(Mais)?,
			AbreParenteses | FechaParenteses | Menos | Asterisco | Barra | Potencia | Virgula
			| Raiz | Log | LogNatural | Seno | Cosseno | Tangente | Integral | X | Theta
			| ConstPI | ConstE | Numero | EOF => self.consome_token(Menos)?,
		};

		let operando = self.exp_final()?;
		Ok(Producao::ExpUnaria(operador, Box::from(operando)))
	}

	fn log_natural(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		self.consome_token(LogNatural)?;
		self.consome_token(AbreParenteses)?;
		let logaritmando = self.exp()?;
		self.consome_token(FechaParenteses)?;

		Ok(Producao::ExpLogNatural(Box::from(logaritmando)))
	}

	fn log(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		self.consome_token(Log)?;
		self.consome_token(AbreParenteses)?;
		let base = self.exp()?;
		self.consome_token(Virgula)?;
		let logaritmando = self.exp()?;
		self.consome_token(FechaParenteses)?;

		Ok(Producao::ExpLog(Box::from(base), Box::from(logaritmando)))
	}

	fn trig(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		let operacao = self.proximo_token.tipo();
		match operacao {
			Seno => self.consome_token(Seno)?,
			Cosseno => self.consome_token(Cosseno)?,
			Tangente | AbreParenteses | FechaParenteses | Mais | Menos | Asterisco | Barra
			| Potencia | Virgula | Raiz | Log | LogNatural | Integral | X | Theta | ConstPI
			| ConstE | Numero | EOF => self.consome_token(Tangente)?,
		};

		self.consome_token(AbreParenteses)?;
		let argumento = self.exp()?;
		self.consome_token(FechaParenteses)?;

		match operacao {
			Seno => Ok(Producao::ExpSeno(Box::from(argumento))),
			Cosseno => Ok(Producao::ExpCosseno(Box::from(argumento))),
			Tangente => Ok(Producao::ExpTangente(Box::from(argumento))),

			AbreParenteses | FechaParenteses | Mais | Menos | Asterisco | Barra | Potencia
			| Virgula | Raiz | Log | LogNatural | Integral | X | Theta | ConstPI | ConstE
			| Numero | EOF => unreachable!(),
		}
	}

	fn raiz(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		self.consome_token(Raiz)?;
		self.consome_token(AbreParenteses)?;
		let indice = self.exp()?;
		self.consome_token(Virgula)?;
		let radicando = self.exp()?;
		self.consome_token(FechaParenteses)?;

		Ok(Producao::ExpRaiz(Box::from(indice), Box::from(radicando)))
	}

	fn integral(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		self.consome_token(Integral)?;
		self.consome_token(AbreParenteses)?;
		let limite_inferior = Box::from(self.exp()?);
		self.consome_token(Virgula)?;
		let limite_superior = Box::from(self.exp()?);
		self.consome_token(Virgula)?;
		let funcao = Box::from(self.exp()?);

		Ok(Producao::ExpIntegral(
			limite_inferior,
			limite_superior,
			funcao,
		))
	}

	fn token_inesperado(&self) -> Result<Producao, Box<dyn ErroExpr>> {
		Err(Box::new(ErroParser::TokenInesperado {
			lex: self.proximo_token.lexema().to_owned(),
		}))
	}

	fn final_inesperado() -> Result<Producao, Box<dyn ErroExpr>> {
		Err(Box::new(ErroParser::FinalInesperado))
	}
}
