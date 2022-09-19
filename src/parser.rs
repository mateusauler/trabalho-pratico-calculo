use std::{
	f64::consts::{E, PI},
	fmt::Debug,
};

use crate::lexer::{
	Lexer,
	TipoToken::{self, *},
	Token,
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
			Producao::ExpRaiz(indice, radicando) => radicando
				.calcular_valor(x)
				.powf(1.0 / indice.calcular_valor(x)),
			Producao::ExpIntegral(_, _, _) => todo!(),
			Producao::Final(t) => t.calcular_valor(x),
		}
	}
}

pub struct Parser {
	lexer: Lexer,
	proximo_token: Token,
}

impl Parser {
	pub fn new<T: ToString + ?Sized>(texto: &T) -> Self {
		let mut lexer = Lexer::new(texto);
		let proximo_token = (&lexer.proximo_token()).to_owned();
		Self {
			lexer,
			proximo_token,
		}
	}

	pub fn parse(&mut self) -> Producao {
		let ast = match self.proximo_token.tipo() {
			FechaParenteses | Asterisco | Barra | Potencia | Virgula | EOF => {
				self.token_inesperado();
				unreachable!();
			}
			_ => self.exp(),
		};
		self.consome_token(EOF);
		ast
	}

	fn consome_token(&mut self, token_esperado: TipoToken) -> Producao {
		if self.proximo_token.tipo() != token_esperado {
			self.token_inesperado();
		}

		let ast = Producao::Final(self.proximo_token.clone());
		self.proximo_token = self.lexer.proximo_token();
		ast
	}

	fn exp(&mut self) -> Producao {
		let mut esq = self.exp_mul();

		let mut tipo = self.proximo_token.tipo();
		while let Mais | Menos = tipo {
			self.consome_token(tipo);
			let dir = self.exp_mul();
			esq = Producao::ExpBinaria(Box::from(esq), tipo, Box::from(dir));
			tipo = self.proximo_token.tipo();
		}

		esq
	}

	fn exp_mul(&mut self) -> Producao {
		let mut esq = self.exp_pot();

		let mut tipo = self.proximo_token.tipo();
		while let Asterisco | Barra = tipo {
			self.consome_token(tipo);
			let dir = self.exp_pot();
			esq = Producao::ExpBinaria(Box::from(esq), tipo, Box::from(dir));
			tipo = self.proximo_token.tipo();
		}

		esq
	}

	fn exp_pot(&mut self) -> Producao {
		let base = self.exp_final();

		if self.proximo_token.tipo() == Potencia {
			self.consome_token(Potencia);
			let expoente = self.exp_pot();
			Producao::ExpBinaria(Box::from(base), Potencia, Box::from(expoente))
		} else {
			base
		}
	}

	fn exp_final(&mut self) -> Producao {
		let token = &self.proximo_token;
		match token.tipo() {
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
			Seno | Cosseno => self.trig(),
			Integral => self.integral(),

			FechaParenteses | Asterisco | Barra | Potencia | Virgula => {
				self.token_inesperado();
				unreachable!();
			}
			EOF => todo!(),
		}
	}

	fn exp_parenteses(&mut self) -> Producao {
		self.consome_token(AbreParenteses);
		let exp = self.exp();
		self.consome_token(FechaParenteses);
		exp
	}

	fn exp_unaria(&mut self) -> Producao {
		let operador = self.proximo_token.tipo();
		match operador {
			Mais => self.consome_token(Mais),
			_ => self.consome_token(Menos),
		};

		let operando = self.exp_final();
		Producao::ExpUnaria(operador, Box::from(operando))
	}

	fn log_natural(&mut self) -> Producao {
		self.consome_token(LogNatural);
		self.consome_token(AbreParenteses);
		let logaritmando = self.exp();
		self.consome_token(FechaParenteses);

		Producao::ExpLogNatural(Box::from(logaritmando))
	}

	fn log(&mut self) -> Producao {
		self.consome_token(Log);
		self.consome_token(AbreParenteses);
		let base = self.exp();
		self.consome_token(Virgula);
		let logaritmando = self.exp();
		self.consome_token(FechaParenteses);

		Producao::ExpLog(Box::from(base), Box::from(logaritmando))
	}

	fn trig(&mut self) -> Producao {
		let operacao = self.proximo_token.tipo();
		match operacao {
			Seno => self.consome_token(Seno),
			_ => self.consome_token(Cosseno),
		};

		self.consome_token(AbreParenteses);
		let argumento = self.exp();
		self.consome_token(FechaParenteses);

		match operacao {
			Seno => Producao::ExpSeno(Box::from(argumento)),
			Cosseno => Producao::ExpCosseno(Box::from(argumento)),
			_ => unreachable!(),
		}
	}

	fn raiz(&mut self) -> Producao {
		self.consome_token(Raiz);
		self.consome_token(AbreParenteses);
		let indice = self.exp();
		self.consome_token(Virgula);
		let radicando = self.exp();
		self.consome_token(FechaParenteses);

		Producao::ExpRaiz(Box::from(indice), Box::from(radicando))
	}

	fn integral(&mut self) -> Producao {
		self.consome_token(Integral);
		self.consome_token(AbreParenteses);
		let limite_inferior = Box::from(self.exp());
		self.consome_token(Virgula);
		let limite_superior = Box::from(self.exp());
		self.consome_token(Virgula);
		let funcao = Box::from(self.exp());

		Producao::ExpIntegral(limite_inferior, limite_superior, funcao)
	}

	fn token_inesperado(&self) {
		panic!("Token '{}' inesperado.", self.proximo_token.lexema());
	}
}
