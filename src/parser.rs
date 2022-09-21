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
	rc::Rc,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Variavel {
	X,
	Theta,
}

#[derive(Debug, Copy, Clone)]
pub struct Propriedades {
	variavel: Option<Variavel>,
	valor: Option<f64>,
}

#[derive(Debug, Clone)]
pub enum TipoProducao {
	ExpBinaria {
		esq: Rc<Producao>,
		op: TipoToken,
		dir: Rc<Producao>,
	},

	ExpUnaria {
		operador: TipoToken,
		operando: Rc<Producao>,
	},

	ExpLog {
		base: Rc<Producao>,
		logaritmando: Rc<Producao>,
	},

	ExpLogNatural(Rc<Producao>),
	ExpSeno(Rc<Producao>),
	ExpCosseno(Rc<Producao>),
	ExpTangente(Rc<Producao>),

	ExpRaiz {
		indice: Rc<Producao>,
		radicando: Rc<Producao>,
	},

	ExpIntegral {
		inf: Rc<Producao>,
		sup: Rc<Producao>,
		fun: Rc<Producao>,
	},

	Final(Token),
}

#[derive(Debug, Clone)]
pub struct Producao {
	tipo: TipoProducao,
	prop: Propriedades,
}

impl Producao {
	fn new(tipo: TipoProducao, prop: Option<Propriedades>) -> Self {
		let variavel = match &tipo {
			TipoProducao::Final(t) => match t.tipo() {
				X => Some(Variavel::X),
				Theta => Some(Variavel::Theta),
				_ => None,
			},
			_ => prop.map(|p| p.variavel).flatten(),
		};

		let prop = Propriedades {
			variavel,
			valor: None,
		};

		Self { tipo, prop }
	}

	pub fn calcular_valor(&mut self, x: Option<f64>) -> Option<f64> {
		if self.prop.valor.is_some() {
			return self.prop.valor;
		}

		let calc_valor =
			|v: &mut Rc<Producao>| Rc::get_mut(v).map(|v| v.calcular_valor(x)).flatten();

		let valor =
			match &mut self.tipo {
				TipoProducao::ExpBinaria { esq, op, dir } => calc_valor(esq)
					.zip(calc_valor(dir))
					.map(|(esq, dir)| match op {
						Mais => esq + dir,
						Menos => esq - dir,
						Asterisco => esq * dir,
						Barra => esq / dir,
						Potencia => esq.powf(dir),
						_ => unreachable!(),
					}),

				TipoProducao::ExpUnaria { operador, operando } => {
					calc_valor(operando).map(|o| match operador {
						Mais => o,
						Menos => -o,
						_ => unreachable!(),
					})
				}

				TipoProducao::ExpLog { base, logaritmando } => calc_valor(logaritmando)
					.zip(calc_valor(base))
					.map(|(l, b)| l.log(b)),

				TipoProducao::ExpLogNatural(logaritmando) => calc_valor(logaritmando).map(f64::ln),
				TipoProducao::ExpSeno(v) => calc_valor(v).map(f64::sin),
				TipoProducao::ExpCosseno(v) => calc_valor(v).map(f64::cos),
				TipoProducao::ExpTangente(v) => calc_valor(v).map(f64::tan),

				TipoProducao::ExpRaiz { indice, radicando } => calc_valor(radicando)
					.zip(calc_valor(indice))
					.map(|(r, i)| r.powf(1.0 / i)),

				TipoProducao::ExpIntegral {
					inf: _,
					sup: _,
					fun: _,
				} => todo!(),

				TipoProducao::Final(t) => match t.tipo() {
					X => x,
					Theta => x,
					ConstPI => Some(PI),
					ConstE => Some(E),
					Numero => Some(t.lexema().parse().unwrap()),
					_ => unreachable!(),
				},
			};

		if self.prop.variavel.is_none() {
			self.prop.valor = valor;
		}

		valor
	}
}

#[derive(Debug)]
enum ErroParser {
	TokenInesperado { lex: String },
	FinalInesperado,
	SomenteUmaVariavelPermitida,
	LimitesIntegralComVariavel,
}

impl ErroExpr for ErroParser {}

impl Display for ErroParser {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ErroParser::TokenInesperado { lex } => write!(f, "Token inesperado: \"{lex}\"."),
			ErroParser::FinalInesperado => write!(f, "Final inesperado."),
			ErroParser::SomenteUmaVariavelPermitida => {
				write!(f, "Somente uma variável é permitida em uma expressão.")
			}
			ErroParser::LimitesIntegralComVariavel => write!(f, "As expressões dos limites de uma integral não podem incluir a variável de integração."),
		}
	}
}

pub struct Parser {
	lexer: Lexer,
	proximo_token: Token,
	precisao: u32,
}

impl Parser {
	pub fn new<T: ToString + ?Sized>(texto: &T, precisao: u32) -> Result<Self, Box<dyn ErroExpr>> {
		let mut lexer = Lexer::new(texto);
		let proximo_token = lexer.proximo_token()?;
		Ok(Self {
			lexer,
			proximo_token,
			precisao,
		})
	}

	pub fn parse(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		let ast = match self.proximo_token.tipo() {
			FechaParenteses | Asterisco | Barra | Potencia | Virgula | Eof => {
				self.token_inesperado()?
			}
			_ => self.exp()?,
		};
		self.consome_token(Eof)?;
		Ok(ast)
	}

	fn consome_token(&mut self, token_esperado: TipoToken) -> Result<Producao, Box<dyn ErroExpr>> {
		if self.proximo_token.tipo() != token_esperado {
			self.token_inesperado()?;
		}

		let ast = Producao::new(TipoProducao::Final(self.proximo_token.clone()), None);
		self.proximo_token = self.lexer.proximo_token()?;
		Ok(ast)
	}

	fn merge_props(elementos: Vec<&Producao>) -> Result<Propriedades, Box<dyn ErroExpr>> {
		if elementos.is_empty() {
			return Ok(Propriedades {
				variavel: None,
				valor: None,
			});
		}

		let mut var = None;

		for el in elementos.iter().map(|el| el.prop.variavel) {
			if var.zip(el).filter(|(e, d)| e != d).is_some() {
				return Err(Box::new(ErroParser::SomenteUmaVariavelPermitida));
			}
			var = var.or(el);
		}

		Ok(Propriedades {
			variavel: var,
			valor: None,
		})
	}

	fn exp(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		let mut esq = self.exp_mul()?;

		let mut tipo = self.proximo_token.tipo();
		while let Mais | Menos = tipo {
			self.consome_token(tipo)?;
			let dir = self.exp_mul()?;
			let prop = Parser::merge_props(vec![&esq, &dir])?;
			esq = Producao::new(
				TipoProducao::ExpBinaria {
					esq: Rc::from(esq),
					op: tipo,
					dir: Rc::from(dir),
				},
				Some(prop),
			);
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
			let prop = Parser::merge_props(vec![&esq, &dir])?;
			esq = Producao::new(
				TipoProducao::ExpBinaria {
					esq: Rc::from(esq),
					op: tipo,
					dir: Rc::from(dir),
				},
				Some(prop),
			);
			tipo = self.proximo_token.tipo();
		}

		Ok(esq)
	}

	fn exp_pot(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		let base = self.exp_final()?;

		if self.proximo_token.tipo() == Potencia {
			self.consome_token(Potencia)?;
			let expoente = self.exp_pot()?;
			let prop = Parser::merge_props(vec![&base, &expoente])?;
			Ok(Producao::new(
				TipoProducao::ExpBinaria {
					esq: Rc::from(base),
					op: Potencia,
					dir: Rc::from(expoente),
				},
				Some(prop),
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
			Eof => Parser::final_inesperado(),
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
			| ConstPI | ConstE | Numero | Eof => self.consome_token(Menos)?,
		};

		let operando = Rc::new(self.exp_final()?);
		let prop = operando.prop;
		Ok(Producao::new(
			TipoProducao::ExpUnaria { operador, operando },
			Some(prop),
		))
	}

	fn log_natural(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		self.consome_token(LogNatural)?;
		self.consome_token(AbreParenteses)?;
		let logaritmando = self.exp()?;
		self.consome_token(FechaParenteses)?;

		let prop = logaritmando.prop;
		Ok(Producao::new(
			TipoProducao::ExpLogNatural(Rc::from(logaritmando)),
			Some(prop),
		))
	}

	fn log(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		self.consome_token(Log)?;
		self.consome_token(AbreParenteses)?;
		let base = Rc::new(self.exp()?);
		self.consome_token(Virgula)?;
		let logaritmando = Rc::new(self.exp()?);
		self.consome_token(FechaParenteses)?;

		let prop = Parser::merge_props(vec![&base, &logaritmando])?;
		Ok(Producao::new(
			TipoProducao::ExpLog { base, logaritmando },
			Some(prop),
		))
	}

	fn trig(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		let operacao = self.proximo_token.tipo();
		match operacao {
			Seno => self.consome_token(Seno)?,
			Cosseno => self.consome_token(Cosseno)?,
			Tangente | AbreParenteses | FechaParenteses | Mais | Menos | Asterisco | Barra
			| Potencia | Virgula | Raiz | Log | LogNatural | Integral | X | Theta | ConstPI
			| ConstE | Numero | Eof => self.consome_token(Tangente)?,
		};

		self.consome_token(AbreParenteses)?;
		let argumento = self.exp()?;
		self.consome_token(FechaParenteses)?;

		let prop = argumento.prop;

		match operacao {
			Seno => Ok(Producao::new(
				TipoProducao::ExpSeno(Rc::from(argumento)),
				Some(prop),
			)),
			Cosseno => Ok(Producao::new(
				TipoProducao::ExpCosseno(Rc::from(argumento)),
				Some(prop),
			)),
			Tangente => Ok(Producao::new(
				TipoProducao::ExpTangente(Rc::from(argumento)),
				Some(prop),
			)),

			AbreParenteses | FechaParenteses | Mais | Menos | Asterisco | Barra | Potencia
			| Virgula | Raiz | Log | LogNatural | Integral | X | Theta | ConstPI | ConstE
			| Numero | Eof => unreachable!(),
		}
	}

	fn raiz(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		self.consome_token(Raiz)?;
		self.consome_token(AbreParenteses)?;
		let indice = Rc::new(self.exp()?);
		self.consome_token(Virgula)?;
		let radicando = Rc::new(self.exp()?);
		self.consome_token(FechaParenteses)?;

		let prop = Parser::merge_props(vec![&indice, &radicando])?;

		Ok(Producao::new(
			TipoProducao::ExpRaiz { indice, radicando },
			Some(prop),
		))
	}

	fn integral(&mut self) -> Result<Producao, Box<dyn ErroExpr>> {
		self.consome_token(Integral)?;
		self.consome_token(AbreParenteses)?;
		let inf = Rc::new(self.exp()?);
		self.consome_token(Virgula)?;
		let sup = Rc::new(self.exp()?);
		self.consome_token(Virgula)?;
		let fun = Rc::new(self.exp()?);
		self.consome_token(FechaParenteses)?;

		if inf.prop.variavel.or_else(|| sup.prop.variavel).is_some() {
			return Err(Box::new(ErroParser::LimitesIntegralComVariavel));
		}

		let prop = fun.prop;

		Ok(Producao::new(
			TipoProducao::ExpIntegral { inf, sup, fun },
			Some(prop),
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
