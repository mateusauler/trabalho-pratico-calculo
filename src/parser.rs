use crate::{
	lexer::{
		Lexer,
		TipoToken::{self, *},
		Token,
	},
	ErroExpr,
};
use std::{
	cell::RefCell,
	f64::consts::{E, PI},
	fmt::{Debug, Display},
};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Variavel {
	X,
	Theta,
}

#[derive(Debug, Copy, Clone)]
struct Propriedades {
	variavel: Option<Variavel>,
	valor: Option<f64>,
}

type Prd = Box<RefCell<Producao>>;
type Erro = Box<dyn ErroExpr>;

fn wrap(valor: Producao) -> Prd {
	Box::from(RefCell::from(valor))
}

#[derive(Debug, Clone)]
enum TipoProducao {
	ExpBinaria { esq: Prd, op: TipoToken, dir: Prd },
	ExpUnaria { operador: TipoToken, operando: Prd },
	ExpLog { base: Prd, logaritmando: Prd },
	ExpLogNatural(Prd),
	ExpSeno(Prd),
	ExpCosseno(Prd),
	ExpTangente(Prd),
	ExpRaiz { indice: Prd, radicando: Prd },
	ExpIntegral { inf: Prd, sup: Prd, fun: Prd },
	Final(Token),
}

#[derive(Debug)]
enum ErroParser {
	TokenInesperado { lex: String },
	FinalInesperado,
	SomenteUmaVariavelPermitida,
	LimitesIntegralComVariavel,
	ErroInesperado,
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
			ErroParser::ErroInesperado => write!(f, "Erro inesperado")
		}
	}
}

fn valor_ou_erro_generico<T>(opcao: Option<T>) -> Result<T, Erro> {
	opcao.ok_or_else(|| -> Erro { Box::from(ErroParser::ErroInesperado) })
}

#[derive(Debug, Clone)]
struct Producao {
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
			_ => prop.and_then(|p| p.variavel),
		};

		let prop = Propriedades {
			variavel,
			valor: None,
		};

		Self { tipo, prop }
	}

	fn from_token(token: Token) -> Self {
		Producao::new(TipoProducao::Final(token), None)
	}

	fn calcular_valor(&mut self, x: Option<f64>, precisao: u32) -> Result<f64, Erro> {
		if let Some(valor) = self.prop.valor {
			return Ok(valor);
		}

		let calc_valor = |v: &mut Prd| v.borrow_mut().calcular_valor(x, precisao);

		let valor = match &mut self.tipo {
			TipoProducao::ExpBinaria { esq, op, dir } => {
				let esq = calc_valor(esq)?;
				let dir = calc_valor(dir)?;
				match op {
					Mais => esq + dir,
					Menos => esq - dir,
					Asterisco => esq * dir,
					Barra => esq / dir,
					Potencia => esq.powf(dir),
					_ => unreachable!(),
				}
			}

			TipoProducao::ExpUnaria { operador, operando } => {
				let o = calc_valor(operando)?;
				match operador {
					Mais => o,
					Menos => -o,
					_ => unreachable!(),
				}
			}

			TipoProducao::ExpLog { base, logaritmando } => {
				calc_valor(logaritmando)?.log(calc_valor(base)?)
			}

			TipoProducao::ExpLogNatural(logaritmando) => calc_valor(logaritmando)?.ln(),
			TipoProducao::ExpSeno(v) => calc_valor(v)?.sin(),
			TipoProducao::ExpCosseno(v) => calc_valor(v)?.cos(),
			TipoProducao::ExpTangente(v) => calc_valor(v)?.tan(),

			TipoProducao::ExpRaiz { indice, radicando } => {
				calc_valor(radicando)?.powf(1.0 / calc_valor(indice)?)
			}

			TipoProducao::ExpIntegral { inf, sup, fun } => {
				let sup = calc_valor(sup)?;
				let inf = calc_valor(inf)?;
				let delta_x = (sup - inf) / precisao as f64;

				let mut x = inf;
				let mut total = 0.0;

				while x < sup {
					total += fun.borrow_mut().calcular_valor(Some(x), precisao)? * delta_x;
					x += delta_x;
				}

				total
			}

			TipoProducao::Final(t) => match t.tipo() {
				X => valor_ou_erro_generico(x)?,
				Theta => valor_ou_erro_generico(x)?,
				ConstPI => PI,
				ConstE => E,
				Numero => valor_ou_erro_generico(t.lexema().parse().ok())?,
				_ => unreachable!(),
			},
		};

		if self.prop.variavel.is_none() {
			self.prop.valor = Some(valor);
		}

		Ok(valor)
	}
}

pub struct Parser {
	lexer: Lexer,
	proximo_token: Token,
	precisao: u32,
	ast: Option<Producao>,
}

impl Parser {
	pub fn new<T: ToString + ?Sized>(texto: &T, precisao: u32) -> Result<Self, Erro> {
		let mut lexer = Lexer::new(texto);
		let proximo_token = lexer.proximo_token()?;
		Ok(Self {
			lexer,
			proximo_token,
			precisao,
			ast: None,
		})
	}

	pub fn calcular_valor(&mut self) -> Result<f64, Erro> {
		let p = self.precisao;
		self.parse()?.calcular_valor(None, p)
	}

	fn consome_token(&mut self, token_esperado: TipoToken) -> Result<(), Erro> {
		if self.proximo_token.tipo() == token_esperado {
			self.proximo_token = self.lexer.proximo_token()?;
		} else if self.proximo_token.tipo() == Eof {
			Parser::final_inesperado()?;
		} else {
			self.token_inesperado()?;
		}
		Ok(())
	}

	fn parse(&mut self) -> Result<&mut Producao, Erro> {
		if self.ast.is_none() {
			self.ast = Some(self.exp()?);
			self.consome_token(Eof)?;
		}
		valor_ou_erro_generico(self.ast.as_mut())
	}

	fn merge_props(elementos: Vec<&Prd>) -> Result<Propriedades, Erro> {
		if elementos.is_empty() {
			return Ok(Propriedades {
				variavel: None,
				valor: None,
			});
		}

		let mut var = None;

		for el in elementos.iter().map(|el| el.borrow().prop.variavel) {
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

	fn exp(&mut self) -> Result<Producao, Erro> {
		let mut esq = self.exp_unaria()?;

		let mut tipo = self.proximo_token.tipo();
		while let Mais | Menos = tipo {
			self.consome_token(tipo)?;
			let dir = self.exp_unaria()?;
			let esq_w = wrap(esq);
			let dir_w = wrap(dir);
			let prop = Parser::merge_props(vec![&esq_w, &dir_w])?;
			esq = Producao::new(
				TipoProducao::ExpBinaria {
					esq: esq_w,
					op: tipo,
					dir: dir_w,
				},
				Some(prop),
			);
			tipo = self.proximo_token.tipo();
		}

		Ok(esq)
	}

	fn exp_unaria(&mut self) -> Result<Producao, Erro> {
		let tipo = self.proximo_token.tipo();
		match tipo {
			Mais | Menos => {
				self.consome_token(tipo)?;
				let operando = self.exp_unaria()?;
				let prop = operando.prop;
				Ok(Producao::new(
					TipoProducao::ExpUnaria {
						operador: tipo,
						operando: wrap(operando),
					},
					Some(prop),
				))
			}
			_ => self.exp_mul(),
		}
	}

	fn exp_mul(&mut self) -> Result<Producao, Erro> {
		let mut esq = self.exp_pot()?;

		let mut tipo = self.proximo_token.tipo();
		while let Asterisco | Barra = tipo {
			self.consome_token(tipo)?;
			let dir = self.exp_pot()?;
			let esq_w = wrap(esq);
			let dir_w = wrap(dir);
			let prop = Parser::merge_props(vec![&esq_w, &dir_w])?;
			esq = Producao::new(
				TipoProducao::ExpBinaria {
					esq: esq_w,
					op: tipo,
					dir: dir_w,
				},
				Some(prop),
			);
			tipo = self.proximo_token.tipo();
		}

		Ok(esq)
	}

	fn exp_pot(&mut self) -> Result<Producao, Erro> {
		let base = self.exp_final()?;

		if self.proximo_token.tipo() == Potencia {
			self.consome_token(Potencia)?;
			let expoente = self.exp_pot()?;
			let esq = wrap(base);
			let dir = wrap(expoente);
			let prop = Parser::merge_props(vec![&esq, &dir])?;
			Ok(Producao::new(
				TipoProducao::ExpBinaria {
					esq,
					op: Potencia,
					dir,
				},
				Some(prop),
			))
		} else {
			Ok(base)
		}
	}

	fn exp_final(&mut self) -> Result<Producao, Erro> {
		let tipo = self.proximo_token.tipo();
		match tipo {
			X | Theta | ConstPI | ConstE | Numero => {
				let token = self.proximo_token.clone();
				self.consome_token(tipo)?;
				Ok(Producao::from_token(token))
			}

			AbreParenteses => self.exp_parenteses(),

			Raiz => self.raiz(),
			Log => self.log(),
			LogNatural => self.log_natural(),
			Seno | Cosseno | Tangente => self.trig(),
			Integral => self.integral(),

			FechaParenteses | Asterisco | Barra | Potencia | Virgula | Mais | Menos => {
				self.token_inesperado()
			}
			Eof => Parser::final_inesperado(),
		}
	}

	fn exp_parenteses(&mut self) -> Result<Producao, Erro> {
		self.consome_token(AbreParenteses)?;
		let exp = self.exp()?;
		self.consome_token(FechaParenteses)?;
		Ok(exp)
	}

	fn log_natural(&mut self) -> Result<Producao, Erro> {
		self.consome_token(LogNatural)?;
		self.consome_token(AbreParenteses)?;
		let logaritmando = self.exp()?;
		self.consome_token(FechaParenteses)?;

		let prop = logaritmando.prop;
		Ok(Producao::new(
			TipoProducao::ExpLogNatural(wrap(logaritmando)),
			Some(prop),
		))
	}

	fn log(&mut self) -> Result<Producao, Erro> {
		self.consome_token(Log)?;
		self.consome_token(AbreParenteses)?;
		let base = wrap(self.exp()?);
		self.consome_token(Virgula)?;
		let logaritmando = wrap(self.exp()?);
		self.consome_token(FechaParenteses)?;

		let prop = Parser::merge_props(vec![&base, &logaritmando])?;
		Ok(Producao::new(
			TipoProducao::ExpLog { base, logaritmando },
			Some(prop),
		))
	}

	fn trig(&mut self) -> Result<Producao, Erro> {
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

		let tipo = match operacao {
			Seno => TipoProducao::ExpSeno(wrap(argumento)),
			Cosseno => TipoProducao::ExpCosseno(wrap(argumento)),
			Tangente => TipoProducao::ExpTangente(wrap(argumento)),

			AbreParenteses | FechaParenteses | Mais | Menos | Asterisco | Barra | Potencia
			| Virgula | Raiz | Log | LogNatural | Integral | X | Theta | ConstPI | ConstE
			| Numero | Eof => unreachable!(),
		};

		Ok(Producao::new(tipo, Some(prop)))
	}

	fn raiz(&mut self) -> Result<Producao, Erro> {
		self.consome_token(Raiz)?;
		self.consome_token(AbreParenteses)?;
		let indice = wrap(self.exp()?);
		self.consome_token(Virgula)?;
		let radicando = wrap(self.exp()?);
		self.consome_token(FechaParenteses)?;

		let prop = Parser::merge_props(vec![&indice, &radicando])?;

		Ok(Producao::new(
			TipoProducao::ExpRaiz { indice, radicando },
			Some(prop),
		))
	}

	fn integral(&mut self) -> Result<Producao, Erro> {
		self.consome_token(Integral)?;
		self.consome_token(AbreParenteses)?;
		let inf = wrap(self.exp()?);
		self.consome_token(Virgula)?;
		let sup = wrap(self.exp()?);
		self.consome_token(Virgula)?;
		let fun = wrap(self.exp()?);
		self.consome_token(FechaParenteses)?;

		if inf
			.borrow()
			.prop
			.variavel
			.or(sup.borrow().prop.variavel)
			.is_some()
		{
			return Err(Box::new(ErroParser::LimitesIntegralComVariavel));
		}

		let prop = fun.borrow().prop;

		Ok(Producao::new(
			TipoProducao::ExpIntegral { inf, sup, fun },
			Some(prop),
		))
	}

	fn token_inesperado(&self) -> Result<Producao, Erro> {
		Err(Box::new(ErroParser::TokenInesperado {
			lex: self.proximo_token.lexema().to_owned(),
		}))
	}

	fn final_inesperado() -> Result<Producao, Erro> {
		Err(Box::new(ErroParser::FinalInesperado))
	}
}
