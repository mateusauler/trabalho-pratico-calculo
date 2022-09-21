use clap::Parser;
use trabalho_pratico::{parser, ErroExpr};

#[derive(Parser, Default, Debug)]
#[clap(
	author = "Mateus Auler",
	about = "Trabalho prático da matéria Cálculo II"
)]
struct Args {
	/// A expressão a ser avaliada
	exp: String,
	/// A precisão com a qual uma integral deve ser avaliada
	precisao: Option<u32>,
}

fn main() {
	let args = Args::parse();
	let precisao = args.precisao.unwrap_or(1000);
	let exp = args.exp;

	let resultado = run(exp, precisao);

	match resultado {
		Ok(Some(r)) => println!("Resultado: {r}."),
		Ok(None) => println!("Sem resultado."),
		Err(e) => eprintln!("{e}"),
	}
}

fn run(exp: String, precisao: u32) -> Result<Option<f64>, Box<dyn ErroExpr>> {
	let mut p = parser::Parser::new(&exp, precisao)?;
	let mut parsed = p.parse()?;
	Ok(parsed.calcular_valor(None, &p))
}
