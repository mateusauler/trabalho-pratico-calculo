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
		Ok(r) => println!("{r}"),
		Err(e) => eprintln!("{e}"),
	}
}

fn run(exp: String, precisao: u32) -> Result<f64, Box<dyn ErroExpr>> {
	parser::Parser::new(&exp, precisao)?.calcular_valor()
}
