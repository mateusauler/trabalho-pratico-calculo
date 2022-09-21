## Como Utilizar

### Baixar o `cargo`
  - Seguindo as [instruções oficiais](https://doc.rust-lang.org/cargo/getting-started/installation.html)
  - Ou utilizando o gerenciador de pacotes da sua distribuição de Linux

### Clonar o repositório
  - [Instale o git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git), caso não tenha instalado.
  - Execute `git clone https://github.com/mateusauler/trabalho-pratico-calculo.git <DIR> && cd <DIR>`, substituindo `<DIR>` pelo nome do diretório desejado. 

### Execução
  - Execute `cargo run --release --` ou `cargo run --release -q --`, seguido da expressão entre áspas e, opcionalmente, o número de passos do somatório de Riemann para o cálculo de integrais.
  Ex.: cargo run --release -q -- "int(1, 3, -x^2 + 4*x - 3)" 100000

### Formato das expressões
As expressões são combinações dos seguintes elementos:
  - Integral: `int(<limite inferior>, <limite superior>, <função a integrar>)`
  - Logarítmo: `log(<base>, <logaritmando>)`
  - Logarítmo natural: `ln(<logaritmando>)`
  - Seno, Cosseno, Tangente: `<sen | cos | tan> (<função>)`
  - Raíz: `raiz(<índice>, <função>)`
  - Constante de Euler: `e`
  - Constante π: `pi`
  - Variável x: `x`
  - Variável θ: `theta`
  - Soma: `<a> + <b>`
  - Subtração: `<a> - <b>`
  - Multiplicação: `<a> * <b>`
  - Divisão: `<a> / <b>`
  - Potenciação: `<base> ^ <expoente>`
  - Unária: `[- | +] <função>`

### Resultados com a precisão padrão de 1000

  - int(1, 3, -x^2 + 4*x - 3): 1.333332000000073
  - int(ln(2), 3, 5 * e^x): 90.55509492517142
  - int(-pi/2, pi/2, sen(theta)): -0.0031415926535675104
