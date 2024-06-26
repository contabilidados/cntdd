% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ind_wacc.R
\name{ind_wacc}
\alias{ind_wacc}
\title{Custo Médio Ponderado de Capital (WACC ou CMPC)}
\usage{
ind_wacc(
  indicador = "WACC",
  periodo = (year(Sys.Date()) - 2):(year(Sys.Date()) - 1),
  empFinCP = c(200, 300),
  empFinLP = c(100, 200),
  patLiq = c(400, 600),
  despJuros = c(30, 40),
  receita = c(1000, 1200),
  atvTotal = c(4000, 5000),
  ir = 0.34,
  capm = T,
  ke = 0.15,
  beta = 1.3,
  rf = 0.07,
  rm = 0.13,
  relatorio = T,
  titulo = "Evolucao do WACC",
  subtitulo = "",
  rodape = paste0("@", year(Sys.Date()), " contabiliDados"),
  corFundo = "orange",
  corLinhaTendencia = "orange",
  tamanhoValores = 6,
  tamanhoTempo = 10,
  tamanhoVariavel = 4,
  tamanhoTitulo = 14,
  tamanhoSubTitulo = 10,
  tamanhoRodape = 8,
  corRodape = "gray"
)
}
\arguments{
\item{indicador}{Um vetor tipo character com o nome do indicador}

\item{periodo}{Vetor numérico indicando o período da análise}

\item{empFinCP}{Vetor com os valores de empréstimos e financiamentos de curto prazo (Passivo Circulante)}

\item{empFinLP}{Vetor com os valores de empréstimos e financiamentos de longo prazo (Passivo Não Circulante)}

\item{patLiq}{Vetor com os valores do Patrimônio Líquido}

\item{despJuros}{Vetor com os valores das despesas com juros (DRE/Notas Explicativas)}

\item{receita}{Vetor com os valores da receita líquida (Demonstração de Resultado - DRE)}

\item{atvTotal}{Vetor com os valores do Ativo Total}

\item{ir}{Valor relativo a alíquota do Imposto de Renda}

\item{capm}{Se \code{TRUE}, calcula \code{ke} pela fórmula \code{rf + beta * (rm - rf)}.
Se \code{FALSE}, calcula pelo parâmetro \code{ke}}

\item{ke}{Valor relativo ao custo de capital do sócio}

\item{beta}{Valor relativo ao beta (medida de volatilidade ou risco sistemático)
de um título ou portfólio comparado ao mercado como um todo}

\item{rf}{Valor relativo à taxa de juros livre de risco}

\item{rm}{Valor relativo à taxa de remuneração paga pelo mercado}

\item{relatorio}{Se \code{TRUE}, Mostra relatório do indicador. Se \code{FALSE}, mostra
apenas o vetor com resultados do indicador (TRUE/FALSE)}

\item{titulo}{Título do gráfico}

\item{subtitulo}{Subtítulo do gráfico}

\item{rodape}{Rodapé do gráfico}

\item{corFundo}{Cor de fundo para os valores (Padrão: Laranja)}

\item{corLinhaTendencia}{Cor da linha de tendência entre os valores (Padrão: Laranja)}

\item{tamanhoValores}{Tamanho da fonte dos valores apresentados (Padrão: 6)}

\item{tamanhoTempo}{Tamanho da fonte dos rótulos relativo aos períodos de tempo (Padrão: 10)}

\item{tamanhoVariavel}{Tamanho da fonte do texto relativo à variável analisada (Padrão: 4)}

\item{tamanhoTitulo}{Tamanho da fonte do título (Padrão: 14)}

\item{tamanhoSubTitulo}{Tamanho da fonte do subtítulo (Padrão: 10)}

\item{tamanhoRodape}{Tamanho da fonte do rodapé (Padrão: 8)}

\item{corRodape}{Cor da fonte do rodapé (Padrão: Cinza)}
}
\description{
Essa função calcula o Custo Médio Ponderado de Capital baseado em vetores relativos ao
custo da dívida (capital de terceiros, deduzido do benefício fiscal do imposto de renda),
peso da dívida, custo de capital do sócio (capital próprio) e peso do capital do sócio.
}
\details{
Apresenta como resultado uma lista com 5 itens:
\enumerate{
\item \strong{Gráfico} se o parâmetro \code{relatorio} for \code{TRUE} ou \code{T}, mostra um gráfico com a
evolução do ativo cíclico da empresa durante os períodos. Se for \code{FALSE} ou \code{F},
o gráfico não é apresentado;
\item \strong{Contas} que corresponde ao banco de dados com as contas informadas para
cálculo do indicador;
\item \strong{Índice} o WACC dos períodos informados;
\item \strong{Análise Vertical} Análise Vertical das contas informadas no item 2.
Contas de resultado terão suas análises verticais em relação à receita total e
contas patrimoniais terão suas análises verticais em relação ao ativo total;
\item \strong{Análise Horizontal} Análise Horizontal das contas informadas no item 2.
}

Todos os itens da lista são bancos de dados no formato tibble que podem ser
usados individualmente durante o processo de análise de dados.

Informações adicionais sobre como usar o pacote, orientamos acessar o menu
\code{cntdd} do Blog do Projeto contabiliDados: \url{https://contabilidados.com.br/}.
Ao acessar, fazer busca pelo nome da função \code{ind_wacc}

Contatos pelo email do Projeto contabiliDados:
Email: \href{mailto:contabilidados@ufersa.edu.br}{contabilidados@ufersa.edu.br}
Siga-nos no Instagram: \url{https://www.instagram.com/contabilidados} @contabilidados
}
\examples{
library(cntdd)

ind_wacc(
 indicador = "WACC",
 periodo = 2021:2022,
 empFinCP = c(200, 300),
 empFinLP = c(100, 200),
 patLiq = c(400, 600),
 despJuros = c(30, 40),
 receita = c(1000, 1200),
 atvTotal = c(4000, 5000),
 ir = 0.34,
 capm = TRUE,
 ke = 0.15,
 beta = 1.3,
 rf = 0.07,
 rm = 0.13,
 relatorio = TRUE,
 titulo = "Evolucao do WACC",
 subtitulo = "",
 rodape = "",
 corFundo = "orange",
 corLinhaTendencia = "orange",
 tamanhoValores = 6,
 tamanhoTempo = 10,
 tamanhoVariavel = 4,
 tamanhoTitulo = 14,
 tamanhoSubTitulo = 10,
 tamanhoRodape = 8,
 corRodape = "gray"
 )

}
