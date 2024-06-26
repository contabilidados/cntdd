% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ind_pmre.R
\name{ind_pmre}
\alias{ind_pmre}
\title{Prazo Médio de Renovação dos Estoques}
\usage{
ind_pmre(
  indicador = "PMRE",
  periodo = (year(Sys.Date()) - 2):(year(Sys.Date()) - 1),
  estoqueInicial = c(700, 500),
  estoqueFinal = c(230, 180),
  cmv = c(5220, 4340),
  receitaLiquida = c(8820, 8630),
  atvTotal = c(10900, 9800),
  qdeDiasAno = 365,
  giro = F,
  relatorio = T,
  titulo = "Evolucao do PMRE",
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

\item{estoqueInicial}{Vetor com os valores do Estoque do período anterior ao da análise}

\item{estoqueFinal}{Vetor com os valores do Estoque do período da análise}

\item{cmv}{Vetor com os valores do custo das mercadorias vendidas da empresa}

\item{receitaLiquida}{Vetor com os valores da receita líquida da empresa}

\item{atvTotal}{Vetor com os valores do Ativo Total}

\item{qdeDiasAno}{Quantidade de dias do ano. (Padrão: 365)}

\item{giro}{Se \code{TRUE} mostrará a quantidade de giros anuais (útil na análise
Dupont), Se \code{FALSE} (padrão), mostrará a quantidade média de dias para
renovação dos estoques.}

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
Essa função calcula o Prazo Médio de Renovação dos Estoques da empresa
}
\details{
Representa a quantidade média de dias em que os estoques ficam armazenados
na empresa.

Apresenta como resultado uma lista com 5 itens:
\enumerate{
\item \strong{Gráfico} se o parâmetro \code{relatorio} for \code{TRUE} ou \code{T}, mostra um gráfico com a
evolução do ativo cíclico da empresa durante os períodos. Se for \code{FALSE} ou \code{F},
o gráfico não é apresentado;
\item \strong{Contas} que corresponde ao banco de dados com as contas informadas para
cálculo do indicador;
\item \strong{Índice} o PMRE dos períodos informados;
\item \strong{Análise Vertical} Análise Vertical das contas informadas no item 2.
Contas de resultado terão suas análises verticais em relação à receita total e
contas patrimoniais terão suas análises verticais em relação ao ativo total;
\item \strong{Análise Horizontal} Análise Horizontal das contas informadas no item 2.
}

Todos os itens da lista são bancos de dados no formato tibble que podem ser
usados individualmente durante o processo de análise de dados.

Informações adicionais sobre como usar o pacote, orientamos acessar o menu
\code{cntdd} do Blog do Projeto contabiliDados: \url{https://contabilidados.com.br/}.
Ao acessar, fazer busca pelo nome da função \code{ind_pmre}

Contatos pelo email do Projeto contabiliDados:
Email: \href{mailto:contabilidados@ufersa.edu.br}{contabilidados@ufersa.edu.br}
Siga-nos no Instagram: \url{https://www.instagram.com/contabilidados} @contabilidados
}
\examples{
library(cntdd)

ind_pmre(
 indicador = "PMRE",
 periodo = 2021:2022,
 estoqueInicial = c(700, 500),
 estoqueFinal = c(230, 180),
 cmv = c(5220, 4340),
 receitaLiquida = c(8820, 8630),
 atvTotal = c(10900, 9800),
 qdeDiasAno = 365,
 giro = FALSE,
 relatorio = TRUE,
 titulo = "Evolucao do PMRE",
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
\seealso{
\code{\link[=ind_analiseCiclo]{ind_analiseCiclo()}}
}
