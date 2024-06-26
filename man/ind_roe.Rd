% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ind_roe.R
\name{ind_roe}
\alias{ind_roe}
\title{Índice de Retorno sobre o Patrimônio Líquido (ROE)}
\usage{
ind_roe(
  indicador = "ROE",
  periodo = (year(Sys.Date()) - 2):(year(Sys.Date()) - 1),
  lucLiq = c(8, 14),
  patLiq = c(150, 200),
  receitaTotal = c(300, 500),
  passivoTotal = c(400, 500),
  relatorio = T,
  titulo = "Evolucao do ROE",
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

\item{lucLiq}{Vetor com os valores do lucro líquido da empresa}

\item{patLiq}{Vetor com os valores do patrimônio líquido da empresa}

\item{receitaTotal}{Vetor com os valores da receita total da empresa}

\item{passivoTotal}{Vetor com os valores do passivo total da empresa}

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
Essa função calcula o Índice de Retorno sobre o Patrimônio Líquido baseado em
vetores relativos às contas de lucro líquido e patrimônio líquido.
}
\details{
Apresenta como resultado uma lista com 5 itens:
\enumerate{
\item \strong{Gráfico} se o parâmetro \code{relatorio} for \code{TRUE} ou \code{T}, mostra um gráfico com a
evolução do ativo cíclico da empresa durante os períodos. Se for \code{FALSE} ou \code{F},
o gráfico não é apresentado;
\item \strong{Contas} que corresponde ao banco de dados com as contas informadas para
cálculo do indicador;
\item \strong{Índice} o ROE dos períodos informados;
\item \strong{Análise Vertical} Análise Vertical das contas informadas no item 2.
Contas de resultado terão suas análises verticais em relação à receita total e
contas patrimoniais terão suas análises verticais em relação ao ativo total;
\item \strong{Análise Horizontal} Análise Horizontal das contas informadas no item 2.
}

Todos os itens da lista são bancos de dados no formato tibble que podem ser
usados individualmente durante o processo de análise de dados.

Informações adicionais sobre como usar o pacote, orientamos acessar o menu
\code{cntdd} do Blog do Projeto contabiliDados: \url{https://contabilidados.com.br/}.
Ao acessar, fazer busca pelo nome da função \code{ind_roe}

Contatos pelo email do Projeto contabiliDados:
Email: \href{mailto:contabilidados@ufersa.edu.br}{contabilidados@ufersa.edu.br}
Siga-nos no Instagram: \url{https://www.instagram.com/contabilidados} @contabilidados
}
\examples{
library(cntdd)

ind_roe(
 indicador = "ROE",
 periodo = 2021:2022,
 lucLiq = c(8,14),
 patLiq = c(150,200),
 receitaTotal = c(300,500),
 passivoTotal = c(400,500),
 relatorio = TRUE,
 titulo = "Evolucao do ROE",
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
