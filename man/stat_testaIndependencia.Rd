% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_testaIndependencia.R
\name{stat_testaIndependencia}
\alias{stat_testaIndependencia}
\title{Teste de Independência}
\usage{
stat_testaIndependencia(bd, correcao = T, ...)
}
\arguments{
\item{bd}{Um data.frame com as variáveis categóricas}

\item{correcao}{Se \code{TRUE}, fará a correcao dos erros}

\item{...}{a lista de variáveis categóricas de interesse}
}
\description{
Essa função faz o teste de independência entre variáveis categóricas e retorna
uma lista com a análise correspondente.
}
\details{
Informações adicionais sobre como usar o pacote, orientamos acessar o menu
\code{cntdd} do Blog do Projeto contabiliDados: \url{https://contabilidados.quarto.pub/}.
Ao acessar, fazer busca pelo nome da função \code{stat_testaIndependencia}

Contatos pelo email do Projeto contabiliDados:
Email: \href{mailto:contabilidados@ufersa.edu.br}{contabilidados@ufersa.edu.br}
Siga-nos no Instagram: \url{https://www.instagram.com/contabilidados} @contabilidados
}
\examples{
library(cntdd)
library(dplyr)

dt <-
cntdd::dt_cvmB3 \%>\%
transmute(
  especieControleAcionario,
  banco = if_else(setorCVM == "Bancos", "Banco", "Nao Banco")
  )

stat_testaIndependencia(bd = dt, correcao = FALSE, especieControleAcionario, banco)

}
