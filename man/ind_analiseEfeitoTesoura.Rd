% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ind_analiseEfeitoTesoura.R
\name{ind_analiseEfeitoTesoura}
\alias{ind_analiseEfeitoTesoura}
\title{Efeito Tesoura}
\usage{
ind_analiseEfeitoTesoura(
  periodo = (year(Sys.Date()) - 2):(year(Sys.Date()) - 1),
  ccl = c(653, 127),
  ncg = c(390, 476),
  ajusteTexto = 0.2,
  titulo = "Analise do Efeito Tesoura",
  subtitulo = "Analise Dinamica do Capital de Giro",
  rodape = paste0("@", year(Sys.Date()), " contabiliDados")
)
}
\arguments{
\item{periodo}{Vetor numérico indicando o período da análise}

\item{ccl}{Vetor com os valores do capital circulante líquido}

\item{ncg}{Vetor com os valores da necessidade de capital de giro}

\item{ajusteTexto}{parâmetro para ajuste do saldo de tesouraria no gráfico (padrão: 0.2)}

\item{titulo}{Título do gráfico}

\item{subtitulo}{Subtítulo do gráfico}

\item{rodape}{Rodapé do gráfico}
}
\description{
Essa função fornece dar suporte para análise do Efeito Tesoura.
}
\details{
Trata-se da diferença entre o capital circulante líquido e o ativo cíclico.
Quando negativa, indica a existência do efeito tesoura. Apresenta como resultado
um gráfico com a evoluçaõd do CCL e da NCG, evidenciando (ou não) a ocorrência
do efeito tesoura.

Informações adicionais sobre como usar o pacote, orientamos acessar o menu
\code{cntdd} do Blog do Projeto contabiliDados: \url{https://contabilidados.com.br/}.
Ao acessar, fazer busca pelo nome da função \code{ind_necessidadeCapGiro}

Contatos pelo email do Projeto contabiliDados:
Email: \href{mailto:contabilidados@ufersa.edu.br}{contabilidados@ufersa.edu.br}
Siga-nos no Instagram: \url{https://www.instagram.com/contabilidados} @contabilidados
}
\examples{
library(cntdd)

ind_analiseEfeitoTesoura(
periodo = 2022:2023,
ccl = c(653, 127),
ncg = c(390, 476),
ajusteTexto = 0.2,
titulo = "Analise do Efeito Tesoura",
subtitulo = "Analise Dinamica do Capital de Giro",
rodape = ""
)

}
\seealso{
\code{\link[=ind_capitalCircLiquido]{ind_capitalCircLiquido()}}, \code{\link[=ind_necessidadeCapGiro]{ind_necessidadeCapGiro()}}, \code{\link[=ind_saldoTesouraria]{ind_saldoTesouraria()}}
}
