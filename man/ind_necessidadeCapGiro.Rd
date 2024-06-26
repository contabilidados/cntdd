% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ind_necessidadeCapGiro.R
\name{ind_necessidadeCapGiro}
\alias{ind_necessidadeCapGiro}
\title{Necessidade de Capital de Giro (NCG)}
\usage{
ind_necessidadeCapGiro(
  indicador = "Necessidade de Capital de Giro",
  periodo = (year(Sys.Date()) - 2):(year(Sys.Date()) - 1),
  atvCirculante = c(150, 180),
  cxEquiv = c(230, 180),
  aplicFinanc = c(20, 40),
  outAtvCircLiqImed = c(0, 0),
  passivoCirculante = c(400, 350),
  empFinCP = c(120, 210),
  outPasCircOneroso = c(2, 10),
  atvTotal = c(900, 800),
  relatorio = T,
  titulo = "Evolucao da Necessidade de Capital de Giro",
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

\item{atvCirculante}{Vetor com os valores do ativo circulante da empresa}

\item{cxEquiv}{Vetor com os valores da conta caixa e equivalentes de caixa da empresa}

\item{aplicFinanc}{Vetor com os valores das aplicações financeiras de curto prazo da empresa}

\item{outAtvCircLiqImed}{Vetor com os valores de outros ativos circulantes de liquidez imediata da empresa}

\item{passivoCirculante}{Vetor com os valores do passivo circulante da empresa}

\item{empFinCP}{Vetor com os valores de empréstimos e financiamentos de curto prazo da empresa}

\item{outPasCircOneroso}{Vetor com os valores de outros passivos onerosos de curto prazo da empresa}

\item{atvTotal}{Vetor com os valores do Ativo Total}

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
Essa função calcula a necessidade de capital de giro da empresa.
}
\details{
Trata-se da diferença entre ativo cíclico e passivo cíclico. Portanto, se
concentra nos ativos de curto prazo voltados às operações específicas da
atividade da empresa

Apresenta como resultado uma lista com 5 itens:
\enumerate{
\item \strong{Gráfico} se o parâmetro \code{relatorio} for \code{TRUE} ou \code{T}, mostra um gráfico com a
evolução do ativo cíclico da empresa durante os períodos. Se for \code{FALSE} ou \code{F},
o gráfico não é apresentado;
\item \strong{Contas} que corresponde ao banco de dados com as contas informadas para
cálculo do indicador;
\item \strong{Índice} a NCG dos períodos informados;
\item \strong{Análise Vertical} Análise Vertical das contas informadas no item 2.
Contas de resultado terão suas análises verticais em relação à receita total e
contas patrimoniais terão suas análises verticais em relação ao ativo total;
\item \strong{Análise Horizontal} Análise Horizontal das contas informadas no item 2.
}

Todos os itens da lista são bancos de dados no formato tibble que podem ser
usados individualmente durante o processo de análise de dados.

Informações adicionais sobre como usar o pacote, orientamos acessar o menu
\code{cntdd} do Blog do Projeto contabiliDados: \url{https://contabilidados.com.br/}.
Ao acessar, fazer busca pelo nome da função \code{ind_necessidadeCapGiro}

Contatos pelo email do Projeto contabiliDados:
Email: \href{mailto:contabilidados@ufersa.edu.br}{contabilidados@ufersa.edu.br}
Siga-nos no Instagram: \url{https://www.instagram.com/contabilidados} @contabilidados
}
\examples{
library(cntdd)

ind_necessidadeCapGiro(
 indicador = "Necessidade de Capital de Giro",
 periodo = 2022:2023,
 atvCirculante = c(150, 180),
 cxEquiv = c(230, 180),
 aplicFinanc = c(20, 40),
 outAtvCircLiqImed = c(0, 0),
 passivoCirculante = c(400, 350),
 empFinCP = c(120, 210),
 outPasCircOneroso = c(2, 10),
 atvTotal = c(900, 800),
 relatorio = TRUE,
 titulo = "Evolucao da Necessidade de Capital de Giro",
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
\code{\link[=ind_ativoCiclico]{ind_ativoCiclico()}}, \code{\link[=ind_ativoFinanceiro]{ind_ativoFinanceiro()}}, \link{ind_passivoCiclico}, \link{ind_passivoFinanceiro}
}
