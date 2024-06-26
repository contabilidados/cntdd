% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utl_createGroup.R
\name{utl_createGroup}
\alias{utl_createGroup}
\title{Cria grupos}
\usage{
utl_createGroup(bd, col_Value, col_Grp, n_grp = 2)
}
\arguments{
\item{bd}{Um data.frame}

\item{col_Value}{String informando o nome da coluna referente a variável numérica
sobre a qual se pretende criar os grupos}

\item{col_Grp}{String com o nome da nova coluna a ser criada com os grupos}

\item{n_grp}{Número de grupos, se pretende criar grupos com o mesmo numero de
observações ou um vetor contendo os percentis para cada grupo. Padrão igual 2.}
}
\description{
Cria grupos baseados nos percentis de uma variavel numérica.
}
\details{
Útil para tornar variáveis numéricas em categóricas para realização de testes
de análise exploratória.

Informações adicionais sobre como usar o pacote, orientamos acessar o menu
\code{cntdd} do Blog do Projeto contabiliDados: \url{https://contabilidados.quarto.pub/}.
Ao acessar, fazer busca pelo nome da função \code{utl_createGroup}.

Contatos pelo email do Projeto contabiliDados:
Email: \href{mailto:contabilidados@ufersa.edu.br}{contabilidados@ufersa.edu.br}
Siga-nos no Instagram: \url{https://www.instagram.com/contabilidados} @contabilidados
}
\examples{
library(cntdd)
library(dplyr)

dt <- cntdd::dt_contabil \%>\% filter(!is.na(ativoTotal))

utl_createGroup(bd = dt, col_Value = "ativoTotal", col_Grp = "GrpAtivo", n_grp = 3) \%>\%
select(empresa, ano, ativoTotal, GrpAtivo) \%>\% head()

}
\seealso{
\code{\link[=cut]{cut()}}, \code{\link[=quantile]{quantile()}}
}
