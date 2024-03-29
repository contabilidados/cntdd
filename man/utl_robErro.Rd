% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/robError.R
\name{utl_robErro}
\alias{utl_robErro}
\title{Erros robustos}
\usage{
utl_robErro(reg, tipo = "HC1", HAC = F)
}
\arguments{
\item{reg}{Um objeto do tipo "lm"}

\item{tipo}{String informando qual a correção da heterocedasticidade (Hc1, Hc2...)}

\item{HAC}{Se FALSE(default), os erros serão robustos à heterocedasticidade. Se TRUE, serão robustos à heterocedasticidade e à autocorrelação.}
}
\description{
Estimação consistente com a heterocedasticidade e/ou autocorrelação da matriz
de covariância das estimativas de coeficientes em modelos de regressão.
}
\examples{

library(cntdd)
library(dplyr)

## Erro robusto à hetorecedasticidade e à autocorrelação
a <- lm(mpg~qsec, data = mtcars)
utl_robErro(a, HAC = T)

## Erro robusto à hetorecedasticidade
a <- lm(mpg~qsec, data = mtcars)
utl_robErro(a, tipo = "HC2")

}
\seealso{
\code{\link[=cut]{cut()}}, \code{\link[=quantile]{quantile()}}, \code{\link[=vcovHAC]{vcovHAC()}}, \code{\link[=vcovHC]{vcovHC()}}
}
