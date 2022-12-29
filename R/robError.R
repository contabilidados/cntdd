#' Erros robustos
#'
#' Estimação consistente com a heterocedasticidade e/ou autocorrelação da matriz
#' de covariância das estimativas de coeficientes em modelos de regressão.
#'
#' @param reg Um objeto do tipo "lm"
#' @param tipo String informando qual a correção da heterocedasticidade (Hc1, Hc2...)
#' @param HAC Se FALSE(default), os erros serão robustos à heterocedasticidade. Se TRUE, serão robustos à heterocedasticidade e à autocorrelação.
#'
#' @seealso [cut()], [quantile()], [vcovHAC()], [vcovHC()]
#'
#' @examples
#'
#' library(cntdd)
#' library(dplyr)
#'
#' ## Erro robusto à hetorecedasticidade e à autocorrelação
#' a <- lm(mpg~qsec, data = mtcars)
#' utl_robErro(a, HAC = T)
#'
#' ## Erro robusto à hetorecedasticidade
#' a <- lm(mpg~qsec, data = mtcars)
#' utl_robErro(a, tipo = "HC2")
#'
#'
#' @export

utl_robErro <- function(reg, tipo = "HC1", HAC = F) {

  if(HAC){
    robErr <- sqrt(diag(sandwich::vcovHAC(reg)))
  } else{
    robErr <- sqrt(diag(sandwich::vcovHC(reg, type = tipo)))
  }
  return(robErr)
}
