#' Erros robustos
#'
#' @description
#' Estimação consistente com a heterocedasticidade e/ou autocorrelação da matriz
#' de covariância das estimativas de coeficientes em modelos de regressão.
#'
#' @details
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.quarto.pub/>.
#' Ao acessar, fazer busca pelo nome da função `utl_robErro`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param reg Um objeto do tipo "lm"
#' @param tipo String informando qual a correção da heterocedasticidade (HC1, HC2...)
#' @param HAC Se FALSE(default), os erros serão robustos à heterocedasticidade.
#' Se TRUE, serão robustos à heterocedasticidade e a autocorrelação.
#'
#' @seealso [cut()], [quantile()], [vcovHAC()], [vcovHC()]
#'
#' @examples
#' library(cntdd)
#' library(sandwich)
#' dt <- na.omit(cntdd::dt_contabil)
#'
#' estima <- lm(log(estoques) ~ log(receita), dt)
#'
#' utl_robErro(estima)
#'
#' utl_robErro(estima, tipo = "HC3")
#'
#' utl_robErro(estima, HAC = TRUE)
#'
#' @import sandwich
#' @export

utl_robErro <- function(reg, tipo = "HC1", HAC = F) {

  if(HAC){
    robErr <- sqrt(diag(vcovHAC(reg)))
  } else{
    robErr <- sqrt(diag(vcovHC(reg, type = tipo)))
  }
  return(robErr)
}

