#' Erros robustos
#'
#' @description
#' Estimacao consistente com a heterocedasticidade e/ou autocorrelacao da matriz
#' de covariancia das estimativas de coeficientes em modelos de regressao.
#'
#' @details
#' Informacoes adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <http://contabilidados.com.br>.
#' Ao acessar, fazer busca pelo nome da funcao `utl_tStarSig`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param reg Um objeto do tipo "lm"
#' @param tipo String informando qual a correção da heterocedasticidade (HC1, HC2...)
#' @param HAC Se FALSE(default), os erros serao robustos a heterocedasticidade. Se TRUE, serao robustos a heterocedasticidade e a autocorrelacao.
#'
#' @seealso [cut()], [quantile()], [vcovHAC()], [vcovHC()]
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
