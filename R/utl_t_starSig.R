#' Formata significância
#'
#' @description
#' Essa função formata a estatística t e o nível de significância. Seu resultado
#' pode ser uma Estatística t com o número de casas decimais informados com o
#' respectivo p-valor identificado por asteriscos.
#'
#' @details
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.quarto.pub/>.
#' Ao acessar, fazer busca pelo nome da função `utl_tStarSig`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param sig o P-Value
#' @param t a Estatística t
#' @param nDigits Número de Dígitos
#' @param sigValue Valores da significância (Padrão: 1%, 5% e 10%)
#' @param sigSimbolo Símbolos usados para indicar as significâncias (Padrão: 1%, 5% e 10%)
#'
#' @examples
#' library(cntdd)
#'
#' utl_tStarSig(0.03, t = NULL, nDigits = 2,
#' sigValue = c(0.01, 0.05, 0.10), sigSimbolo = c("***", "**", "*"))
#'
#' # ou
#'
#' utl_tStarSig(0.03, t = 1.97, nDigits = 2,
#' sigValue = c(0.01, 0.05, 0.10), sigSimbolo = c("+", "**", "*"))
#'
#' @import dplyr
#' @export

utl_tStarSig <-
  function(sig, t = NULL, nDigits = 2, sigValue = c(0.01, 0.05, 0.10), sigSimbolo = c("***", "**", "*")){

  SIGVALUE <- sigValue
  SIGSIMBOLO <- sigSimbolo

    if(is.null(t)){
      star <-
        case_when(
          sig < SIGVALUE[1] ~ SIGSIMBOLO[1],
          sig < SIGVALUE[2] ~ SIGSIMBOLO[2],
          sig < SIGVALUE[3] ~ SIGSIMBOLO[3],
          TRUE ~ ""
        )
    } else {

      t <- format(round(t, nDigits), nsmall = nDigits)

      star <-
        case_when(
          sig <= SIGVALUE[1] ~ SIGSIMBOLO[1],
          sig <= SIGVALUE[2] ~ SIGSIMBOLO[2],
          sig <= SIGVALUE[3] ~ SIGSIMBOLO[3],
          TRUE ~ ""
        )

      star <- paste(c(t, star), collapse = "")

    }

    return(star)

  }

