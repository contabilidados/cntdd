#' Formata significancia
#'
#' @description
#' Essa funcao formata a estatistica t e o nivel de significancia. Seu resultado
#' pode ser uma Estatistica t com o numero de casas decimais informados com o
#' respectivo p-valor identificado por asteriscos.
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
#' @param sig o P-Value
#' @param t a Estatística t
#' @param nDigits Numero de Digitos
#' @param sigValue Valores da significancia (Padrao: 1%, 5% e 10%)
#' @param sigSimbolo Simbolos usados para indicar as significancias (Padrao: 1%, 5% e 10%)
#'
#' @import dplyr
#' @import tidyr
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
