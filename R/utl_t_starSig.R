#' Formata significância
#'
#' @details
#'
#' Essa função formata a estatística t e o nível de significância. Seu resultado
#' pode ser uma Estatística t com o número de casas decimais informados com o
#' respectivo p-valor identificado por asteriscos, sendo `*`10%, `**`5% e `***`1%.
#'
#' @param sig o P-Value
#' @param t a Estatística t
#' @param nDigits Número de Dígitos
#'
#' @examples
#'
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: \href{http://contabilidados.com.br}{(Acesse Aqui)}. Ao acessar, fazer busca
#' pelo nome da função `ind_liqSeca`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: \email{contabilidados@@ufersa.edu.br}
#' Siga-nos no Instagram: \href{https://www.instagram.com/contabilidados/}{@contabilidados}
#'
#' @import dplyr
#' @import tidyr
#' @import rstatix
#' @export

utl_tStarSig <-
  function(sig, t = NULL, nDigits = 2){

    if(is.null(t)){
      star <-
        case_when(
          sig < 0.01 ~ "***",
          sig < 0.05 ~ "**",
          sig < 0.10 ~ "*",
          TRUE ~ ""
        )
    } else {

      t <- format(round(t, nDigits), nsmall = nDigits)

      star <-
        case_when(
          sig <= 0.01 ~ "***",
          sig <= 0.05 ~ "**",
          sig <= 0.10 ~ "*",
          TRUE ~ ""
        )

      star <- paste(c(t, star), collapse = "")

    }

    return(star)

  }
