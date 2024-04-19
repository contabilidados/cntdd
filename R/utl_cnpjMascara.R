#' Mascara CNPJ
#'
#' @description
#' Gera numero CNPJ no formato desejado.
#'
#' @details
#' Essa funcao formata o numero do CNPJ completo (99.999.999/0001-99) ou
#' simplificado (999999990000199), baseado em string com numero do CNPJ.
#'
#' Informacoes adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <http://contabilidados.com.br>.
#' Ao acessar, fazer busca pelo nome da funcao `utl_tStarSig`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param cnpj string com numero do CNPJ
#' @param completo Se `TRUE` ou `T`, o output mostrar o CNPJ no formato 99.999.999/0001-99. Se `FALSE` ou `F`, o output sera no formato 99999999000199.
#'
#' @import stringr
#' @import dplyr
#' @export

utl_CNPJ_Mascara <- function(cnpj, completo = T){

  sapply(cnpj, function(cnpjBase){
    CNPJ <- paste(str_extract_all(cnpjBase, "[0-9]+")[[1]], collapse = "")

    if(nchar(CNPJ) > 14){
      CNPJ <- "99999999999999"
    }

    qdeCar <- nchar(CNPJ)

    if(qdeCar > 14) stop("Numero de caracteres numericos supera os 14 caracteres do CNPJ")

    cnpj1 <- paste0(substr("00000000000000", 1, (14-qdeCar)),
                    substr(CNPJ, 1, qdeCar-2), "-",
                    substr(CNPJ, qdeCar-1, qdeCar))
    cnpjFinal <- paste0(substr(cnpj1, 1, 2), ".",
                        substr(cnpj1, 3, 5), ".",
                        substr(cnpj1, 6, 8), "/",
                        substr(cnpj1, 9, 15))
    cnpjNum <- paste(str_extract_all(cnpjFinal, "[0-9]+")[[1]], collapse = "")

    if (completo) {
      return(cnpjFinal)
    } else {
      return(cnpjNum)
    }
  }
         )

}
