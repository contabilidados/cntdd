#' Gera número CNPJ no formato desejado
#'
#' @details
#'
#' Essa função formata o número do CNPJ completo (99.999.999/0001-99) ou
#' simplificado (999999990000199), baseado em string com número do CNPJ.
#'
#' @param cnpj string com número do CNPJ
#' @param completo Se `TRUE` ou `T`, o output mostrar o CNPJ no formato 99.999.999/0001-99. Se `FALSE` ou `F`, o output será no formato 99999999000199.
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
#' @export

utl_CNPJ_Mascara <- function(cnpj, completo = T){

  ################ Instruções #####################################################
  # Objetivo: Gerar a máscara do CNPJ quando o CNPJ vier como numero
  #
  # Input: CNPJ em formato numérico
  # Output: CNPJ em formato string com a máscara de 18 dígitos
  #################################################################################

  CNPJ <- paste(stringr::str_extract_all(cnpj, "[0-9]+")[[1]], collapse = "")

  if(nchar(CNPJ) > 14){
    CNPJ <- "99999999999999"
  }

  qdeCar <- nchar(CNPJ)

  if(qdeCar > 14) stop("Numero de caracteres numéricos supera os 14 caracteres do CNPJ")

  cnpj1 <- paste0(substr("00000000000000", 1, (14-qdeCar)),
                  substr(CNPJ, 1, qdeCar-2), "-",
                  substr(CNPJ, qdeCar-1, qdeCar))
  cnpjFinal <- paste0(substr(cnpj1, 1, 2), ".",
                      substr(cnpj1, 3, 5), ".",
                      substr(cnpj1, 6, 8), "/",
                      substr(cnpj1, 9, 15))
  cnpjNum <- paste(stringr::str_extract_all(cnpjFinal, "[0-9]+")[[1]], collapse = "")

  if (completo) {
    return(cnpjFinal)
  } else {
    return(cnpjNum)
  }

}

