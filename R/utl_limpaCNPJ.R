#' Limpeza de CNPJs em um DataFrame
#'
#' @description
#' Esta funcão recebe um DataFrame e um nome de coluna, e retorna uma lista de CNPJs limpos,
#' com a remoção de caracteres especiais como pontos, barras e hífens.
#'
#' @param df DataFrame que contém a coluna de CNPJs.
#' @param nome_coluna O nome da coluna no DataFrame que contém os CNPJs.
#'
#' @details
#' A máscara do CNPJ deve esta no formato 99.999.999/9999-99.
#'
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.quarto.pub/>.
#' Ao acessar, fazer busca pelo nome da função `utl_limpaCNPJ`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @return Um vetor de CNPJs limpos.
#'
#' @examples
#' library(cntdd)
#'
#' utl_limpaCNPJ(cntdd::dt_cvmB3, cnpj)
#'
#' @import dplyr
#' @export


utl_limpaCNPJ <- function(df, nome_coluna) {

  cnpj_limpados <-
    df %>%
    select({{nome_coluna}}) %>%
    filter(!is.na({{nome_coluna}})) %>%
    pull({{nome_coluna}})

  cnpj_limpados <- gsub("[./-]", "", cnpj_limpados)


  return(cnpj_limpados)

}


