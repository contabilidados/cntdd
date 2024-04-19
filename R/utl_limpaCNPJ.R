#' Limpeza de CNPJs em um DataFrame
#'
#' @description
#' Esta funcao recebe um DataFrame e um nome de coluna, e retorna uma lista de CNPJs limpos,
#' com a remocao de caracteres especiais como pontos, barras e hifens.
#'
#' @param df DataFrame que contém a coluna de CNPJs.
#' @param nome_coluna O nome da coluna no DataFrame que contem os CNPJs.
#'
#' @details
#' A máscara do CNPJ deve esta no formato 99.999.999/9999-99.
#'
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <http://contabilidados.com.br>.
#' Ao acessar, fazer busca pelo nome da função `utl_limpaCNPJ`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @return Um vetor de CNPJs limpos.
#'
#' @import dplyr
#' @import tidyr
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


