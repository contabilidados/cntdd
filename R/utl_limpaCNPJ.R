#' Limpeza de CNPJs em um DataFrame
#'
#' Esta função recebe um DataFrame e um nome de coluna, e retorna uma lista de CNPJs limpos,
#' com a remoção de caracteres especiais como pontos, barras e hífens.
#'
#' @param df DataFrame que contém a coluna de CNPJs.
#' @param nome_coluna O nome da coluna no DataFrame que contém os CNPJs.
#'
#' @return Um vetor de CNPJs limpos.
#'
#' @examples
#' df <- data.frame(cnpj = c("00.000.000/0001-00", "11.111.111/1111-11"))
#' utl_limpaCNPJ(df, "cnpj")
#' @export
#' 

utl_limpaCNPJ <- function(df, nome_coluna) {
  
  cnpj_limpados <-
    df %>%
    select({{nome_coluna}}) %>%
    filter(!is.na({{nome_coluna}})) %>%
    pull({{nome_coluna}}) %>%
    gsub("[./-]", "", .)
  
  return(cnpj_limpados)
  
}


