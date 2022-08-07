#' Tabela de média por grupos
#'
#' Calcula a média de uma variável numérica baseada em grupos de duas variáveis.
#' trata-se de uma análise tridimensional
#'
#' @param bd Um data.frame
#' @param col_Value String informando o nome da coluna referente à variável númerica sobre a qual se pretende criar os grupos
#' @param col_Grp String com o nome da nova coluna a ser criada com os grupos
#' @param n_grp Número de grupos, se pretende criar grupos com o mesmo número de observações ou um vetor contendo os percentis para cada grupo. Padrão igual 2.
#' @param lbl_grp String com o prefixo posto no início do nome do de cada grupo. Padrão igual "grp"
#'
#' @examples
#'
#' ## Grupo 1 com os primeiros 20% das observações de menor valor, Grupo 2 com as observações de
#' ## 20% a 80% e Grupo 3 com os 20% das observações de maior valor
#' utl_createGroup(mtcars, "mpg", "grp.mpg", c(0, .2, .8, 1))
#'
#' ## Grupo 1 com metade das observações e Grupo 2 com a metade restante
#' utl_createGroup(mtcars, "mpg", "grp.mpg", 2)
#'
#'
#' @export


utl_tableStat <- function(bd, grp1, n_grp1, grp2, n_grp2, value, ...){

  bd %>% dplyr::select_at(.vars = value) %>% names -> vlName

  bd %>%
    utl_createGroup(., grp1, "grpA", n_grp1) %>%
    utl_createGroup(., grp2, "grpB", n_grp2) %>%
    group_by(grpA, grpB) %>%
    summarise_at(value, ...) %>%
    pivot_wider(names_from = "grpB", values_from = value) -> result

  names(result)[1] <- paste0("Stat_", vlName)

  return(result)

}








