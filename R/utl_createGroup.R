#' Cria grupos
#'
#' Cria grupos baseados nos percentise de uma variável númerica
#'
#' @param bd Um data.frame
#' @param col_Value String informando o nome da coluna referente à variável númerica sobre a qual se pretende criar os grupos
#' @param col_Grp String com o nome da nova coluna a ser criada com os grupos
#' @param n_grp Número de grupos, se pretende criar grupos com o mesmo número de observações ou um vetor contendo os percentis para cada grupo. Padrão igual 2.
#' @param lbl_grp String com o prefixo posto no início do nome do de cada grupo. Padrão igual "grp"
#'
#' @seealso [cut()], [quantile()]
#'
#' @examples
#'
#' library(cntdd)
#' library(dplyr)
#'
#' ## Grupo 1 com os primeiros 20% das observações de menor valor, Grupo 2 com as observações de
#' ## 20% a 80% e Grupo 3 com os 20% das observações de maior valor
#' utl_createGroup(mtcars, "mpg", "grp.mpg", c(0, .2, .8, 1)) %>% head()
#'
#' ## Grupo 1 com metade das observações e Grupo 2 com a metade restante
#' utl_createGroup(mtcars, "mpg", "grp.mpg", 2) %>% head()
#'
#'
#' @import ggplot2
#' @import readxl
#' @import dplyr
#' @export

utl_createGroup <-
  function(bd, col_Value, col_Grp, n_grp = 2) {

    bd %>% dplyr::select_at(.vars = col_Value) %>% names -> vlName

    if(length(n_grp)==1){
      dplyr::mutate(.data = bd, {{col_Grp}} :=
                      cut(bd[[col_Value]],
                          quantile(bd[[col_Value]], probs = 0:n_grp/n_grp),
                          include.lowest = T,
                          labels = paste0(vlName, "_", 1:n_grp)))
    } else {
      dplyr::mutate(.data = bd, {{col_Grp}} :=
                      cut(bd[[col_Value]],
                          quantile(bd[[col_Value]], probs = n_grp),
                          include.lowest = T,
                          labels = paste0(vlName, "_", 1:(length(n_grp)-1))))
    }
}
