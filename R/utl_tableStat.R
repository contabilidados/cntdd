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
#' library(cntdd)
#'
#' ## Valor da média das despesas operacionais de grupos de empresas baseadas no
#' ## valor do caixa e equivalentes de caixa e nas dívidas de curto prazo.
#' ## No exemplo, as empresas com mais dívidas de curto prazo e maiores valores
#' ## de caixa e equivalentes de caixa são as que possuem os maiores valores
#' ## médios de despesas operacionais ($ 1.999.101).
#'
#' utl_tableStat(cntdd::dt_contabil, "cxEquiv", 3, "dividasCP", 2, "despOper", mean)
#'
#' ## Por outro lado, as empresas com valores intermediários de caixa e equivalentes
#' ## caixa (cxEquiv_2) e com maiores valores de dívidas de curto prazo são as que
#' ## apresentam maior desvio-padrão entre os valores médios ($ 874.873).
#'
#' utl_tableStat(cntdd::dt_contabil, "cxEquiv", 3, "dividasCP", 2, "despOper", sd)
#'
#' @import ggplot2
#' @import readxl
#' @import dplyr
#' @import tidyr
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
