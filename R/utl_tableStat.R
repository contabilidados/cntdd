#' Tabela de estatística por grupos
#'
#' @description
#' Calcula a estatistica descritiva de uma variável numérica baseada em grupos de duas variaveis.
#' trata-se de uma análise tridimensional.
#'
#' @details
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.quarto.pub/>.
#' Ao acessar, fazer busca pelo nome da função `utl_tableStat`.
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param bd Um data.frame
#' @param grp1 String informando o nome da coluna referente a variável 1
#' @param n_grp1 Número de grupos da variável 1
#' @param grp2 String informando o nome da coluna referente a variável 2
#' @param n_grp2 Número de grupos da variável 2
#' @param value Variável do banco de dados da qual se pretende retirar a estatística descritiva
#' @param ... Informar qual estatística descritiva (mean, sd)
#'
#' @examples
#' library(cntdd)
#' library(dplyr)
#' library(tidyr)
#'
#' dt <- na.omit(cntdd::dt_contabil)
#'
#' utl_tableStat(bd = dt,
#' grp1 = "estoques", n_grp1 = 2, grp2 = "flxCxInvest", n_grp2 = 2, value = "receita", mean)
#'
#' @import dplyr
#' @import tidyr
#' @export


utl_tableStat <- function(bd, grp1, n_grp1, grp2, n_grp2, value, ...){

  bd %>% select(all_of(value)) %>% names -> vlName

  # bd %>%
    bd <- utl_createGroup(bd, grp1, "grpA", n_grp1)
    bd <- utl_createGroup(bd, grp2, "grpB", n_grp2) %>%
      group_by(across(all_of(c("grpA", "grpB")))) %>%
      summarise_at(value, ...) %>%
      pivot_wider(names_from = "grpB", values_from = value) -> result

  names(result)[1] <- paste0("Stat_", vlName)

  return(result)

}


