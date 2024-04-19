#' Tabela de estatistica por grupos
#'
#' @description
#' Calcula a estatistica descritiva de uma variavel numerica baseada em grupos de duas variaveis.
#' trata-se de uma analise tridimensional
#'
#' @details
#' Informacoes adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <http://contabilidados.com.br>.
#' Ao acessar, fazer busca pelo nome da funcao `utl_tStarSig`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param bd Um data.frame
#' @param grp1 String informando o nome da coluna referente a variavel 1
#' @param n_grp1 Numero de grupos da variavel 1
#' @param grp2 String informando o nome da coluna referente a variavel 2
#' @param n_grp2 Numero de grupos da variavel 2
#' @param value Variavel do banco de dados da qual se pretende retirar a estatisca descritiva
#' @param ... Informar qual estatistica descritiva (mean, sd)
#'
#' @import ggplot2
#' @import readxl
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
