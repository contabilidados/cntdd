#' Cria grupos
#'
#' @description
#' Cria grupos baseados nos percentis de uma variavel numérica.
#'
#' @details
#' Útil para tornar variáveis numéricas em categóricas para realização de testes
#' de análise exploratória.
#'
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.quarto.pub/>.
#' Ao acessar, fazer busca pelo nome da função `utl_createGroup`.
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param bd Um data.frame
#' @param col_Value String informando o nome da coluna referente a variável numérica
#' sobre a qual se pretende criar os grupos
#' @param col_Grp String com o nome da nova coluna a ser criada com os grupos
#' @param n_grp Número de grupos, se pretende criar grupos com o mesmo numero de
#' observações ou um vetor contendo os percentis para cada grupo. Padrão igual 2.
#'
#' @seealso [cut()], [quantile()]
#'
#' @examples
#' library(cntdd)
#' library(dplyr)
#'
#' dt <- cntdd::dt_contabil %>% filter(!is.na(ativoTotal))
#'
#' utl_createGroup(bd = dt, col_Value = "ativoTotal", col_Grp = "GrpAtivo", n_grp = 3) %>%
#' select(empresa, ano, ativoTotal, GrpAtivo) %>% head()
#'
#' @import dplyr
#' @importFrom stats quantile
#' @export

utl_createGroup <-
  function(bd, col_Value, col_Grp, n_grp = 2) {

    bd %>% select(all_of(col_Value)) %>% names -> vlName

    if(length(n_grp)==1){
      mutate(.data = bd, grupo =
                      cut(bd[[col_Value]],
                          quantile(bd[[col_Value]], probs = 0:n_grp/n_grp),
                          include.lowest = T,
                          labels = paste0(vlName, "_", 1:n_grp))) %>%
        rename_with(~ col_Grp, all_of("grupo"))
    } else {
      mutate(.data = bd, grupo =
                      cut(bd[[col_Value]],
                          quantile(bd[[col_Value]], probs = n_grp),
                          include.lowest = T,
                          labels = paste0(vlName, "_", 1:(length(n_grp)-1)))) %>%
        rename_with(~ col_Grp, all_of("grupo"))
    }
}
