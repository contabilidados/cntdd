#' Contador de valores nulos em colunas
#'
#' @description
#' Faz a contagem de valores nulos/inexistentes em cada coluna de um data.frame.
#'
#' @details
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.quarto.pub/>.
#' Ao acessar, fazer busca pelo nome da função `utl_qdeNAcols`.
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param dt Um data.frame
#' @param nomeVariavel String contendo o nome da coluna que receberá a quantidade de NAs
#' @param ... Um vetor que irá receber os nomes das colunas que terão os valores nulos contabilizados
#'
#' @examples
#' library(cntdd)
#' library(dplyr)
#'
#' df <- data.frame(X1 = c(NA, 1:2, NA), X2 = c(NA, NA, 2, 4), X3 = c(NA, 4, 1, NA))
#'
#' ## Escolha das colunas utilizando a função starts_with
#' utl_qdeNAcols(df, "qdeNAcomX", starts_with(c("X")))
#'
#' ## Escolha das colunas informando o nome diretamente
#' utl_qdeNAcols(df, "qdeNAx1ax3", c("X1", "X2", "X3"))
#'
#' ## Escolha das colunas informando o inicio da coluna e a sequência de números
#' utl_qdeNAcols(df, "qdeNA", num_range("X", 1:3))
#'
#' ## Escolha das colunas informando o número das colunas. Nesse caso, a primeira e a terceira coluna
#' utl_qdeNAcols(df, "qdeNA", c(1,3))
#'
#' ## Escolha das colunas informando todo o banco de dados
#' utl_qdeNAcols(df, "qdeNAtodas", everything())
#'
#' @import dplyr
#' @export

utl_qdeNAcols <-
  function(dt, nomeVariavel, ...){
    dt %>%
      rowwise() %>%
      mutate(
        qdeNA = sum(is.na(c_across(all_of(...))))
      ) %>%
      rename_with(~ nomeVariavel, all_of("qdeNA"))
  }
