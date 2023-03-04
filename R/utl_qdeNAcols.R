#' Contador de valores nulos em colunas
#'
#' Faz a contagem de valores nulos/inexistentes em cada coluna
#'
#' @param dt Um data.frame
#' @param ... Um vetor que irá receber os nomes das colunas que terão os valores nulos contabilizados
#'
#'
#' @examples
#'
#' library(cntdd)
#' library(dplyr)
#'
#' df <- data.frame(X1 = c(NA, 1:2, NA), X2 = c(NA, NA, 2, 4), X3 = c(NA, 4, 1, NA))
#' ## Escolha das colunas utilizando a função starts_with
#' utl_qdeNAcols(df, starts_with(c("X")))
#'
#' ## Escolha das colunas informando o nome diretamente
#' utl_qdeNAcols(df, c("X1", "X2", "X3"))
#'
#' ## Escolha das colunas informando o inicio da coluna e a sequência de números
#' utl_qdeNAcols(df, num_range("X", 1:3))
#'
#' ## Escolha das colunas informando o número das colunas. Nesse caso, a primeira e a terceira coluna
#' utl_qdeNAcols(df, c(1,3))
#'
#' ## Escolha das colunas informando todo o banco de dados
#' utl_qdeNAcols(df, everything())
#' @import ggplot2
#' @import readxl
#' @import dplyr
#' @export

utl_qdeNAcols <-
  function(dt, ...){
    dt %>%
      rowwise() %>%
      mutate(
        qdeNA = sum(is.na(c_across(all_of(...))))
      )
  }
