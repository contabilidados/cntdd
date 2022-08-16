#' Calcular a média geométrica
#'
#' Esta função calculará a média geométrica de um vetor numérico
#'
#' @param x Um vetor numérico
#' @param dec O número de casas decimais (padrão: 3)
#'
#' @import tidyverse
#'
#' @examples
#' library(cntdd)
#' stat_geomMean(1:6, 2) # Vetor
#'
#' stat_geomMean(dt_contabil$atvTotal) # DataFrame
#'
#' suppressMessages(suppressWarnings(library(dplyr)))
#' dt_contabil %>% summarise(estoqueMedio = stat_geomMean(estoque)) # Dplyr
#'
#' @export

stat_geomMean <- function(x, dec = 3){
  round(prod(x, na.rm = T)^(1/length(x[!is.na(x)])), dec)
}
