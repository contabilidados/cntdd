#' Calcular a média geométrica
#'
#' Esta função calculará a média geométrica de um vetor numérico
#'
#' @param x Um vetor numérico
#' @param dec O número de casas decimais (padrão: 3)
#'
#' @examples
#'
#' geomMean(1:6, 2) # Vetor
#'
#' geomMean(mtcars$mpg) # DataFrame
#'
#' mtcars %>% summarise(media = geomMean(mpg)) # Dplyr
#'
#' @export

stat_geomMean <- function(x, dec = 3){
  round(prod(x, na.rm = T)^(1/length(x[!is.na(x)])), dec)
}
