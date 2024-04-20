#' Média geométrica
#'
#' @description
#' Esta função calculará a média geométrica de um vetor numérico.
#'
#' @details
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.quarto.pub/>.
#' Ao acessar, fazer busca pelo nome da função `stat_geomMean`.
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param x Um vetor numérico
#' @param dec O número de casas decimais (padrão: 3)
#'
#' @examples
#' stat_geomMean(x = c(4, 10), dec = 3)
#'
#' @export

stat_geomMean <- function(x, dec = 3){
  round(prod(x, na.rm = T)^(1/length(x[!is.na(x)])), dec)
}
