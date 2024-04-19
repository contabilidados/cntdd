#' Media geometrica
#'
#' @description
#' Esta funcao calculara a media geometrica de um vetor numerico.
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
#' @param x Um vetor numerico
#' @param dec O numero de casas decimais (padrao: 3)
#'
#' @export

stat_geomMean <- function(x, dec = 3){
  round(prod(x, na.rm = T)^(1/length(x[!is.na(x)])), dec)
}
