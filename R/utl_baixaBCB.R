#' Baixa dados Banco Central
#'
#' @description
#' Baixa dados da API do Banco Central
#'
#' @details
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.quarto.pub/>.
#' Ao acessar, fazer busca pelo nome da função `utl_baixaBCB`.
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param codSerie Valor do código da série no Banco Central
#' @param inicio String contendo a data de início da série no formato dd/mm/aaaa
#' @param fim String contendo a data final da série no formato dd/mm/aaaa
#'
#' @examples
#' library(cntdd)
#'
#' utl_baixaBCB(codSerie = 433, inicio = "01/01/2003", fim = "31/12/2017")
#'
#' @import jsonlite
#'
#' @export

utl_baixaBCB <- function(codSerie = 433, inicio = "01/01/2003", fim = "31/12/2017"){

  fromJSON(
    paste0(
      "https://api.bcb.gov.br/dados/serie/bcdata.sgs.",
      codSerie,
      "/dados?formato=json&dataInicial=",
      inicio, "&dataFinal=", fim
    )
  )
}


