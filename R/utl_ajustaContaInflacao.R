#' Ajuste de Valores pela Inflação
#'
#' @description
#' Faz o ajuste do valor de uma conta pelo valor do índice de inflação informado.
#'
#' @details
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.quarto.pub/>.
#' Ao acessar, fazer busca pelo nome da função `utl_ajustaContaInflacao`.
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param periodo Vetor contendo o período dos dados
#' @param vrConta Vetor contendo o valor da conta que pretende atualizar pela inflação
#' @param indiceInflacao Valor percentual do indice de inflacao do período.
#' Se o valor for 2,5%, por exemplo, informar 2.5
#' @param periodoBase Período específico para o qual deseja atualizar os valores
#' @param apenasVetor Se `TRUE` ou `T` (padrão), o resultado é um vetor com os valores
#' atualizados. Se `FALSE` ou `F`, o resultado é um data.frame com todos os valores
#' envolvidos no cálculo do ajuste à inflação
#'
#' @examples
#' library(cntdd)
#'
#' utl_ajustaContaInflacao(
#' periodo = 2019:2023,
#' vrConta = c(100, 110, 120, 130, 140),
#' indiceInflacao = c(4.31, 4.52, 10.06, 5.79, 4.62),
#' periodoBase = 2023, apenasVetor = TRUE)
#'
#' @import dplyr
#' @export

utl_ajustaContaInflacao <-
  function(
    periodo = 2019:2023,
    vrConta = c(100, 110, 120, 130, 140),
    indiceInflacao = c(4.31, 4.52, 10.06, 5.79, 4.62),
    periodoBase = 2023, apenasVetor = T){

    message("Aviso: Certifique-se de que os dados de inflacao estao em percentual.
            Exemplo: Se for 2,5%, informar o valor 2.5 no argumento 'indiceInflacao'")

    dt <-
      data.frame(
        periodo = periodo, valor = vrConta, indiceInflacao = indiceInflacao
      )

    dt$INDICE <- 1 + indiceInflacao / 100

    dt$indiceBASE <- dt$INDICE[dt$periodo == periodoBase]

    dt$fatorAjuste <- dt$indiceBASE / dt$INDICE

    dt$valorAjustado <- dt$valor * dt$fatorAjuste

    vetorValor <- dt$valorAjustado

    if(apenasVetor){
      return(vetorValor)
    } else {
      return(dt)
    }

  }

