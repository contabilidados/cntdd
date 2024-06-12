#' Análise Vertical
#'
#' @description
#' Essa função calcula a análise vertical de contas específicas.
#'
#' @details
#' Apresenta como resultado um data.frame com 4 colunas: conta, valor, AV e Conta Base.
#'
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.quarto.pub/>.
#' Ao acessar, fazer busca pelo nome da funcao `ind_analiseVertical`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param contas Um vetor tipo character com o nome das contas para análise vertical
#' @param valores Vetor numérico indicando o valor das contas para análise vertical
#' @param contaBase String com o nome da conta que servirá de base para a análise vertical
#' @param vrContaBase Valor numérico da conta base
#'
#' @examples
#' library(cntdd)
#'
#' ind_analiseVertical(
#' contas = c("estoques", "imobilizado"),
#' valores = c(300, 700),
#' contaBase = "Ativo Total",
#' vrContaBase = 2000
#' )
#'
#' @export

ind_analiseVertical <- function(
    contas = c("estoques", "imobilizado"), valores = c(300, 700),
    contaBase = "Ativo Total", vrContaBase = 2000){

  dt <-
    data.frame(
    conta = contas, valor = valores, AV = valores/vrContaBase, `conta base` = contaBase
  )

  names(dt)[4] <- "Conta Base"

  return(dt)

  }


