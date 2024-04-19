#' Contador de valores nulos em colunas
#'
#' @description
#' Faz a contagem de valores nulos/inexistentes em cada coluna.
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
#' @param dt Um data.frame
#' @param ... Um vetor que irá receber os nomes das colunas que terão os valores nulos contabilizados
#'
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
