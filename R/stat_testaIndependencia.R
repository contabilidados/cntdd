#' Teste de Independencia
#'
#' @description
#' Essa funcao faz o teste de independencia entre variaveis categoricas e retorna
#' uma lista com a analise correspondente.
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
#' @param bd Um data.frame com as variaveis categoricas
#' @param ... a lista de variaveis categoricas de interesse
#' @param correcao Se `TRUE`, fara a correcao dos erros
#'
#' @import dplyr
#' @import tidyr
#' @import rstatix
#' @importFrom stats chisq.test
#' @importFrom stats qnorm
#' @export

stat_testaIndependencia <-

  function(bd, correcao = T, ...){

    bd %>%
      select(...) -> bd

    tabela <-  table(bd)
    testeQui2 <- stat_testaAssociacao(table(bd), CORREC = correcao)
    esperado <- suppressMessages(suppressWarnings(chisq.test(table(bd), correct = correcao)$expected))
    erroStd <- suppressMessages(suppressWarnings(chisq.test(table(bd), correct = correcao)$stdres))

    novoalva <- 0.1/(ncol(tabela) * nrow(tabela))
    corte <- round(abs(qnorm(novoalva/2)), 4)
    residSig = abs(erroStd) > corte

    vCramer <- round(suppressMessages(suppressWarnings(cramer_v(tabela))), 4)

    resultado <- list(
      `Tabela de Frequencia` =  tabela,
      `Analise Qui2` = testeQui2,
      `Valores Esperados` = esperado,
      `Residuos Padronizados Ajustados` = erroStd,
      `Ponto de Corte` = corte,
      `Residuos Significativos` = residSig,
      `V de Cramer` = vCramer
    )

    return(resultado)

}
