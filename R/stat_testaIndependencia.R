#' Teste de Independência
#'
#' @details
#'
#' Essa função faz o teste de independência entre variáveis categóricas e retorna
#' uma lista com a análise correspondente.
#'
#' @param bd Um data.frame com as variaveis categóricas
#' @param ... a lista de variaveis categóricas de interesse
#' @param correcao Se `TRUE`, fará a correção dos erros
#'
#' @examples
#'
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: \href{http://contabilidados.com.br}{(Acesse Aqui)}. Ao acessar, fazer busca
#' pelo nome da função `ind_liqSeca`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: \email{contabilidados@@ufersa.edu.br}
#' Siga-nos no Instagram: \href{https://www.instagram.com/contabilidados/}{@contabilidados}
#'
#' @import dplyr
#' @import tidyr
#' @import rstatix
#' @export

testaIndependencia <-

  function(bd, correcao = T, ...){

    bd %>%
      dplyr::select(...) -> bd

    tabela <-  table(bd)
    testeQui2 <- testaAssociacao(table(bd), CORREC = correcao)
    esperado <- suppressMessages(suppressWarnings(chisq.test(table(bd), correct = correcao)$expected))
    erroStd <- suppressMessages(suppressWarnings(chisq.test(table(bd), correct = correcao)$stdres))

    novoalva <- 0.1/(ncol(tabela) * nrow(tabela))
    corte <- round(abs(qnorm(novoalva/2)), 4)
    residSig = abs(erroStd) > corte

    vCramer <- round(suppressMessages(suppressWarnings(cramer_v(tabela))), 4)

    resultado <- list(
      `Tabela de Frequência` =  tabela,
      `Análise Qui2` = testeQui2,
      `Valores Esperados` = esperado,
      `Resíduos Padronizados Ajustados` = erroStd,
      `Ponto de Corte` = corte,
      `Resíduos Significativos` = residSig,
      `V de Cramer` = vCramer
    )

    return(resultado)

}
