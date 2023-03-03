#' Índice de Liquidez Corrente
#'
#' @details
#'
#' Liquidez corrente é uma medida financeira que indica a capacidade de uma
#' empresa de cumprir suas obrigações de curto prazo. Calculada
#' dividindo-se o total de ativos circulantes pelo total de passivos
#' circulantes, a liquidez corrente é expressa em forma de números, sendo
#' valores acima de 1 indicativos de uma posição financeira saudável.
#' É representado pela fórmula:
#'
#' \deqn{\frac{AC}{PC}}
#' em que: **AC** é o Ativo Circulante e **PC** é o Passivo Circulante
#'
#' Para melhorar o processo de análise, os valores de Ativos Circulante e Passivo
#' Circulante foram desmembrados, respectivamente para
#'  \eqn{AC = cxEquiv + estoque + ctaRecCP + outAtvCirc} e para
#'  \eqn{PC = fornec + dividasCP + outPasCirc}. O item detalhes (details) apresenta
#'  a descrição de cada conta.
#'
#'  Assim, tem-se que a liquidez corrente corresponde a:
#'
#'  A equação não contempla todas as contas do ativo circulante com exceção da
#'  conta estoque.
#'
#'  A liquidez corrente pode ser útil para gestores financeiros, pois permite
#'  avaliar a necessidade de recursos de curto prazo, como empréstimos bancários ou
#'  financiamentos, para cobrir despesas operacionais
#'
#' @param indicador Um vetor tipo character com o nome do indicador
#' @param periodo Vetor numérico indicando o período da análise
#' @param cxEquiv Vetor com os valores do caixa e equivalentes de caixa (Ativo Circulante)
#' @param estoque Vetor com os valores do estoque (Ativo Circulante)
#' @param ctaRecCP Vetor com os valores do contas a receber (Ativo Circulante)
#' @param outAtvCirc Vetor com os valores de outros ativos circulantes (Ativo Circulante)
#' @param fornec Vetor com os valores de fornecedores (Passivo Circulante)
#' @param dividasCP Vetor com os valores de empréstimos e financiamentos de curto prazo (Passivo Circulante)
#' @param outPasCirc Vetor com os valores de outros passivos circulantes (Passivo Circulante)
#' @param atvTotal Vetor com os valores do Ativo Total
#' @param plot Mostra gráfico? (TRUE/FALSE)
#'
#' @examples
#'
#' suppressMessages(suppressWarnings(library(cntdd)))
#' suppressMessages(suppressWarnings(library(dplyr)))
#' suppressMessages(suppressWarnings(library(tidyr)))
#' suppressMessages(suppressWarnings(library(ggplot2)))
#'
#' ## Usando Vetores
#'
#' ind_liqCorrente(
#' indicador  = "Liquidez Corrente",
#' periodo    = 2018:2020,
#' cxEquiv    = c(500,300,400),
#' estoque    = c(2000,3000,4000),
#' ctaRecCP   = c(2500, 5000, 2800),
#' outAtvCirc = c(20, 35, 80),
#' fornec     = c(1200, 1400, 1600),
#' dividasCP  = c(500, 200, 750),
#' outPasCirc = c(30, 20, 40),
#' atvTotal   = c(10000, 12000, 11000),
#' plot = T)
#'
#' ## Usando um data.frame
#' ## Todos as variáveis devem ser do tipo numérico e os dados faltantes devem ser
#' ## substituidos por zero.
#'
#' dadosAlpha <- dt_contabil %>% filter(empresa == "alpha")
#'
#' ind_liqCorrente(
#' indicador  = "Liquidez Corrente",
#' periodo    = dadosAlpha$ano,
#' cxEquiv    = dadosAlpha$caixaEquiv,
#' aplicFin   = dadosAlpha$aplicFinanc,
#' estoque    = dadosAlpha$estoques,
#' ctaRecCP   = dadosAlpha$clientesCP,
#' fornec     = dadosAlpha$forneced,
#' dividasCP  = dadosAlpha$empFinCP,
#' atvTotal   = dadosAlpha$ativoTotal,
#' plot = F)
#'
#' @export

ind_liqCorrente <- function(
    indicador = "Liq Corrente", periodo = 2019:2020, cxEquiv = c(8,10),
    estoque = c(150,200), ctaRecCP = c(400, 300), outAtvCirc = c(1, 3),
    fornec = c(50, 20), dividasCP = c(30, 40), outPasCirc = c(10, 8),
    atvTotal = c(900,800), plot = T){

  ratio <-
    (cxEquiv + estoque + ctaRecCP + outAtvCirc)  /
    (fornec + dividasCP + outPasCirc)

  dt <- data.frame(Periodo = periodo, ratio = ratio)
  names(dt)[2] <- indicador

  if(plot){
    print(ind_plots(dt, indicador))
  }

  dtGeral <-
    data.frame(
      periodo     = periodo,
      cxEquiv     = cxEquiv,
      estoque     = estoque,
      ctaRecCP      = ctaRecCP,
      outAtvCirc  = outAtvCirc,
      fornec      = fornec,
      dividasCP   = dividasCP,
      outPasCirc  = outPasCirc,
      atvTotal    = atvTotal
    ) %>%
    mutate(
      {{indicador}} := ratio
    ) %>%
    arrange(periodo) %>%
    mutate(
      AH.CxEquiv    = round(cxEquiv / dplyr::lag(cxEquiv) - 1, 4),
      AH.Estoque    = round(estoque / dplyr::lag(estoque) - 1, 4),
      AH.ctaRecCP     = round(ctaRecCP / dplyr::lag(ctaRecCP) - 1, 4),
      AH.outAtvCirc = round(outAtvCirc / dplyr::lag(outAtvCirc) - 1, 4),
      AH.fornec     = round(fornec / dplyr::lag(fornec) - 1, 4),
      AH.dividasCP  = round(dividasCP / dplyr::lag(dividasCP) - 1, 4),
      AH.outPasCirc = round(outPasCirc / dplyr::lag(outPasCirc) - 1, 4),
      AV.cxEquiv    = cxEquiv / atvTotal,
      AV.Estoque    = estoque / atvTotal,
      AV.ctaRecCP     = ctaRecCP / atvTotal,
      AV.outAtvCirc = outAtvCirc / atvTotal,
      AV.fornec     = fornec / atvTotal,
      AV.dividasCP  = dividasCP / atvTotal,
      AV.outPasCirc = outPasCirc / atvTotal
    )

  dtGeral %>%
    dplyr::select(periodo:atvTotal) %>%
    pivot_longer(cols = -periodo, names_to = "conta", values_to = "value") %>%
    na.omit() %>%
    pivot_wider(names_from = periodo, values_from = value) -> contas

  dtGeral %>%
    dplyr::select(periodo, {{indicador}}) %>%
    pivot_longer(cols = -periodo, names_to = "conta", values_to = "value") %>%
    na.omit() %>%
    pivot_wider(names_from = periodo, values_from = value) -> ratio

  dtGeral %>%
    dplyr::select(periodo, starts_with("AV.")) %>%
    pivot_longer(cols = -periodo, names_to = "conta", values_to = "value") %>%
    mutate(value = showPercent(value)) %>%
    na.omit() %>%
    pivot_wider(names_from = periodo, values_from = value) -> bdAV

  dtGeral %>%
    dplyr::select(periodo, starts_with("AH.")) %>%
    pivot_longer(cols = -periodo, names_to = "conta", values_to = "value") %>%
    mutate(value = showPercent(value)) %>%
    na.omit() %>%
    pivot_wider(names_from = periodo, values_from = value) -> bdAH


  return(list(Contas = contas, `Índice` = ratio,
              `Análise Vertical` = bdAV,
              `Análise Horizontal` = bdAH))

}


