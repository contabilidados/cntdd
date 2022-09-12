#' Índice de Liquidez Seca
#'
#' @details
#'
#' O índice de Liquidez Seca revela a capacidade de pagamento de dívidas de curto
#' prazo, considerando que a empresa não consiga reverter o valor de seus estoques
#' em dinheiro. Relaciona, portanto, quanto uma empresa possui de ativo
#'  (circulante), deduzido do valor de seus estoques, com o total de seu passivo
#'  circulante. É representado pela fórmula:
#'
#' \deqn{\frac{AC - Est}{PC}}
#' em que: **AC** é o Ativo Circulante, **Est** é o estoque e **PC** é o Passivo Circulante
#'
#' Para melhorar o processo de análise, os valores de Ativos Circulante e Passivo
#' Circulante foram desmembrados, respectivamente para
#'  \eqn{AC = cxEquiv + estoque + ctaRecCP + outAtvCirc} e para
#'  \eqn{PC = fornec + dividasCP + outPasCirc}. O item detalhes (details) apresenta
#'  a descrição de cada conta.
#'
#'  Assim, tem-se que a liquidez seca corresponde a:
#'
#'  \deqn{\frac{(cxEquiv + ctaRecCP + outAtvCirc)}{(fornec + dividasCP + outPasCirc)}}
#'
#'  A equação não contempla todas as contas do ativo circulante com exceção da
#'  conta estoque.
#'
#'  Indicamos essa análise quando a empresa apresentar dificuldade com o giro
#'  de seus estoques. Em codições normais, não haveria motivos para a empresa não
#'  conseguir rotacionar seus estoques. Quando há indicativos de que a rotação
#'  dos estoques ficará prejudicada, esse indicador poderá representar melhor
#'  a capacidade das empress em quitar suas dívidas de curto prazo.
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
#' @seealso [dplyr], [tidyr::pivot_longer()], [tidyr::pivot_wider()]
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
#' ind_liqSeca(
#' indicador  = "Liquidez Seca",
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
#' ind_liqSeca(
#' indicador  = "Liquidez Seca",
#' periodo    = dadosAlpha$ano,
#' cxEquiv    = dadosAlpha$cxEquiv,
#' estoque    = dadosAlpha$estoque,
#' ctaRecCP   = dadosAlpha$ctaRecCP,
#' outAtvCirc = dadosAlpha$outAtvCirc,
#' fornec     = dadosAlpha$fornec,
#' dividasCP  = dadosAlpha$dividasCP,
#' outPasCirc = dadosAlpha$outPasCirc,
#' atvTotal   = dadosAlpha$atvTotal,
#' plot = F)
#'
#' @export

ind_liqSeca <- function(
    indicador = "Liq Seca", periodo = 2019:2020, cxEquiv = c(8,10),
    estoque = c(150,200), ctaRecCP = c(400, 300), outAtvCirc = c(1, 3),
    fornec = c(50, 20), dividasCP = c(30, 40), outPasCirc = c(10, 8),
    atvTotal = c(900,800), plot = T){

  ratio <-
    ((cxEquiv + estoque + ctaRecCP + outAtvCirc) - estoque) /
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


