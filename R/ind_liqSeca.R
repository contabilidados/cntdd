#' Índice de Liquidez Seca
#'
#' @details
#'
#' Essa função calcula o Índice de Liquidez Seca baseado em vetores relativos às
#' contas de ativo circulante e passivo circulante. Apresenta como resultado uma
#' lista com 5 itens:
#'
#' 1. **Gráfico** se o parâmetro `plot` for `TRUE` ou `T`, mostra um gráfico com a
#' evolução da Liquidez Seca da empresa durante os períodos. Se for `FALSE` ou `F`,
#' o gráfico não é apresentado;
#'
#' 2.  **Contas** que corresponde ao banco de dados com as contas informadas para
#' cálculo do indicador;
#'
#' 3.  **Índice** o índice de Liquidez Seca dos períodos informados;
#'
#' 4.  **Análise Vertical** Análise Vertical das contas informadas no item 1;
#'
#' 5.  **Análise Horizontal** Análise Horizontal das contas informadas no item 1.
#'
#' Todos os itens da lista são bancos de dados no formato tibble que podem ser
#' usados individualmente durante o processo de análise de dados.
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
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: \href{http://contabilidados.com.br}{(Acesse Aqui)}. Ao acessar, fazer busca
#' pelo nome da função `ind_liqSeca`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: \email{contabilidados@@ufersa.edu.br}
#' Siga-nos no Instagram: \href{https://www.instagram.com/contabilidados/}{@contabilidados}
#'
#' @import ggplot2
#' @import readxl
#' @import dplyr
#' @import tidyr
#' @export

ind_liqSeca <-
  function(
    indicador = "Liq Seca",
    periodo = 2019:2020,
    cxEquiv = c(8,10),
    estoque = c(150,200),
    ctaRecCP = c(400, 300),
    outAtvCirc = c(1, 3),
    fornec = c(50, 20),
    dividasCP = c(30, 40),
    outPasCirc = c(10, 8),
    atvTotal = c(900,800),
    plot = T
    ){

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
