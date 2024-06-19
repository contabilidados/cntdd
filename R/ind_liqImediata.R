#' Índice de Liquidez Imediata
#'
#' @description
#' Essa função calcula o Índice de Liquidez Imediata baseado em vetores relativos
#' ao caixa e equivalentes de caixa e às contas de passivo circulante da empresa.
#'
#' @details
#' Apresenta como resultado uma lista com 5 itens:
#'
#' 1. **Gráfico** se o parâmetro `relatorio` for `TRUE` ou `T`, mostra um gráfico com a
#' evolução do ativo cíclico da empresa durante os períodos. Se for `FALSE` ou `F`,
#' o gráfico não é apresentado;
#'
#' 2.  **Contas** que corresponde ao banco de dados com as contas informadas para
#' cálculo do indicador;
#'
#' 3.  **Índice** o índice de Liquidez Imediata dos períodos informados;
#'
#' 4.  **Análise Vertical** Análise Vertical das contas informadas no item 2.
#' Contas de resultado terão suas análises verticais em relação à receita total e
#' contas patrimoniais terão suas análises verticais em relação ao ativo total;
#'
#' 5.  **Análise Horizontal** Análise Horizontal das contas informadas no item 2.
#'
#' Todos os itens da lista são bancos de dados no formato tibble que podem ser
#' usados individualmente durante o processo de análise de dados.
#'
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.com.br/>.
#' Ao acessar, fazer busca pelo nome da função `ind_liqImediata`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param indicador Um vetor tipo character com o nome do indicador
#' @param periodo Vetor numérico indicando o período da análise
#' @param cxEquiv Vetor com os valores do caixa e equivalentes de caixa (Ativo Circulante)
#' @param fornec Vetor com os valores de fornecedores (Passivo Circulante)
#' @param dividasCP Vetor com os valores de empréstimos e financiamentos de curto prazo (Passivo Circulante)
#' @param outPasCirc Vetor com os valores de outros passivos circulantes (Passivo Circulante)
#' @param atvTotal Vetor com os valores do Ativo Total
#' @param relatorio Se `TRUE`, Mostra relatório do indicador. Se `FALSE`, mostra
#' apenas o vetor com resultados do indicador (TRUE/FALSE)
#' @param titulo Título do gráfico
#' @param subtitulo Subtítulo do gráfico
#' @param rodape Rodapé do gráfico
#' @param corFundo Cor de fundo para os valores (Padrão: Laranja)
#' @param corLinhaTendencia Cor da linha de tendência entre os valores (Padrão: Laranja)
#' @param tamanhoValores Tamanho da fonte dos valores apresentados (Padrão: 6)
#' @param tamanhoTempo Tamanho da fonte dos rótulos relativo aos períodos de tempo (Padrão: 10)
#' @param tamanhoVariavel Tamanho da fonte do texto relativo à variável analisada (Padrão: 4)
#' @param tamanhoTitulo Tamanho da fonte do título (Padrão: 14)
#' @param tamanhoSubTitulo Tamanho da fonte do subtítulo (Padrão: 10)
#' @param tamanhoRodape Tamanho da fonte do rodapé (Padrão: 8)
#' @param corRodape Cor da fonte do rodapé (Padrão: Cinza)
#'
#' @examples
#' library(cntdd)
#'
#' ind_liqImediata(
#'  indicador = "Liquidez Imediata",
#'  periodo = 2021:2022,
#'  cxEquiv = c(8,10),
#'  fornec = c(50, 20),
#'  dividasCP = c(30, 40),
#'  outPasCirc = c(10, 8),
#'  atvTotal = c(900,800),
#'  relatorio = TRUE,
#'  titulo = "Evolucao da Liquidez Imediata",
#'  subtitulo = "",
#'  rodape = "",
#'  corFundo = "orange",
#'  corLinhaTendencia = "orange",
#'  tamanhoValores = 6,
#'  tamanhoTempo = 10,
#'  tamanhoVariavel = 4,
#'  tamanhoTitulo = 14,
#'  tamanhoSubTitulo = 10,
#'  tamanhoRodape = 8,
#'  corRodape = "gray"
#'  )
#'
#' @importFrom CGPfunctions newggslopegraph
#' @import dplyr
#' @import tidyr
#' @importFrom lubridate year
#' @importFrom stats na.omit
#' @export

ind_liqImediata <-
  function(
    indicador = "Liquidez Imediata",
    periodo = (year(Sys.Date())-2):(year(Sys.Date())-1),
    cxEquiv = c(8,10),
    fornec = c(50, 20),
    dividasCP = c(30, 40),
    outPasCirc = c(10, 8),
    atvTotal = c(900,800),
    relatorio = T,
    titulo = "Evolucao da Liquidez Imediata",
    subtitulo = "",
    rodape = paste0("@", year(Sys.Date()), " contabiliDados"),
    corFundo = "orange",
    corLinhaTendencia = "orange",
    tamanhoValores = 6,
    tamanhoTempo = 10,
    tamanhoVariavel = 4,
    tamanhoTitulo = 14,
    tamanhoSubTitulo = 10,
    tamanhoRodape = 8,
    corRodape = "gray"
    ){

  ratio <-
    round(cxEquiv  / (fornec + dividasCP + outPasCirc), 4)

  dtGraf <-
    data.frame(
      periodo = factor(periodo, ordered = T),
      indicador = indicador,
      ratio = ratio
    )

  if(relatorio){
    grafico <-
      newggslopegraph(
        dtGraf, periodo, ratio, indicador,
        Title = titulo,
        SubTitle = subtitulo,
        Caption = rodape,
        DataLabelFillColor = corFundo,
        DataLabelPadding = 0.4, DataLabelLineSize = 0,
        WiderLabels = F, LineThickness = 1, LineColor = corLinhaTendencia,
        XTextSize = tamanhoTempo, YTextSize = tamanhoVariavel, TitleTextSize = tamanhoTitulo,
        SubTitleTextSize = tamanhoSubTitulo, CaptionTextSize = tamanhoRodape,
        TitleJustify = "left", SubTitleJustify = "left",
        CaptionJustify = "right", DataTextSize = tamanhoValores,
        ThemeChoice = "bw"
      ) +
      theme(
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(color = corRodape)
      )
  } else {
    grafico <- NULL
  }

  dtGeral <-
    data.frame(
      periodo     = periodo,
      cxEquiv     = cxEquiv,
      fornec      = fornec,
      dividasCP   = dividasCP,
      outPasCirc  = outPasCirc,
      atvTotal    = atvTotal
    ) %>%
    mutate(
      ratio = ratio
    ) %>%
    arrange(periodo) %>%
    mutate(
      AH.CxEquiv    = round(cxEquiv / lag(cxEquiv) - 1, 4),
      AH.fornec     = round(fornec / lag(fornec) - 1, 4),
      AH.dividasCP  = round(dividasCP / lag(dividasCP) - 1, 4),
      AH.outPasCirc = round(outPasCirc / lag(outPasCirc) - 1, 4),
      AH.atvTotal = round(atvTotal / lag(atvTotal) - 1, 4),
      AV.cxEquiv    = cxEquiv / atvTotal,
      AV.fornec     = fornec / atvTotal,
      AV.dividasCP  = dividasCP / atvTotal,
      AV.outPasCirc = outPasCirc / atvTotal
    ) %>%
    rename_with(~ indicador, all_of("ratio"))

  dtGeral %>%
    select(periodo:atvTotal) %>%
    pivot_longer(cols = -periodo, names_to = "conta", values_to = "value") %>%
    na.omit() %>%
    pivot_wider(names_from = "periodo", values_from = "value") -> contas

  dtGeral %>%
    select(periodo, {{indicador}}) %>%
    pivot_longer(cols = -periodo, names_to = "conta", values_to = "value") %>%
    na.omit() %>%
    pivot_wider(names_from = "periodo", values_from = "value") -> ratio

  dtGeral %>%
    select(periodo, starts_with("AV.")) %>%
    pivot_longer(cols = -periodo, names_to = "conta", values_to = "value") %>%
    mutate(value = eval(parse(text = "showPercent(value)"))) %>%
    na.omit() %>%
    pivot_wider(names_from = "periodo", values_from = "value") -> bdAV

  dtGeral %>%
    select(periodo, starts_with("AH.")) %>%
    pivot_longer(cols = -periodo, names_to = "conta", values_to = "value") %>%
    mutate(value = eval(parse(text = "showPercent(value)"))) %>%
    na.omit() %>%
    pivot_wider(names_from = "periodo", values_from = "value") -> bdAH


  listaRelatorio <-
    list(
      Contas = contas, `Indice` = ratio,
      `Analise Vertical` = bdAV,
      `Analise Horizontal` = bdAH,
      plot = grafico)

  apenasVetor <-
    ratio %>%
    pivot_longer(-1, names_to = 'ano', values_to = "valor") %>%
    select(3) %>% pull() %>% round(3)
  names(apenasVetor) <- names(ratio)[-1]

  if(relatorio){
    return(listaRelatorio)
  } else {
    return(apenasVetor)
  }

}

