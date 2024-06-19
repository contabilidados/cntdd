#' Necessidade de Capital de Giro (NCG)
#'
#' @description
#' Essa função calcula a necessidade de capital de giro da empresa.
#'
#' @details
#' Trata-se da diferença entre ativo cíclico e passivo cíclico. Portanto, se
#' concentra nos ativos de curto prazo voltados às operações específicas da
#' atividade da empresa
#'
#' Apresenta como resultado uma lista com 5 itens:
#'
#' 1. **Gráfico** se o parâmetro `relatorio` for `TRUE` ou `T`, mostra um gráfico com a
#' evolução do ativo cíclico da empresa durante os períodos. Se for `FALSE` ou `F`,
#' o gráfico não é apresentado;
#'
#' 2.  **Contas** que corresponde ao banco de dados com as contas informadas para
#' cálculo do indicador;
#'
#' 3.  **Índice** a NCG dos períodos informados;
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
#' Ao acessar, fazer busca pelo nome da função `ind_necessidadeCapGiro`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param indicador Um vetor tipo character com o nome do indicador
#' @param periodo Vetor numérico indicando o período da análise
#' @param atvCirculante Vetor com os valores do ativo circulante da empresa
#' @param cxEquiv Vetor com os valores da conta caixa e equivalentes de caixa da empresa
#' @param aplicFinanc Vetor com os valores das aplicações financeiras de curto prazo da empresa
#' @param outAtvCircLiqImed Vetor com os valores de outros ativos circulantes de liquidez imediata da empresa
#' @param passivoCirculante Vetor com os valores do passivo circulante da empresa
#' @param empFinCP Vetor com os valores de empréstimos e financiamentos de curto prazo da empresa
#' @param outPasCircOneroso Vetor com os valores de outros passivos onerosos de curto prazo da empresa
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
#' @seealso [ind_ativoCiclico()], [ind_ativoFinanceiro()], [ind_passivoCiclico], [ind_passivoFinanceiro]
#'
#' @examples
#' library(cntdd)
#'
#' ind_necessidadeCapGiro(
#'  indicador = "Necessidade de Capital de Giro",
#'  periodo = 2022:2023,
#'  atvCirculante = c(150, 180),
#'  cxEquiv = c(230, 180),
#'  aplicFinanc = c(20, 40),
#'  outAtvCircLiqImed = c(0, 0),
#'  passivoCirculante = c(400, 350),
#'  empFinCP = c(120, 210),
#'  outPasCircOneroso = c(2, 10),
#'  atvTotal = c(900, 800),
#'  relatorio = TRUE,
#'  titulo = "Evolucao da Necessidade de Capital de Giro",
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
#' )
#'
#' @importFrom CGPfunctions newggslopegraph
#' @import dplyr
#' @import tidyr
#' @importFrom lubridate year
#' @importFrom stats na.omit
#' @export

ind_necessidadeCapGiro <-
  function(
    indicador = "Necessidade de Capital de Giro",
    periodo = (year(Sys.Date())-2):(year(Sys.Date())-1),
    atvCirculante = c(150, 180),
    cxEquiv = c(230, 180),
    aplicFinanc = c(20, 40),
    outAtvCircLiqImed = c(0, 0),
    passivoCirculante = c(400, 350),
    empFinCP = c(120, 210),
    outPasCircOneroso = c(2, 10),
    atvTotal = c(900, 800),
    relatorio = T,
    titulo = "Evolucao da Necessidade de Capital de Giro",
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
    ind_ativoCiclico(
      indicador = "acic",
      periodo = periodo,
      atvCirculante = atvCirculante,
      cxEquiv = cxEquiv,
      aplicFinanc = aplicFinanc,
      outAtvCircLiqImed = outAtvCircLiqImed,
      atvTotal = atvTotal, relatorio = F
    ) - ind_passivoCiclico(
      indicador = "pcic",
      periodo = periodo,
      passivoCirculante = passivoCirculante,
      empFinCP = empFinCP,
      outPasCircOneroso = outPasCircOneroso,
      atvTotal = atvTotal, relatorio = F,
    )

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
      periodo           = periodo,
      atvCirculante = atvCirculante,
      cxEquiv = cxEquiv,
      aplicFinanc = aplicFinanc,
      outAtvCircLiqImed = outAtvCircLiqImed,
      passivoCirculante = passivoCirculante,
      empFinCP = empFinCP,
      outPasCircOneroso = outPasCircOneroso,
      atvTotal = atvTotal
      ) %>%
    mutate(
      ratio = ratio
    ) %>%
    arrange(periodo) %>%
    mutate(
      AH.atvCirculante    = round(atvCirculante / lag(atvCirculante) - 1, 4),
      AH.cxEquiv   = round(cxEquiv / lag(cxEquiv) - 1, 4),
      AH.aplicFinanc   = round(aplicFinanc / lag(aplicFinanc) - 1, 4),
      AH.outAtvCircLiqImed   = round(outAtvCircLiqImed / lag(outAtvCircLiqImed) - 1, 4),
      AH.passivoCirculante   = round(passivoCirculante / lag(passivoCirculante) - 1, 4),
      AH.empFinCP   = round(empFinCP / lag(empFinCP) - 1, 4),
      AH.outPasCircOneroso   = round(outPasCircOneroso / lag(outPasCircOneroso) - 1, 4),
      AH.atvTotal  = round(atvTotal / lag(atvTotal) - 1, 4),
      AV.atvCirculante    = atvCirculante / atvTotal,
      AV.cxEquiv    = cxEquiv / atvTotal,
      AV.aplicFinanc    = aplicFinanc / atvTotal,
      AV.outAtvCircLiqImed    = outAtvCircLiqImed / atvTotal,
      AV.passivoCirculante    = passivoCirculante / atvTotal,
      AV.empFinCP    = empFinCP / atvTotal,
      AV.outPasCircOneroso    = outPasCircOneroso / atvTotal
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
