#' Capital Circulante Líquido (CCL)
#'
#' @description
#' Essa função calcula o Capital Circulante Líquido que corresponde a diferença
#' entre o ativo circulante e o passivo circulante.
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
#' 3.  **Índice** o CCL dos períodos informados;
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
#' Ao acessar, fazer busca pelo nome da função `ind_capitalCircLiquido`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param indicador Um vetor tipo character com o nome do indicador
#' @param periodo Vetor numérico indicando o período da análise
#' @param ativoCirculante Vetor com os valores do ativo circulante da empresa
#' @param passivoCirculante Vetor com os valores do passivo circulante da empresa
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
#' ind_capitalCircLiquido(
#'  indicador = "Capital Circulante Liquido",
#'  periodo = 2021:2022,
#'  ativoCirculante = c(150, 180),
#'  passivoCirculante = c(350, 220),
#'  atvTotal = c(500,400),
#'  relatorio = TRUE,
#'  titulo = "Evolucao do Capital Circulante Líquido",
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

ind_capitalCircLiquido <-
  function(
    indicador = "Capital Circulante Liquido",
    periodo = (year(Sys.Date())-2):(year(Sys.Date())-1),
    ativoCirculante = c(150, 180),
    passivoCirculante = c(350, 220),
    atvTotal = c(500,400),
    relatorio = T,
    titulo = "Evolucao do Capital Circulante Liquido",
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
    ativoCirculante - passivoCirculante

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
      ativoCirculante   = ativoCirculante,
      passivoCirculante = passivoCirculante,
      atvTotal          = atvTotal
    ) %>%
    mutate(
      ratio = ratio
    ) %>%
    arrange(periodo) %>%
    mutate(
      AH.ativoCirculante    = round(ativoCirculante / lag(ativoCirculante) - 1, 4),
      AH.passivoCirculante   = round(passivoCirculante / lag(passivoCirculante) - 1, 4),
      AH.atvTotal  = round(atvTotal / lag(atvTotal) - 1, 4),
      AV.ativoCirculante    = ativoCirculante / atvTotal,
      AV.passivoCirculante    = passivoCirculante / atvTotal
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

