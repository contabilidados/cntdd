#' Ciclo Operacional
#'
#' @description
#' Essa função calcula o Ciclo Operacional da empresa
#'
#' @details
#' Representa a quantidade média de dias entre a estocagem e o recebimento das
#' vendas da empresa.
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
#' 3.  **Índice** o Ciclo Operacional dos períodos informados;
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
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.quarto.pub/>.
#' Ao acessar, fazer busca pelo nome da função `ind_cicloOperacional`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param indicador Um vetor tipo character com o nome do indicador
#' @param periodo Vetor numérico indicando o período da análise
#' @param estoqueInicial Vetor com os valores do Estoque do período anterior ao da análise
#' @param estoqueFinal Vetor com os valores do Estoque do período da análise
#' @param clientesInicial Vetor com os valores do contas a receber (clientes) do período anterior ao da análise
#' @param clientesFinal Vetor com os valores do contas a receber (clientes) do período da análise
#' @param receitaBruta Vetor com os valores da receita bruta (disponível da DVA) da empresa
#' @param cmv Vetor com os valores do custo das mercadorias vendidas da empresa
#' @param receitaLiquida Vetor com os valores da receita líquida da empresa
#' @param atvTotal Vetor com os valores do Ativo Total
#' @param qdeDiasAno Quantidade de dias do ano. (Padrão: 365)
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
#' @param corRodape Cor da fonte do rodapé (Padrão: Cinza)2min
#'
#' @seealso [ind_pmre()], [ind_pmrv()], [ind_pmpc()]
#'
#' @examples
#' library(cntdd)
#'
#' ind_cicloOperacional(
#'  indicador = "Ciclo Operacional",
#'  periodo = 2021:2022,
#'  clientesInicial = c(2320, 2032),
#'  clientesFinal = c(4327, 3013),
#'  estoqueInicial = c(1712, 1430),
#'  estoqueFinal = c(3260, 2120),
#'  cmv = c(3500, 2781),
#'  receitaLiquida = c(7200, 5630),
#'  atvTotal = c(10900, 9800),
#'  qdeDiasAno = 365,
#'  relatorio = TRUE,
#'  titulo = "Evolucao do Ciclo Operacional",
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
#' @importFrom lubridate year
#' @importFrom stats na.omit
#' @export

ind_cicloOperacional <-
  function(
    indicador = "Ciclo Operacional",
    periodo = (year(Sys.Date())-2):(year(Sys.Date())-1),
    estoqueInicial = c(2320, 2032),
    estoqueFinal = c(4327, 3013),
    clientesInicial = c(1712, 1430),
    clientesFinal = c(3260, 2120),
    receitaBruta = c(7500, 6000),
    cmv = c(3500, 2781),
    receitaLiquida = c(7200, 5630),
    atvTotal = c(10900, 9800),
    qdeDiasAno = 365,
    relatorio = T,
    titulo = "Evolucao do Ciclo Operacional",
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
      ind_pmre(
        relatorio = F, giro = F,
        periodo = periodo,
        qdeDiasAno = qdeDiasAno,
        estoqueInicial = estoqueInicial,
        estoqueFinal = estoqueFinal,
        cmv = cmv,
        receitaLiquida = receitaLiquida, atvTotal = atvTotal
        ) +
      ind_pmrv(
        relatorio = F, giro = F,
        periodo = periodo,
        qdeDiasAno = qdeDiasAno,
        clientesInicial = clientesInicial,
        clientesFinal = clientesFinal,
        receitaBruta = receitaBruta,
        atvTotal = atvTotal
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
      periodo         = periodo,
      estoqueInicial = estoqueInicial,
      estoqueFinal   = estoqueFinal,
      clientesInicial = clientesInicial,
      clientesFinal   = clientesFinal,
      receitaBruta = receitaBruta,
      cmv = cmv,
      receitaLiquida    = receitaLiquida,
      atvTotal        = atvTotal
    ) %>%
    mutate(
      ratio = ratio
    ) %>%
    arrange(periodo) %>%
    mutate(
      AH.clientesInicial = round(clientesInicial / lag(clientesInicial) - 1, 4),
      AH.clientesFinal   = round(clientesFinal / lag(clientesFinal) - 1, 4),
      AH.estoqueInicial = round(estoqueInicial / lag(estoqueInicial) - 1, 4),
      AH.estoqueFinal   = round(estoqueFinal / lag(estoqueFinal) - 1, 4),
      AH.receitaBruta    = round(receitaBruta / lag(receitaBruta) - 1, 4),
      AH.cmv  = round(cmv / lag(cmv) - 1, 4),
      AH.receitaLiquida    = round(receitaLiquida / lag(receitaLiquida) - 1, 4),
      AH.atvTotal        = round(atvTotal / lag(atvTotal) - 1, 4),
      AV.clientesInicial = clientesInicial / atvTotal,
      AV.clientesFinal   = clientesFinal / atvTotal,
      AV.estoqueInicial = estoqueInicial / atvTotal,
      AV.estoqueFinal   = estoqueFinal / atvTotal,
      AV.cmv = cmv / receitaLiquida
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
