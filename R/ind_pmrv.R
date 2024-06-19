#' Prazo Médio de Rececimento de Vendas
#'
#' @description
#' Essa função calcula o Prazo Médio de Rececimento de Vendas da empresa
#'
#' @details
#' Representa a quantidade média de dias que a empresa concede de prazo para
#' seus clientes.
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
#' 3.  **Índice** o PMRV dos períodos informados;
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
#' Ao acessar, fazer busca pelo nome da função `ind_pmrv`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param indicador Um vetor tipo character com o nome do indicador
#' @param periodo Vetor numérico indicando o período da análise
#' @param clientesInicial Vetor com os valores do contas a receber (clientes) do período anterior ao da análise
#' @param clientesFinal Vetor com os valores do contas a receber (clientes) do período da análise
#' @param receitaBruta Vetor com os valores da receita bruta (disponível da DVA) da empresa
#' @param atvTotal Vetor com os valores do Ativo Total
#' @param qdeDiasAno Quantidade de dias do ano. (Padrão: 365)
#' @param giro Se `TRUE` mostrará a quantidade de giros anuais (útil na análise
#' Dupont), Se `FALSE` (padrão), mostrará a quantidade média de dias para
#' recebimento das vendas.
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
#' @seealso [ind_analiseCiclo()]
#'
#' @examples
#' library(cntdd)
#'
#' ind_pmrv(
#'  indicador = "PMRV",
#'  periodo = 2021:2022,
#'  clientesInicial = c(1712, 1430),
#'  clientesFinal = c(3260, 2120),
#'  receitaBruta = c(7200, 5630),
#'  atvTotal = c(10900, 9800),
#'  qdeDiasAno = 365,
#'  giro = FALSE,
#'  relatorio = TRUE,
#'  titulo = "Evolucao do PMRV",
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

ind_pmrv <-
  function(
    indicador = "PMRV",
    periodo = (year(Sys.Date())-2):(year(Sys.Date())-1),
    clientesInicial = c(1712, 1430),
    clientesFinal = c(3260, 2120),
    receitaBruta = c(7200, 5630),
    atvTotal = c(10900, 9800),
    qdeDiasAno = 365,
    giro = F,
    relatorio = T,
    titulo = "Evolucao do PMRV",
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

    if(giro){
      ratio <-
        round(receitaBruta /((clientesInicial + clientesFinal)/2), 4)
    } else {
      ratio <-
        round(((clientesInicial + clientesFinal)/2) / receitaBruta * qdeDiasAno, 4)
    }


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
      clientesInicial = clientesInicial,
      clientesFinal   = clientesFinal,
      receitaBruta    = receitaBruta,
      atvTotal        = atvTotal
    ) %>%
    mutate(
      ratio = ratio
    ) %>%
    arrange(periodo) %>%
    mutate(
      AH.clientesInicial = round(clientesInicial / lag(clientesInicial) - 1, 4),
      AH.clientesFinal   = round(clientesFinal / lag(clientesFinal) - 1, 4),
      AH.receitaBruta    = round(receitaBruta / lag(receitaBruta) - 1, 4),
      AH.atvTotal        = round(atvTotal / lag(atvTotal) - 1, 4),
      AV.clientesInicial = clientesInicial / atvTotal,
      AV.clientesFinal   = clientesFinal / atvTotal
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

