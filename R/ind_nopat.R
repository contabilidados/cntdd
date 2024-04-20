#' Lucro Operacional Liquido após os impostos (NOPAT)
#'
#' @description
#' Essa função calcula o Lucro Operacional Liquido após os impostos (NOPAT -
#' Net operating profit after tax).
#'
#' @details
#' Apresenta como resultado uma lista com 5 itens:
#'
#' 1. **Gráfico** se o parâmetro `plot` for `TRUE` ou `T`, mostra um gráfico com a
#' evolução do NOPAT da empresa durante os períodos. Se for `FALSE` ou `F`,
#' o gráfico nao é apresentado;
#'
#' 2.  **Contas** que corresponde ao banco de dados com as contas informadas para
#' cálculo do indicador;
#'
#' 3.  **Índice** o NOPAT dos períodos informados;
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
#' Ao acessar, fazer busca pelo nome da funcao `ind_nopat`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param indicador Um vetor tipo character com o nome do indicador
#' @param periodo Vetor numérico indicando o período da análise
#' @param ebit Vetor com os valores do Lucro antes dos Juros e Impostos (EBIT) da empresa
#' @param ir Vetor com o percentual do imposto de renda (padrão: 34%)
#' @param receitaLiquida Vetor com os valores da receita liquida da empresa
#' @param plot Mostra gráfico? (TRUE/FALSE)
#' @param relatorio Se `TRUE`, Mostra relatório do indicador. Se `FALSE`,
#' mostra apenas o vetor com resultados do indicador (TRUE/FALSE)
#'
#' @examples
#' library(cntdd)
#'
#' ind_nopat(
#'  indicador = "nopat",
#'  periodo = 2021:2022,
#'  ebit = c(150,200),
#'  receitaLiquida = c(290, 230),
#'  ir = 0.34,
#'  plot = TRUE,
#'  relatorio = TRUE
#'  )
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom lubridate year
#' @importFrom stats na.omit
#' @export

ind_nopat <-
  function(
    indicador = "nopat",
    periodo = (year(Sys.Date())-2):(year(Sys.Date())-1),
    ebit = c(150,200),
    receitaLiquida = c(290, 230),
    ir = 0.34,
    plot = T,
    relatorio = T
    ){

  ratio <-
    ebit * (1 - ir)

  dt <- data.frame(Periodo = periodo, ratio = ratio)
  names(dt)[2] <- indicador

  if(plot & relatorio){
    print(ind_plots(dt, indicador))
  }

  dtGeral <-
    data.frame(
      periodo     = periodo,
      ebit     = ebit,
      receitaLiquida     = receitaLiquida
    ) %>%
    mutate(
      ratio = ratio
    ) %>%
    arrange(periodo) %>%
    mutate(
      AH.ebit    = round(ebit / lag(ebit) - 1, 4),
      AH.receitaLiquida    = round(receitaLiquida / lag(receitaLiquida) - 1, 4),
      AV.ebit    = ebit / receitaLiquida
    ) %>%
    rename_with(~ indicador, all_of("ratio"))

  dtGeral %>%
    select(periodo:receitaLiquida) %>%
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
      `Analise Horizontal` = bdAH)

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

