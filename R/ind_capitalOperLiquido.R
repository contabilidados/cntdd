#' Capital Operacional Líquido (COL)
#'
#' @description
#' Essa função calcula o Capital Operacional Líquido de forma simplificada, pois
#' não calcula o ativo e passivo operacional. Faz uma simplificação pela
#' soma do patrimônio líquido com a dívida de curto e longo prazo, deduzidos do
#' valor dos investimentos, cujo resultado é semelhante.
#'
#' @details
#' Apresenta como resultado uma lista com 5 itens:
#'
#' 1. **Gráfico** se o parâmetro `plot` for `TRUE` ou `T`, mostra um gráfico com a
#' evolução do capital operacional líquido da empresa durante os períodos. Se for
#' `FALSE` ou `F`, o gráfico não é apresentado;
#'
#' 2.  **Contas** que corresponde ao banco de dados com as contas informadas para
#' cálculo do indicador;
#'
#' 3.  **Índice** o capital operacional líquido dos periodos informados;
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
#' Ao acessar, fazer busca pelo nome da função `ind_capitalOperLiquido`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param indicador Um vetor tipo character com o nome do indicador
#' @param periodo Vetor numérico indicando o período da análise
#' @param patLiq Vetor com os valores do patrimônio líquido da empresa
#' @param investimentos Vetor com os valores dos investimentos da empresa
#' @param dividasCP Vetor com os valores de empréstimos e financiamentos de
#' curto prazo (Passivo Circulante)
#' @param dividasLP Vetor com os valores de empréstimos e financiamentos de
#' longo prazo (Passivo Não Circulante)
#' @param atvTotal Vetor com os valores do Ativo Total
#' @param plot Mostra gráfico? (TRUE/FALSE)
#' @param relatorio Se `TRUE`, Mostra relatório do indicador. Se `FALSE`,
#' mostra apenas o vetor com resultados do indicador (TRUE/FALSE)
#'
#' @examples
#' library(cntdd)
#'
#' ind_capitalOperLiquido(
#'  indicador = "Liq Seca",
#'  periodo = 2021:2022,
#'  patLiq = c(150,200),
#'  investimentos = c(5,3),
#'  dividasCP = c(30, 40),
#'  dividasLP = c(60, 90),
#'  atvTotal = c(500,400),
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

ind_capitalOperLiquido <-
  function(
    indicador = "Liq Seca",
    periodo = (year(Sys.Date())-2):(year(Sys.Date())-1),
    patLiq = c(150,200),
    investimentos = c(5,3),
    dividasCP = c(30, 40),
    dividasLP = c(60, 90),
    atvTotal = c(500,400),
    plot = T,
    relatorio = T
    ){

  ratio <-
    patLiq - investimentos + dividasCP + dividasLP

  dt <- data.frame(Periodo = periodo, ratio = ratio)
  names(dt)[2] <- indicador

  if(plot & relatorio){
    print(ind_plots(dt, indicador))
  }

  dtGeral <-
    data.frame(
      periodo     = periodo,
      patLiq     = patLiq,
      investimentos     = investimentos,
      dividasCP   = dividasCP,
      dividasLP   = dividasLP,
      atvTotal    = atvTotal
    ) %>%
    mutate(
      ratio = ratio
    ) %>%
    arrange(periodo) %>%
    mutate(
      AH.patLiq    = round(patLiq / lag(patLiq) - 1, 4),
      AH.investimentos    = round(investimentos / lag(investimentos) - 1, 4),
      AH.dividasCP  = round(dividasCP / lag(dividasCP) - 1, 4),
      AH.dividasLP  = round(dividasLP / lag(dividasLP) - 1, 4),
      AH.atvTotal  = round(atvTotal / lag(atvTotal) - 1, 4),
      AV.patLiq    = patLiq / atvTotal,
      AV.investimentos    = investimentos / atvTotal,
      AV.dividasCP  = dividasCP / atvTotal,
      AV.dividasLP  = dividasLP / atvTotal
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
