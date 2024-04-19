#' Capital Operacional Liquido (COL)
#'
#' @description
#' Essa funcao calcula o Capital Operacional Liquido de forma simplificada, pois
#' nao calcula o ativo e passivo operacional, mas faz uma simplificacao pela
#' soma do patrimonio liquido com a divida de curto e longo prazo, deduzidos do
#' valor dos investimentos. Em linhas gerais, o resultado e semelhante.
#'
#' @details
#' Apresenta como resultado uma lista com 5 itens:
#'
#' 1. **Grafico** se o parametro `plot` for `TRUE` ou `T`, mostra um grafico com a
#' evolucao capital operacional líquido da empresa durante os periodos. Se for `FALSE` ou `F`,
#' o grafico nao e apresentado;
#'
#' 2.  **Contas** que corresponde ao banco de dados com as contas informadas para
#' calculo do indicador;
#'
#' 3.  **Indice** o capital operacional liquido dos periodos informados;
#'
#' 4.  **Analise Vertical** Analise Vertical das contas informadas no item 1. Contas de resultado
#' terao sua analise vertical em relacao a receita total e contas patrimoniais terao
#' sua analise vertical em relacao ao ativo total;
#'
#' 5.  **Analise Horizontal** Analise Horizontal das contas informadas no item 1.
#'
#' Todos os itens da lista sao bancos de dados no formato tibble que podem ser
#' usados individualmente durante o processo de analise de dados.
#'
#' Informacoes adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <http://contabilidados.com.br>.
#' Ao acessar, fazer busca pelo nome da funcao `ind_roa`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param indicador Um vetor tipo character com o nome do indicador
#' @param periodo Vetor numerico indicando o periodo da analise
#' @param patLiq Vetor com os valores do patrimonio liquido da empresa
#' @param investimentos Vetor com os valores dos investimentos da empresa
#' @param dividasCP Vetor com os valores de emprestimos e financiamentos de curto prazo (Passivo Circulante)
#' @param dividasLP Vetor com os valores de emprestimos e financiamentos de longo prazo (Passivo Nao Circulante)
#' @param atvTotal Vetor com os valores do Ativo Total
#' @param plot Mostra grafico? (TRUE/FALSE)
#' @param relatorio Se `TRUE`, Mostra relatorio do indicador. Se `FALSE`, mostra apenas o vetor com resultados do indicador (TRUE/FALSE)
#'
#' @import ggplot2
#' @import readxl
#' @import dplyr
#' @import tidyr
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
