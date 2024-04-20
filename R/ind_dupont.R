#' Análise Dupont (ROE desmembrado)
#'
#' @description
#' Essa função calcula o Índice de Retorno sobre o Patrimônio Líquido, baseado
#' em vetores relativos as contas de Lucro Liquido, receita, Ativo Total e
#' endividamento.
#'
#' @details
#' Apresenta como resultado uma lista com 5 itens:
#'
#' 1. **Gráfico** se o parâmetro `plot` for `TRUE` ou `T`, mostra um gráfico com a
#' evolução do ROE da empresa durante os períodos. Se for `FALSE` ou `F`,
#' o gráfico não é apresentado;
#'
#' 2.  **Contas** que corresponde ao banco de dados com as contas informadas para
#' cálculo do indicador;
#'
#' 3.  **Índice** o índice de Retorno sobre o patrimônio líquido dos períodos informados;
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
#' Ao acessar, fazer busca pelo nome da função `ind_dupont`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param indicador Um vetor tipo character com o nome do indicador
#' @param periodo Vetor numérico indicando o período da análise
#' @param lucLiq Vetor com os valores do lucro líquido da empresa
#' @param patLiq vetor com os valores do patrimônio líquido da empresa
#' @param ativoTotal Vetor com os valores do ativo total da empresa
#' @param receitaTotal Vetor com os valores da receita total da empresa
#' @param plot Mostra gráfico? (TRUE/FALSE)
#' @param relatorio Se `TRUE`, Mostra relatório do indicador. Se `FALSE`, mostra
#' apenas o vetor com resultados do indicador (TRUE/FALSE)
#'
#' @examples
#' library(cntdd)
#'
#' ind_dupont(
#'  indicador = "ROE_Dupont",
#'  periodo = 2021:2022,
#'  lucLiq = c(80,100),
#'  patLiq = c(180,190),
#'  ativoTotal = c(200,300),
#'  receitaTotal = c(300,500),
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

ind_dupont <-
  function(
    indicador = "ROE_Dupont",
    periodo = (year(Sys.Date())-2):(year(Sys.Date())-1),
    lucLiq = c(80,100),
    patLiq = c(180,190),
    ativoTotal = c(200,300),
    receitaTotal = c(300,500),
    plot = T,
    relatorio = T
    ){

  margemLuc <-
    lucLiq / receitaTotal

  giroAtivo <-
    receitaTotal / ativoTotal

  alavancagemFin <-
    ativoTotal / patLiq

  ROA <-
    margemLuc * giroAtivo

  ratio <-
    ROA * alavancagemFin

  dt <- data.frame(Periodo = periodo, ratio = ratio)
  names(dt)[2] <- indicador



  if(plot & relatorio){
    print(ind_plots(dt, indicador))
  }

  dtGeral <-
    data.frame(
      periodo     = periodo,
      receitaTotal = receitaTotal,
      lucLiq      = lucLiq,
      patLiq      = patLiq,
      ativoTotal    = ativoTotal,
      margemLuc   = margemLuc,
      giroAtivo   = giroAtivo,
      ROA        = ROA
    ) %>%
    mutate(
      ratio = ratio
    ) %>%
    arrange(periodo) %>%
    mutate(
      AH.receitaTotal   = round(receitaTotal / lag(receitaTotal) - 1, 4),
      AH.lucLiq   = round(lucLiq / lag(lucLiq) - 1, 4),
      AH.patLiq    = round(patLiq / lag(patLiq) - 1, 4),
      AH.ativoTotal    = round(ativoTotal / lag(ativoTotal) - 1, 4),
      AH.margemLuc    = round(margemLuc / lag(margemLuc) - 1, 4),
      AH.giroAtivo    = round(giroAtivo / lag(giroAtivo) - 1, 4),
      AH.ROA    = round(ROA / lag(ROA) - 1, 4),
      AV.receitaTotal    = receitaTotal / receitaTotal,
      AV.lucLiq    = lucLiq / receitaTotal,
      AV.patLiq   = patLiq/ ativoTotal
    ) %>%
    rename_with(~ indicador, all_of("ratio"))


    dtGeral %>%
      select(periodo:ROA) %>%
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

