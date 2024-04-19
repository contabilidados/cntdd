#' Passivo Oneroso
#'
#' @description
#' Essa funcao calcula o passivo oneroso que corresponde a passivos que exigem
#' juros como remuneracao do capital empregado. Essencialmente, sao atribuidos
#' os valores de emprestimos e financiamento de curto e longo prazo.
#'
#' @details
#' Apresenta como resultado uma lista com 5 itens:
#'
#' 1. **Grafico** se o parametro `plot` for `TRUE` ou `T`, mostra um grafico com a
#' evolucao do passivo oneroso da empresa durante os periodos. Se for `FALSE` ou `F`,
#' o grafico nao e apresentado;
#'
#' 2.  **Contas** que corresponde ao banco de dados com as contas informadas para
#' calculo do indicador;
#'
#' 3.  **Indice** o passivo oneroso dos periodos informados;
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
#' Ao acessar, fazer busca pelo nome da funcao `ind_passivoOneroso`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param indicador Um vetor tipo character com o nome do indicador
#' @param periodo Vetor numerico indicando o periodo da analise
#' @param empFinLP Vetor com os valores de emprestimos e financiamentos de curto prazo da empresa
#' @param empFinCP Vetor com os valores de emprestimos e financiamentos de longo prazo da empresa
#' @param atvTotal Vetor com os valores do Ativo Total
#' @param plot Mostra grafico? (TRUE/FALSE)
#' @param relatorio Se `TRUE`, Mostra relatorio do indicador. Se `FALSE`, mostra apenas o vetor com resultados do indicador (TRUE/FALSE)
#'
#' @import ggplot2
#' @import readxl
#' @import dplyr
#' @import tidyr
#' @export

ind_passivoOneroso <-
  function(
    indicador = "passivoOneroso",
    periodo = (year(Sys.Date())-2):(year(Sys.Date())-1),
    empFinCP = c(150,200),
    empFinLP = c(230,190),
    atvTotal = c(500,400),
    plot = T,
    relatorio = T
    ){

  ratio <-
    empFinCP + empFinLP

  dt <- data.frame(Periodo = periodo, ratio = ratio)
  names(dt)[2] <- indicador

  if(plot & relatorio){
    print(ind_plots(dt, indicador))
  }

  dtGeral <-
    data.frame(
      periodo     = periodo,
      empFinCP   = empFinCP,
      empFinLP   = empFinLP,
      atvTotal    = atvTotal
    ) %>%
    mutate(
      ratio = ratio
    ) %>%
    arrange(periodo) %>%
    mutate(
      AH.empFinCP    = round(empFinCP / lag(empFinCP) - 1, 4),
      AH.empFinLP    = round(empFinLP / lag(empFinLP) - 1, 4),
      AH.atvTotal  = round(atvTotal / lag(atvTotal) - 1, 4),
      AV.empFinCP    = empFinCP / atvTotal,
      AV.empFinLP    = empFinLP / atvTotal
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


