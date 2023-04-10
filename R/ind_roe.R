#' Índice de Retorno sobre o Patrimônio Líquido (ROE)
#'
#' @description
#'
#' Essa função calcula o Índice de Retorno sobre o Patrimônio Líquido baseado em vetores relativos às
#' contas de lucro líquido e patrimônio líquido. Apresenta como resultado uma
#' lista com 5 itens:
#'
#' 1. **Gráfico** se o parâmetro `plot` for `TRUE` ou `T`, mostra um gráfico com a
#' evolução do ROE da empresa durante os períodos. Se for `FALSE` ou `F`,
#' o gráfico não é apresentado;
#'
#' 2.  **Contas** que corresponde ao banco de dados com as contas informadas para
#' cálculo do indicador;
#'
#' 3.  **Índice** o índice de Retorno sobre o Patrimônio Líquido dos períodos informados;
#'
#' 4.  **Análise Vertical** Análise Vertical das contas informadas no item 1;
#'
#' 5.  **Análise Horizontal** Análise Horizontal das contas informadas no item 1.
#'
#' Todos os itens da lista são bancos de dados no formato tibble que podem ser
#' usados individualmente durante o processo de análise de dados.
#'
#' @param indicador Um vetor tipo character com o nome do indicador
#' @param periodo Vetor numérico indicando o período da análise
#' @param lucLiq Vetor com os valores do lucro líquido da empresa 
#' @param patLiq Vetor com os valores do patrimônio líquido 
#' @param plot Mostra gráfico? (TRUE/FALSE)
#'
#' @examples
#'
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: \href{http://contabilidados.com.br}{(Acesse Aqui)}. Ao acessar, fazer busca
#' pelo nome da função `ind_roe`
#' 
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: \email{contabilidados@@ufersa.edu.br}
#' Siga-nos no Instagram: \href{https://www.instagram.com/contabilidados/}{@contabilidados}
#'
#'
#' @import ggplot2
#' @import readxl
#' @import dplyr
#' @import tidyr
#' @export

ind_roe <-
  function(
    indicador = "Roe",
    periodo = 2019:2020,
    lucLiq = c(8,10),
    patLiq = c(150,200),
    receitaTotal = c(300,500),
    passivoTotal = c(400,500),
    plot = T
    ){

  ratio <-
    lucLiq/patLiq

  dt <- data.frame(Periodo = periodo, ratio = ratio)
  names(dt)[2] <- indicador
  


  if(plot){
    print(ind_plots(dt, indicador))
  }

  dtGeral <-
    data.frame(
      periodo     = periodo,
      lucLiq     = lucLiq,
      patLiq     = patLiq,
      receitaTotal = receitaTotal,
      passivoTotal = passivoTotal
    ) %>%
    mutate(
      {{indicador}} := ratio
    ) %>%
    arrange(periodo) %>%
    mutate(
      AH.lucLiq    = round(lucLiq / dplyr::lag(lucLiq) - 1, 4),
      AH.patLiq    = round(patLiq / dplyr::lag(patLiq) - 1, 4),
      AV.lucLiq    = lucLiq / receitaTotal,
      AV.patLiq   = patLiq / passivoTotal
    )

  
    dtGeral %>%
      dplyr::select(periodo:passivoTotal) %>%
      pivot_longer(cols = -periodo, names_to = "conta", values_to = "value") %>%
      na.omit() %>%
    pivot_wider(names_from = periodo, values_from = value) -> contas

  dtGeral %>%
    dplyr::select(periodo, {{indicador}}) %>%
    pivot_longer(cols = -periodo, names_to = "conta", values_to = "value") %>%
    na.omit() %>%
    pivot_wider(names_from = periodo, values_from = value) -> ratio

  dtGeral %>%
    dplyr::select(periodo, starts_with("AV.")) %>%
    pivot_longer(cols = -periodo, names_to = "conta", values_to = "value") %>%
    mutate(value = showPercent(value)) %>%
    na.omit() %>%
    pivot_wider(names_from = periodo, values_from = value) -> bdAV

  dtGeral %>%
    dplyr::select(periodo, starts_with("AH.")) %>%
    pivot_longer(cols = -periodo, names_to = "conta", values_to = "value") %>%
    mutate(value = showPercent(value)) %>%
    na.omit() %>%
    pivot_wider(names_from = periodo, values_from = value) -> bdAH


  return(list(Contas = contas, `Índice` = ratio,
              `Análise Vertical` = bdAV,
              `Análise Horizontal` = bdAH)) 

  }



        