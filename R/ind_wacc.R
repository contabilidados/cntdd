#' Custo Médio de Capital Ponderado (WACC ou CMPC)
#'
#' @description
#'
#' Essa função calcula o Custo Médio de Capital Ponderado baseado em vetores relativos ao
#' custo da dívida, peso da dívida e o benefício fiscal do imposto de renda,
#' custo de capital do sócio, peso do capital do sócio. 
#' Apresenta como resultado uma #' lista com 5 itens:
#'
#' 1. **Gráfico** se o parâmetro `plot` for `TRUE` ou `T`, mostra um gráfico com a
#' evolução do Custo Médio de Capital Ponderado dos sócios e banco. Se for `FALSE` ou `F`,
#' o gráfico não é apresentado;
#'
#' 2.  **Contas** que corresponde ao banco de dados com as contas informadas para
#' cálculo do indicador;
#'
#' 3.  **Índice** o índice de Custo Médio de Capital Ponderado dos períodos informados;
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
#' @param empFinCP Vetor com os valores de empréstimos e financiamentos de curto prazo (Passivo Circulante)
#' @param empFinLP Vetor com os valores de empréstimos e financiamentos de longo prazo (Passivo Não Circulante)
#' @param patLiq Vetor com os valores do Patrimônio Líquido
#' @param despJuros Vetor com os valores das despesas com juros (DRE/Notas Explicativas)
#' @param receita Vetor com os valores da receita líquida (Demonstração de Resultado - DRE)
#' @param atvTotal Vetor com os valores do Ativo Total
#' @param ir Valor relativo à alíquota do Imposto de Renda
#' @param ke Valor relativo ao custo de capital do sócio
#' @param beta Valor relativo ao beta (medida de volatilidade ou risco sistemático) de um título ou portifólio comparado ao mercado como um todo
#' @param rf Valor relativo à taxa de juros livre de risco ou risco baixo
#' @param rm Valor relativo à taxa de remuneração paga pelo mercado
#' @param capm Se `TRUE`, cálcula `ke` pela fórmula `rf + beta * (rm - rf)`. Se `FALSE`, cálcula pelo parâmetro ke 
#' @param plot Se `TRUE`, Mostra gráfico do indicador. Se `FALSE`, não mostra o gráfico (TRUE/FALSE)
#'
#' @examples
#'
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: \href{http://contabilidados.com.br}{(http://contabilidados.com.br)}. Ao acessar, fazer busca
#' pelo nome da função `ind_wacc`
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

ind_wacc <-
  function(
    indicador = "WACC",
    periodo = 2019:2020,
    empFinCP = c(200, 300),
    empFinLP = c(100, 200),
    patLiq = c(400, 600),
    despJuros = c(30, 40),
    receita = c(1000, 1200),
    atvTotal = c(4000, 5000),
    ir = 0.34,
    capm = T,
    ke = 0.15,
    beta = 1.3,
    rf = 0.07,
    rm = 0.13,
    plot = T
    ){
    
    divida <- empFinCP + empFinLP
    wd <- round(divida / (divida + patLiq), 4)
    we <- 1 - wd
    kd <- despJuros / divida
    
    if(capm){
      ke <- rf + beta * (rm - rf)
    } else{
        ke <- ke
      }
    
  ratio <-
    wd * kd * (1 - ir) + we * ke

  dt <- data.frame(Periodo = periodo, ratio = ratio)
  names(dt)[2] <- indicador

  if(plot){
    print(ind_plots(dt, indicador))
  }

  dtGeral <-
    data.frame(
      periodo     = periodo,
      empFinCP     = empFinCP,
      empFinLP     = empFinLP,
      patLiq      = patLiq,
      despJuros  = despJuros,
      receita    = receita,
      atvTotal    = atvTotal
    ) %>%
    mutate(
      {{indicador}} := ratio
    ) %>%
    arrange(periodo) %>%
    mutate(
      AH.empFinCP    = round(empFinCP / dplyr::lag(empFinCP) - 1, 4),
      AH.empFinLP    = round(empFinLP / dplyr::lag(empFinLP) - 1, 4),
      AH.patLiq     = round(patLiq / dplyr::lag(patLiq) - 1, 4),
      AH.despJuros  = round(despJuros / dplyr::lag(despJuros) - 1, 4),
      AH.receita     = round(receita / dplyr::lag(receita) - 1, 4),
      AH.atvTotal   = round(atvTotal / dplyr::lag(atvTotal) - 1, 4),
      AV.empFinCP    = empFinCP / atvTotal,
      AV.empFinLP    = empFinLP / atvTotal,
      AV.patLiq     = patLiq / atvTotal,
      AV.despJuros = despJuros / receita
    )

  dtGeral %>%
    dplyr::select(periodo:atvTotal) %>%
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
