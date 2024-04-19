#' Custo Medio de Capital Ponderado (WACC ou CMPC)
#'
#' @description
#' Essa funcao calcula o Custo Medio de Capital Ponderado baseado em vetores relativos ao
#' custo da divida, peso da divida e o beneficio fiscal do imposto de renda,
#' custo de capital do sócio, peso do capital do sócio.
#'
#' @details
#' Apresenta como resultado uma #' lista com 5 itens:
#'
#' 1. **Grafico** se o parametro `plot` for `TRUE` ou `T`, mostra um grafico com a
#' evolucao do Custo Medio de Capital Ponderado pelos valores investidos pelos
#' socios e banco. Se for `FALSE` ou `F`, o grafico nao e apresentado;
#'
#' 2.  **Contas** que corresponde ao banco de dados com as contas informadas para
#' calculo do indicador;
#'
#' 3.  **Indice** o indice de Custo Medio de Capital Ponderado dos periodos informados;
#'
#' 4.  **Analise Vertical** Analise Vertical das contas informadas no item 1. Conta de resultado
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
#' @param empFinCP Vetor com os valores de emprestimos e financiamentos de curto prazo (Passivo Circulante)
#' @param empFinLP Vetor com os valores de emprestimos e financiamentos de longo prazo (Passivo Nao Circulante)
#' @param patLiq Vetor com os valores do Patrimônio Liquido
#' @param despJuros Vetor com os valores das despesas com juros (DRE/Notas Explicativas)
#' @param receita Vetor com os valores da receita liquida (Demonstracao de Resultado - DRE)
#' @param atvTotal Vetor com os valores do Ativo Total
#' @param ir Valor relativo a aliquota do Imposto de Renda
#' @param ke Valor relativo ao custo de capital do sócio
#' @param beta Valor relativo ao beta (medida de volatilidade ou risco sistematico) de um titulo ou portifólio comparado ao mercado como um todo
#' @param rf Valor relativo a taxa de juros livre de risco ou risco baixo
#' @param rm Valor relativo a taxa de remuneracao paga pelo mercado
#' @param capm Se `TRUE`, calcula `ke` pela fórmula `rf + beta * (rm - rf)`. Se `FALSE`, calcula pelo parametro ke
#' @param plot Se `TRUE`, Mostra grafico do indicador. Se `FALSE`, nao mostra o grafico (TRUE/FALSE)
#' @param relatorio Se `TRUE`, Mostra relatorio do indicador. Se `FALSE`, mostra apenas o vetor com resultados do indicador (TRUE/FALSE)
#'
#' @import ggplot2
#' @import readxl
#' @import dplyr
#' @import tidyr
#' @export

ind_wacc <-
  function(
    indicador = "WACC",
    periodo = (year(Sys.Date())-2):(year(Sys.Date())-1),
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
    plot = T,
    relatorio = T
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

  if(plot & relatorio){
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
      ratio = ratio
    ) %>%
    arrange(periodo) %>%
    mutate(
      AH.empFinCP    = round(empFinCP / lag(empFinCP) - 1, 4),
      AH.empFinLP    = round(empFinLP / lag(empFinLP) - 1, 4),
      AH.patLiq     = round(patLiq / lag(patLiq) - 1, 4),
      AH.despJuros  = round(despJuros / lag(despJuros) - 1, 4),
      AH.receita     = round(receita / lag(receita) - 1, 4),
      AH.atvTotal   = round(atvTotal / lag(atvTotal) - 1, 4),
      AV.empFinCP    = empFinCP / atvTotal,
      AV.empFinLP    = empFinLP / atvTotal,
      AV.patLiq     = patLiq / atvTotal,
      AV.despJuros = despJuros / receita
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

  dtGeral %>%
    transmute(
      periodo,
      pesoDivida = wd,
      pesoCapitalProprio = we
    ) %>%
    pivot_longer(cols = -periodo, names_to = "conta", values_to = "value") %>%
    pivot_wider(names_from = "periodo", values_from = "value") %>%
    mutate(
      conta = c("Peso da Divida (wd)", "Peso do Capital Proprio (we)")
    ) -> pesos


  listaRelatorio <-
    list(Contas = contas, `Indice` = ratio,
         Pesos = pesos,
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


