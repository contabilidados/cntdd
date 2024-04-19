#' Indice de Retorno sobre o Ativo (ROA)
#'
#' @description
#'
#' Essa funcao calcula o Indice de Retorno sobre o Ativo baseado em vetores relativos as
#' contas de Lucro e Ativo Total. O usuario definira se usara lucro liquido ou
#' lucro operacional, conforme literatura de interesse.
#'
#' @details
#' Apresenta como resultado uma lista com 5 itens:
#'
#' 1. **Grafico** se o parametro `plot` for `TRUE` ou `T`, mostra um grafico com a
#' evolucao do ROIC da empresa durante os periodos. Se for `FALSE` ou `F`,
#' o grafico nao e apresentado;
#'
#' 2.  **Contas** que corresponde ao banco de dados com as contas informadas para
#' calculo do indicador;
#'
#' 3.  **Indice** o indice de Retorno sobre o Capital Investido dos periodos informados;
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
#' @param lucro Vetor com os valores do lucro liquido da organizacao
#' @param patLiq vetor com os valores do Patrimônio Liquido da organizacao
#' @param ativoCirculante Vetor com os valores do ativo circulante da organizacao
#' @param ativoNCirculante Vetor com os valores do ativo nao circulante da organizacao
#' @param receitaTotal Vetor com os valores da receita liquida da organizacao
#' @param plot Mostra grafico? (TRUE/FALSE)
#' @param relatorio Se `TRUE`, Mostra relatorio do indicador. Se `FALSE`, mostra apenas o vetor com resultados do indicador (TRUE/FALSE)
#'
#' @import ggplot2
#' @import readxl
#' @import dplyr
#' @import tidyr
#' @export


ind_roa <-
  function(
    indicador = "ROA",
    periodo = (year(Sys.Date())-2):(year(Sys.Date())-1),
    lucro = c(8,10),
    patLiq = c(100,150),
    ativoCirculante = c(200,300),
    ativoNCirculante = c(250,500),
    receitaTotal = c(300,500),
    plot = T,
    relatorio = T
    ){

  ativoTotal <-
    ativoCirculante+ativoNCirculante

  ratio <-
    lucro/ativoTotal

  dt <- data.frame(Periodo = periodo, ratio = ratio)
  names(dt)[2] <- indicador



  if(plot & relatorio){
    print(ind_plots(dt, indicador))
  }

  dtGeral <-
    data.frame(
      periodo     = periodo,
      receitaTotal = receitaTotal,
      lucro      = lucro,
      patLiq      = patLiq,
      ativoCirculante = ativoCirculante,
      ativoNCirculante = ativoNCirculante
    ) %>%
    mutate(
      ratio = ratio
    ) %>%
    arrange(periodo) %>%
    mutate(
      AH.lucro   = round(lucro / lag(lucro) - 1, 4),
      AH.ativoCirculante    = round(ativoCirculante / lag(ativoCirculante) - 1, 4),
      AH.ativoNCirculante    = round(ativoNCirculante / lag(ativoNCirculante) - 1, 4),
      AV.lucro    = lucro / receitaTotal,
      AV.ativoCirculante   = ativoCirculante / ativoTotal,
      AV.ativoNCirculante   = ativoNCirculante / ativoTotal
    ) %>%
    rename_with(~ indicador, all_of("ratio"))


    dtGeral %>%
      select(periodo:ativoNCirculante) %>%
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
