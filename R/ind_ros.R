#' Índice de Retorno sobre as Vendas (ROS)
#'
#' @description
#'
#' Essa função calcula o Índice de Retorno sobre as Vendas baseado em vetores relativos às
#' contas de Lucro e Receitas. O usuário definirá se usará lucro líquido ou
#' outra medida de lucro, conforme literatura de interesse. Da mesma forma,
#' escolherá entre receita líquida ou bruta. A literatura, especialmente sobre
#' análise Dupont, trata essa medida como **margem**.
#'
#' @details
#' Apresenta como resultado uma lista com 5 itens:
#'
#' 1. **Gráfico** se o parâmetro `plot` for `TRUE` ou `T`, mostra um gráfico com a
#' evolução do ROS da empresa durante os períodos. Se for `FALSE` ou `F`,
#' o gráfico não é apresentado;
#'
#' 2.  **Contas** que corresponde ao banco de dados com as contas informadas para
#' cálculo do indicador;
#'
#' 3.  **Índice** o ROS dos períodos informados;
#'
#' 4.  **Análise Vertical** Análise Vertical das contas informadas no item 2.
#' Contas de resultado terão suas analises verticais em relação à receita total e
#' contas patrimoniais terão suas analises verticais em relação ao ativo total;
#'
#' 5.  **Análise Horizontal** Análise Horizontal das contas informadas no item 2.
#'
#' Todos os itens da lista são bancos de dados no formato tibble que podem ser
#' usados individualmente durante o processo de análise de dados.
#'
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.quarto.pub/>.
#' Ao acessar, fazer busca pelo nome da função `ind_ros`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param indicador Um vetor tipo character com o nome do indicador
#' @param periodo Vetor numérico indicando o período da análise
#' @param lucro Vetor com os valores do lucro da empresa
#' @param receitaTotal Vetor com os valores da receita líquida da empresa
#' @param plot Mostra gráfico? (TRUE/FALSE)
#' @param relatorio Se `TRUE`, Mostra relatório do indicador. Se `FALSE`,
#' mostra apenas o vetor com resultados do indicador (TRUE/FALSE)
#'
#' @examples
#' library(cntdd)
#'
#' ind_ros(
#'  indicador = "ROS",
#'  periodo = 2021:2022,
#'  lucro = c(8,10),
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

ind_ros <-
  function(
    indicador = "ROS",
    periodo = (year(Sys.Date())-2):(year(Sys.Date())-1),
    lucro = c(8,10),
    receitaTotal = c(300,500),
    plot = T,
    relatorio = T
    ){

  ratio <-
    lucro / receitaTotal

  dt <- data.frame(Periodo = periodo, ratio = ratio)
  names(dt)[2] <- indicador

  grafico <- ind_plots(dt, indicador)

  if(plot & relatorio){
    print(grafico)
  }

  dtGeral <-
    data.frame(
      periodo     = periodo,
      lucro      = lucro,
      receitaTotal = receitaTotal
    ) %>%
    mutate(
      ratio = ratio
    ) %>%
    arrange(periodo) %>%
    mutate(
      AH.lucro   = round(lucro / lag(lucro) - 1, 4),
      AH.receitaTotal    = round(receitaTotal / lag(receitaTotal) - 1, 4),
      AV.lucro    = lucro / receitaTotal
    ) %>%
    rename_with(~ indicador, all_of("ratio"))

    dtGeral %>%
      select(periodo:receitaTotal) %>%
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
