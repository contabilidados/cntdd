#' Ciclo Operacional e Financeiro
#'
#' @description
#' Essa função dá suporte na análise dos ciclos operacional e financeiro das empresas.
#'
#' @details
#' Apresenta como resultado um gráfico de barras com a representçaão gráfica
#' do prazo médio de renovação dos estoques, prazo médio de recebimento de vendas,
#' prazo médio de pagamento de compras, ciclo operacional e ciclo financeiro.
#'
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.quarto.pub/>.
#' Ao acessar, fazer busca pelo nome da função `ind_dupont`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param periodo Vetor numérico indicando o período da análise
#' @param pmrv Vetor com os valores do prazo médio do recebimento de vendas
#' @param pmre Vetor com os valores do prazo médio da renovação dos estoques
#' @param pmpc Vetor com os valores do prazo médio do pagamento de compras
#' @param periodoGrafico Vetor com um dos periodos informado no parâmetro `periodo` a ser considerado na análise
#'
#' @seealso [ind_pmre()], [ind_pmrv()], [ind_pmpc()], [ind_cicloOperacional()], [ind_cicloFinanceiro()]
#'
#' @examples
#' library(cntdd)
#'
#' ind_analiseCiclo(
#'  periodo = 2022:2023,
#'  pmrv = c(60, 75),
#'  pmre = c(45, 20),
#'  pmpc = c(50, 100),
#'  periodoGrafico = 2023
#'  )
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom lubridate year
#' @export

ind_analiseCiclo <-
  function(
    periodo = (year(Sys.Date())-2):(year(Sys.Date())-1),
    pmrv = c(60, 75),
    pmre = c(45, 20),
    pmpc = c(50, 100),
    periodoGrafico = year(Sys.Date())-1
  ){

    dt <-
      data.frame(
        periodo = rep(periodo, 3),
        indices = sort(rep(c("PMRV", "PMRE", "PMPC"), length(periodo)), decreasing = T),
        grupoIndices = c(
          rep("Ciclo Operacional", 4), "PMPC", "PMPC"
          ),
        valores = c(pmrv, pmre, pmpc)
    )

    dt$indices <- factor(dt$indices, levels = c("PMRV", "PMRE", "PMPC"))
    dt$grupoIndices <- factor(dt$grupoIndices, levels = c("PMPC", "Ciclo Operacional"))

    dt <- dt[periodo == periodoGrafico,]


    cicloOperac <-
      dt$valores[dt$indices == "PMRE"] + dt$valores[dt$indices == "PMRV"]
    cicloFinanc <- cicloOperac - dt$valores[dt$indices == "PMPC"]

    rotulo <- if_else(
      cicloFinanc >= 0,
      paste0("Ciclo Financeiro Positivo: Empresa financia as atividades operacionais por ", cicloFinanc, "dd\nEfeito negativo para o fluxo de caixa"),
      paste0("Ciclo Financeiro Negativo: Terceiros financiam as atividades operacionais por ", cicloFinanc, "dd\nEfeito positivo para o fluxo de caixa")
    )

    if(cicloFinanc < 0){
      rangeBarras <- c(cicloOperac, cicloOperac - cicloFinanc)
      localTexto <- "Ciclo Operacional"
    } else {
      rangeBarras <- c(cicloOperac - cicloFinanc, cicloOperac)
      localTexto <- "PMPC"
    }


    plot <-
      ggplot(
        data = dt,
        aes(x = grupoIndices, y = valores, fill = indices,
            label = paste0(valores,"dd\n",indices))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("#7F8E39", "#42858C", "#E48F1B")) +
      xlab(NULL) + ylab(NULL) +
      geom_hline(yintercept= rangeBarras,
                 linetype="dashed", color = "#D33B44", linewidth=1) +
      geom_text(position = position_stack(vjust = .5), color = "#FFFFFF", size = 8) +
      labs(
        title = "Analise do Ciclo Financeiro",
        subtitle = paste0("Ciclo Operacional: ", cicloOperac, "dd\n", rotulo),
        caption = paste0("@", year(Sys.Date()), " contabiliDados")
        ) +
      theme_minimal() + coord_flip() +
      theme(
        legend.position = "none",
        plot.title = element_text(colour = "#1d3554", face = "bold", size = 16),
        plot.subtitle = element_text(colour = "#1d3554", size = 12),
        plot.caption = element_text(colour = "#97b043"),
        axis.text.x = element_blank(), axis.text.y = element_blank()) +
      annotate("label", angle = 90,
               x = localTexto,
               y = mean(rangeBarras), hjust=.5,
               label = "Ciclo Financeiro", color = "#1d3554", fill = "#DFE07C"
               )

    return(plot)

  }

globalVariables(c("valores", "grupoIndices", "indices"))
