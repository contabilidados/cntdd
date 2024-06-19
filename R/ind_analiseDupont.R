#' ROE desmembrado (Dupont)
#'
#' @description
#' Essa função demonstra o desmembramento do ROE de acordo com a análise Dupont.
#'
#' @details
#' Apresenta como resultado uma lista com 2 itens:
#'
#' 1. **Gráfico** se o parâmetro `plot` for `TRUE` ou `T`, mostra um gráfico com a
#' evolução do ROE da empresa e seus componentes, conforme análise Dupont, durante
#' os períodos. Se for `FALSE` ou `F`, o gráfico não é apresentado;
#'
#' 2.  **Contas** que corresponde ao banco de dados com as contas informadas para
#' cálculo do indicador. o banco de dado está no formato tibble que pode ser
#' usados individualmente durante o processo de análise de dados.
#'
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.com.br/>.
#' Ao acessar, fazer busca pelo nome da função `ind_dupont`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param intervalo Vetor numérico indicando o período da análise
#' @param patrimonioLiquido Vetor com os valores do patrimônio líquido da empresa
#' @param ativoTotal Vetor com os valores do ativo total da empresa
#' @param receita Vetor com os valores da receita líquida da empresa
#' @param lucroLiquido Vetor com os valores do lucro líquido da empresa
#' @param apenasDados Se `TRUE`, Mostra apenas o banco de dados. Se `FALSE`, mostra
#' o banco de dados e o gráfico com a análise dupont. (TRUE/FALSE)
#'
#' @seealso [ind_roe()], [ind_roa()], [ind_nivelEndividamento()], [ind_giro()], [ind_ros()], [ind_pmre()]
#'
#' @examples
#' library(cntdd)
#'
#' ind_analiseDupont(
#'  intervalo = 2009:2011,
#'  patrimonioLiquido = c(200, 150, 175),
#'  ativoTotal = c(300, 500, 600),
#'  receita = c(500, 450, 400),
#'  lucroLiquido = c(8, 10, 14),
#'  apenasDados = FALSE
#'  )
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom lubridate year
#' @importFrom stats na.omit
#' @importFrom stats df
#' @importFrom CGPfunctions newggslopegraph
#' @importFrom ggpubr ggarrange
#' @importFrom ggpubr annotate_figure
#' @importFrom ggpubr text_grob
#' @export

ind_analiseDupont <-
  function(
    intervalo = 2009:2011,
    patrimonioLiquido = c(200, 150, 175),
    ativoTotal = c(300, 500, 600),
    receita = c(500, 450, 400),
    lucroLiquido = c(8, 10, 14),
    apenasDados = F
  ){

    evolGraf <-
      function(indicador, .corLabel, titulo){

        plot <-
          newggslopegraph(dataframe = subset(df, indice %in% indicador),
                          Times = ano,
                          Measurement = valor,
                          Grouping = indice,
                          DataLabelFillColor = .corLabel,
                          DataLabelPadding = 0.4,
                          DataLabelLineSize = 0,
                          WiderLabels = F,
                          Title = titulo,
                          SubTitle = "",
                          Caption = "",
                          LineThickness = .5,
                          LineColor = "black",
                          XTextSize = 8,    # Size of the times
                          YTextSize = 4,     # Size of the groups
                          TitleTextSize = 8,
                          SubTitleTextSize = 8,
                          CaptionTextSize = 8,
                          TitleJustify = "left",
                          SubTitleJustify = "left",
                          CaptionJustify = "left",
                          DataTextSize = 3.5,
                          ThemeChoice = "bw"
          ) +
          theme(
            plot.background = element_rect(fill = .corLabel),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
        return(plot)
      }



    rbind(
      ind_nivelEndividamento(
        periodo = intervalo,
        patLiq = patrimonioLiquido,
        atvTotal = ativoTotal,
        indicador = "Endiv", relatorio = T)$Indice,
      ind_giro(
        periodo = intervalo,
        receitaLiquida = receita,
        atvTotal = ativoTotal,
        indicador = "Giro", relatorio = T)$Indice,
      ind_ros(
        periodo = intervalo,
        receitaTotal = receita,
        lucro = lucroLiquido,
        indicador = "ROS", relatorio = T)$Indice
    ) %>%
      pivot_longer(-1, names_to = "ano", values_to = "valor") %>%
      pivot_wider(names_from = "conta", values_from = "valor") %>%
      mutate(ROA = .data$Giro * .data$ROS, ROE = .data$ROA * .data$Endiv) %>%
      pivot_longer(-ano, names_to = "indice", values_to = "valor") %>%
      mutate(valor = round(.data$valor, 2)) -> df

    dt <-
      df %>%
      pivot_wider(names_from = "ano", values_from = "valor")

    graf.roe <- evolGraf("ROE", "aquamarine", "Rentabilidade dos Acionistas")
    graf.roa <- evolGraf("ROA", "azure", "Rentabilidade da Empresa")
    graf.Endiv <- evolGraf("Endiv", "lightgoldenrod1", "Financiamento de Terceiros")
    graf.ros <- evolGraf("ROS", "azure2", "Rentabilidade das Vendas")
    graf.Giro <- evolGraf("Giro", "azure3", "Eficiencia")

    ggarrange(
      graf.roe, nrow = 3,
      ggarrange(graf.roa, graf.Endiv, nrow = 1),
      ggarrange(graf.ros, graf.Giro,  nrow = 1, ncol = 3)
    ) -> result

    grafico <-
      annotate_figure(
        result,
        top = text_grob(
          "Analise Dupont",
          color = "black",
          face = "bold", size = 14)
        )

    listaRelatorio <-
      list(
        Contas = dt, plot = grafico)

    if(apenasDados){
      return(dt)
    } else {
      return(listaRelatorio)
    }

    gc()

  }

globalVariables(c("indice", "ano"))
