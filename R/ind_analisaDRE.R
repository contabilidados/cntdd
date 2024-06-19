#' DRE Detalhada
#'
#' @description
#' Essa função ajuda na análise da Demonstração do Resultado do Exercício (DRE),
#' demonstrando os diversos tipos de lucro com sua respectiva formação.
#'
#' @details
#' O gráfico gerado nessa função dar suporte na compreensão dos conceitos de
#' Lucro Bruto, EBITDA, EBIT, NOPAT, Lucro antes dos impostos, lucro líquido e
#' lucros retidos.
#'
#' Apresenta como resultado um gráfico ou um data.frame com as contas para análise.
#' Se o parâmetro `verDados` for `FALSE` ou `F`, mostra um gráfico com a
#' formação do lucro da empresa. Se for `TRUE` ou `T`, mostra o data.frame com
#' as contas que compõem o gráfico.
#'
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.com.br/>.
#' Ao acessar, fazer busca pelo nome da função `ind_analisaDRE`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param empresa Um vetor tipo character com o nome da empresa
#' @param periodo Vetor numérico ou string indicando o período da análise
#' @param receita Vetor com os valores da receita líquida da empresa
#' @param custo Vetor com os valores do custo da empresa
#' @param despSemDep Vetor com os valores das despesas operacionais sem o valor da depreciação
#' @param depreciacao Vetor com os valores da depreciação
#' @param juros Vetor com os valores dos juros passivos
#' @param ircsll Vetor com os valores dos impostos incidentes sobre o lucro
#' @param dividendos Vetor com os valores dos dividendos propostos
#' @param aliquotaIR Alíquota do Imposto de Renda (Padrão: 0,34 ou 34%)
#' @param verDados Mostra data.frame? (TRUE/FALSE). Se `TRUE`, Mostra o data.frame. Se `FALSE`,
#' mostra o gráfico (Padrão: `F`)
#' @param fontBar Valor para ajuste do tamanho da fonte do texto sobre as barras (Padrão: 4)
#' @param colorBar1 String (texto) com a especificação da cor da barra do tipo 1
#' @param colorBar2 String (texto) com a especificação da cor da barra do tipo 2
#' @param colorBar3 String (texto) com a especificação da cor da barra do tipo 3
#' @param colorBar4 String (texto) com a especificação da cor da barra do tipo 4
#' @param ajustLabel Valor para ajuste da localização do texto sobre as barras (Padrão: 10)
#'
#' @examples
#' library(cntdd)
#'
#' ind_analisaDRE(
#' empresa = "Empresa",
#' periodo = 2023,
#' receita = 80,
#' custo = 20,
#' despSemDep = 20,
#' depreciacao = 5,
#' juros = 2,
#' ircsll = 1,
#' dividendos = 2,
#' aliquotaIR = 0.34,
#' verDados = FALSE,
#' fontBar = 4,
#' colorBar1 = "#9ecae1",
#' colorBar2 = "#3182bd",
#' colorBar3 = "#e34a33",
#' colorBar4 = "#fee8c8",
#' ajustLabel = 10
#' )
#'
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @importFrom lubridate year
#' @importFrom tidyr pivot_longer
#' @export


ind_analisaDRE <-
  function(
    empresa = "Empresa", periodo = (year(Sys.Date())-1),
    receita = 150, custo = 30, despSemDep = 20, depreciacao = 10,
    juros = 35, ircsll = 15, dividendos = 10, aliquotaIR = 0.34, verDados = F,
    fontBar = 3, colorBar1 = "#9ecae1", colorBar2 = "#3182bd",
    colorBar3 = "#e34a33", colorBar4 = "#fee8c8", ajustLabel = 8
  ) {

    lucroBruto = receita - custo
    ebitda = lucroBruto - despSemDep
    ebit = ebitda - depreciacao
    ir_nopat = ebit*aliquotaIR
    nopat = ebit*(1-aliquotaIR)
    lucroAntesImpostos = ebit - juros
    lucroliquido = lucroAntesImpostos - ircsll
    lucrosRetidos = lucroliquido - dividendos

    dt <-
      data.frame(
        receita, custo, lucroBruto, despSemDep, ebitda, depreciacao,
        ebit, ir_nopat, nopat, juros, lucroAntesImpostos, ircsll, lucroliquido,
        dividendos, lucrosRetidos
      ) %>%
      pivot_longer(
        everything(),
        names_to = "contas", values_to = "valor"
      ) %>%
      mutate(
        grupos =
          case_when(
            contas %in% "receita" ~ "Receita",
            contas %in% c("custo", "lucroBruto") ~ "Lucro Bruto",
            contas %in% c("despSemDep", "ebitda") ~ "EBITDA",
            contas %in% c("depreciacao", "ebit") ~ "EBIT",
            contas %in% c("ir_nopat", "nopat") ~ "NOPAT",
            contas %in% c("juros", "lucroAntesImpostos") ~ "LAIR",
            contas %in% c("ircsll", "lucroliquido") ~ "Lucro Liquido",
            contas %in% c("dividendos", "lucrosRetidos") ~ "Lucros Retidos"
          )
      )

    dt$grupos <-
      factor(
        dt$grupos,
        levels =
          c(
            "Receita", "Lucro Bruto", "EBITDA", "EBIT",
            "NOPAT", "LAIR", "Lucro Liquido", "Lucros Retidos"
            ),
        ordered = T
        )

    dt$contas <- factor(
      dt$contas,
      levels =
        c("receita", "custo", "lucroBruto",
          "despSemDep", "ebitda", "depreciacao",
          "ebit", "ir_nopat", "nopat", "juros",
          "lucroAntesImpostos", "ircsll", "lucroliquido",
          "dividendos", "lucrosRetidos"
          ),
      ordered = T)

    dre <-
      dt %>% dplyr::select(contas, valor)

    textos <-
      dt %>%
      filter(
        contas %in% c(
          "receita", "lucroBruto", "ebitda", "ebit", "nopat",
          "lucroAntesImpostos", "lucroliquido"
        )
      )


    vrNOPAT <- textos$valor[textos$grupos == "EBIT"]
    vrRECEITA <- textos$valor[textos$grupos == "Receita"]

    textos <-
      textos %>%
      mutate(valor = if_else(contas == "nopat", vrNOPAT, valor)) %>%
      mutate(
        barra = valor + vrRECEITA * ajustLabel / 100
        ) %>%
      dplyr::select(barra) %>% pull() %>% floor()


    plot <-
      ggplot(
      data = dt,
      aes(x = grupos, y = valor, fill = contas)
    ) +
      geom_bar(
        stat="identity"
      ) +
      scale_fill_manual(
        values = c(
          colorBar1,
          colorBar2, colorBar1, colorBar2, colorBar1,
          colorBar2, colorBar1, colorBar3, colorBar4,
          colorBar2, colorBar1, colorBar2, colorBar1,
          colorBar3, colorBar4
        )
      ) +
      labs(
        title = "Etapas do Lucro na DRE",
        subtitle = paste0(empresa, " (", periodo, ")"),
        caption = paste0("@", lubridate::year(Sys.Date()), " contabiliDados")
      ) +
      xlab("") + ylab("") +
      scale_x_discrete(
        labels =
          c(
            "Receita", "Lucro\nBruto", "EBITDA",
            "EBIT", "NOPAT", "Lucro antes\ndos Impostos",
            "Lucro\nLiquido", "Lucros\nRetidos")
      ) +
      scale_y_continuous(
        labels = scales::label_number(accuracy=0.1, scale_cut=scales::cut_short_scale())
      ) +
      annotate("text", x = "Lucro Bruto", y = textos[1],
               label = c("Custo"), size = fontBar, color = "#3182bd", angle = 0) +
      annotate("text", x = "EBITDA", y = textos[2],
               label = c("Despesa sem\n Depreciacao"), size = fontBar, color = "#3182bd", angle = 0) +
      annotate("text", x = "EBIT", y = textos[3],
               label = c("Depreciacao"), size = fontBar, color = "#3182bd", angle = 0) +
      annotate("text", x = "NOPAT", y = textos[4],
               label = c("IR/CSLL\n(34%)"), size = fontBar, color = "#e34a33", angle = 0) +
      annotate("text", x = "LAIR", y = textos[5],
               label = c("Juros"), size = fontBar, color = "#3182bd", angle = 0) +
      annotate("text", x = "Lucro Liquido", y = textos[6],
               label = c("IR/CSLL"), size = fontBar, color = "#3182bd", angle = 0) +
      annotate("text", x = "Lucros Retidos", y = textos[7],
               label = c("Dividendos"), size = fontBar, color = "#e34a33", angle = 0) +
      theme_classic() +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 14, colour = "#636363"),
        plot.caption = element_text(colour = "#bdbdbd"),
        axis.text.y = element_text(colour = "#636363"),
        axis.text.x = element_text(face = "bold")
      )

    if(verDados){
      return(dre)
    } else {
      return(plot)
    }
  }

globalVariables(c("contas", "barra", "grupos", "valor"))

