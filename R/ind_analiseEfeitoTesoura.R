#' Efeito Tesoura
#'
#' @description
#' Essa função fornece dar suporte para análise do Efeito Tesoura.
#'
#' @details
#' Trata-se da diferença entre o capital circulante líquido e o ativo cíclico.
#' Quando negativa, indica a existência do efeito tesoura. Apresenta como resultado
#' um gráfico com a evoluçaõd do CCL e da NCG, evidenciando (ou não) a ocorrência
#' do efeito tesoura.
#'
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.com.br/>.
#' Ao acessar, fazer busca pelo nome da função `ind_necessidadeCapGiro`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param periodo Vetor numérico indicando o período da análise
#' @param ccl Vetor com os valores do capital circulante líquido
#' @param ncg Vetor com os valores da necessidade de capital de giro
#' @param ajusteTexto parâmetro para ajuste do saldo de tesouraria no gráfico (padrão: 0.2)
#' @param titulo Título do gráfico
#' @param subtitulo Subtítulo do gráfico
#' @param rodape Rodapé do gráfico
#'
#' @seealso [ind_capitalCircLiquido()], [ind_necessidadeCapGiro()], [ind_saldoTesouraria()]
#'
#' @examples
#' library(cntdd)
#'
#' ind_analiseEfeitoTesoura(
#' periodo = 2022:2023,
#' ccl = c(653, 127),
#' ncg = c(390, 476),
#' ajusteTexto = 0.2,
#' titulo = "Analise do Efeito Tesoura",
#' subtitulo = "Analise Dinamica do Capital de Giro",
#' rodape = ""
#' )
#'
#' @import ggplot2
#' @import ggrepel
#' @import dplyr
#' @import tidyr
#' @importFrom lubridate year
#' @importFrom stats na.omit
#' @export

ind_analiseEfeitoTesoura <-
  function(
    periodo = (year(Sys.Date())-2):(year(Sys.Date())-1),
    ccl = c(653, 127),
    ncg = c(390, 476),
    ajusteTexto = 0.2,
    titulo = "Analise do Efeito Tesoura",
    subtitulo = "Analise Dinamica do Capital de Giro",
    rodape = paste0("@", year(Sys.Date()), " contabiliDados")
    ){

    localTexto <- mean(c(ccl[length(ccl)], ncg[length(ncg)]))

    st <- ccl[length(ccl)] - ncg[length(ncg)]
    st <- scales::label_number(accuracy=0.1,  scale_cut=scales::cut_short_scale())(st)

    dt <-
      data.frame(
        periodo = periodo,
        CCL = ccl,
        NCG = ncg
      ) %>%
      pivot_longer(-periodo, names_to = "conta", values_to = "valor") %>%
      mutate(name_lab = if_else(periodo == max(periodo), conta, ""))

    dt %>%
      ggplot(aes(x = periodo, y=valor, colour = conta)) +
      geom_line(linewidth = 1) +
      scale_x_continuous(
        expand = c(0.1, 0.1),
        limits = c(min(periodo), max(periodo)),
        breaks = seq(min(periodo), max(periodo), by = 1)
      ) +
      labs(
        title = titulo,
        subtitle = subtitulo,
        caption = rodape
      ) +
      xlab("") + ylab("") +
      geom_text_repel(
        aes(color = conta, label = name_lab),
        # family = "Lato",
        fontface = "bold",
        size = 4,
        direction = "y",
        xlim = c(min(periodo), NA),
        hjust = 0,
        segment.size = .7,
        segment.alpha = .9,
        segment.linetype = "dotted",
        box.padding = .4,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20
      ) +
      scale_y_continuous(
        labels = scales::label_number(accuracy=0.1, scale_cut=scales::cut_short_scale())
      ) +
      theme_classic() +
      theme(
        legend.position = "none",
        plot.title = element_text(colour = "#1d3554", face = "bold", size = 16),
        plot.subtitle = element_text(colour = "#1d3554", size = 12),
        plot.caption = element_text(colour = "#97b043")) +
      annotate(
        "label", angle = 0,
        x = max(periodo),
        y = localTexto, vjust = ajusteTexto, size = 5,
        label = paste0("Saldo de Tesouraria\n", st), color = "#1d3554", fill = "#DFE07C"
      )

}


globalVariables(c("name_lab", "conta"))
