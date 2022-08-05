#' Calcular a média geométrica
#'
#' Esta função calculará a média geométrica de um vetor numérico
#'
#' @param indicador Um vetor tipo character com o nome do indicador
#' @param periodo Vetor numérico indicando o período da análise
#' @param estoque Vetor com os valores do estoque
#' @param atvCirc Vetor com os valores do Ativo Circulante
#' @param pasCirc Vetor com os valores do Passivo Circulante
#'
#' @examples
#'
#' ind_liqSeca(indicador = "Liquidez Seca", periodo = 2018:2020, estoque = c(2000,3000,4000), atvCirc = c(5000,6000,7000), pasCirc = c(1000,2000,3000)) # Vetor
#'
#' Usando como relatório HTML
#' analise <- ind_liqSeca(indicador = "Liquidez Seca", periodo = 2018:2020, estoque = c(2000,3000,4000), atvCirc = c(5000,6000,7000), pasCirc = c(1000,2000,3000))
#' a$`Como Analisar` %>% sjPlot::tab_df(encoding = "Latim-1")
#'
#' mtcars %>% summarise(media = geomMean(mpg)) # Dplyr
#'
#' @export

ind_liqSeca <- function(indicador, periodo, estoque, atvCirc, pasCirc){

  ratio <- (atvCirc - estoque) / pasCirc

  cat(plot(ratio))

  dt <- data.frame(Periodo = periodo, ratio = ratio)
  names(dt)[2] <- indicador

  if(nrow(dt) > 1){
    dtGeral <-
      data.frame(
        periodo = periodo,
        estoque = estoque,
        atvCirc = atvCirc,
        pasCirc = pasCirc
      )%>%
      mutate(
        {{indicador}} := format((atvCirc - estoque) / pasCirc, nsmall  =2)
      ) %>%
      arrange(periodo) %>%
      mutate(
        AH.Estoque = round(estoque / dplyr::lag(estoque) - 1, 4),
        AH.AtvCirc = round(atvCirc / dplyr::lag(atvCirc) - 1, 4),
        AH.PasCirc = round(pasCirc / dplyr::lag(pasCirc) - 1, 4)
      ) %>%
      filter(periodo == max(periodo)) %>%
      mutate(
        AH.Estoque = case_when(
          AH.Estoque > 0 ~
            paste0("Essa conta contribuiu para aumentar o indicador, pois variou positivamente em ",
                   showPercent(AH.Estoque), " em relação ao período anterior"),
          TRUE ~ "Essa conta contribuiu negativamente para o indicador"
        ),
        AH.AtvCirc = case_when(
          AH.AtvCirc > 0 ~ "Essa conta contribuiu positivamente para o indicador",
          TRUE ~ "Essa conta contribuiu negativamente para o indicador"
        ),
        AH.PasCirc = case_when(
          AH.PasCirc <= 0 ~ "Essa conta contribuiu positivamente para o indicador",
          TRUE ~ "Essa conta contribuiu negativamente para o indicador"
        )
      ) %>%
      dplyr::select(periodo, {{indicador}}, starts_with("AH.")) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = everything(), names_to = "Dados", values_to = "Analise") %>%
      data.frame()
  } else {
    dtGeral <-
      data.frame(Analise = "Observe se esse indicador supera 1")
  }

  return(list(indices = dt, `Como Analisar` = dtGeral))

}





