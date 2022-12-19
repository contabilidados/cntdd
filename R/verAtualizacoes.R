verAtualizacoes <-
  function(){

    url <- "https://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/DFP/DADOS/"
    arquivo <-
      url %>%
      read_html() %>%
      html_nodes(xpath='/html/body/div[1]/pre/a') %>%
      html_text()
    dataArquivo <- url %>% read_html() %>%
      # html() %>%
      html_nodes(xpath='/html/body/div[1]/pre/text()') %>%
      html_text() %>%
      gsub("[[:space:]]", "", .)
    library(tidyverse)

    .df <-
      data.frame(arquivo, dataArquivo) %>%
      slice(-1) %>%
      separate(dataArquivo, c("dia", "mes", "resto"), sep = "\\-") %>%
      na.omit %>%
      mutate(
        ano = substr(resto, 1, 4),
        hora = substr(resto, 5, 9),
        tamanho = substr(resto, 10, nchar(resto))) %>%
      left_join(
        cntdd::dt_meses %>% dplyr::select(ends_with("abb")), by = c("mes" = "month.abb")
      ) %>%
      mutate(
        data = as.Date(paste(ano, mes.abb, dia, sep = "-"), format = "%Y-%b-%d")
      ) %>%
      dplyr::select(arquivo, data, tamanho)

    return(.df)

  }


