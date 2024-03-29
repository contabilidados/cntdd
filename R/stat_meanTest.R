#' Realizar teste de médias
#'
#' Esta função realiza o teste de diferença de médias de dois vetores numéricos
#' e apresenta os resultados em uma tabela
#'
#' @param value_Y Um vetor numérico representando o primeiro grupo
#' @param name_Y Um nome para o vetor numérico que representa o primeiro grupo
#' @param value_X Um vetor numérico representando o segundo grupo
#' @param name_X Um nome para o vetor numérico que representa o segundo grupo
#' @param pvalor O valor crítido a ser considerado nos testes. O padrão é 5%
#'
#' @returns
#' Um data.frame com a análise sobre o teste de média dos dois grupos de valores.
#'
#' @examples
#'
#' library(cntdd)
#' library(dplyr)
#'
#' stat_meanTest(value_Y = 1:6,name_Y = "Menores", value_X = 4:10, name_X = "Maiores", pvalor = 0.01)
#'
#' stat_meanTest(mtcars$mpg[mtcars$carb == 1], name_Y = "Carb 1", value_X = mtcars$mpg[mtcars$carb == 4], name_X = "Carb 4")
#'
#' # Formatando a tabela com KableExtra
#' # cntdd::stat_meanTest(value_Y = 1:6,name_Y = "Menores", value_X = 4:10, name_X = "Maiores", pvalor = 0.01) %>%
#' # kableExtra::kbl(booktabs = T, format = "html") %>% kableExtra::kable_styling()
#' @import ggplot2
#' @import readxl
#' @import dplyr
#' @export

stat_meanTest <- function(value_Y, name_Y = "Y", value_X, name_X = "X", pvalor = 0.05){

  A <- value_Y
  B <- value_X

  qdeA <- length(A); qdeB <- length(B)
  name_a <- name_Y
  name_b <- name_X

  norm.A <- nortest::lillie.test(A)
  norm.B <- nortest::lillie.test(B)
  pvNormA <- norm.A$p.value
  pvNormB <- norm.B$p.value

  varAB <- var.test(A, B)
  pvVarAB <- varAB$p.value

  if(pvVarAB > pvalor) {parVar <- T} else {parVar <- F}

  if(pvNormA > pvalor & pvNormB > pvalor) {
    difmedia <- "Teste t";
    pvteste <- t.test(A, B, var.equal = parVar);
    Normalidade <- "Ambos grupos tem distribuição normal";
    RESULT <- "Médias"
    } else {
      difmedia <- "Mann-Whitney";
      pvteste <- suppressMessages(suppressWarnings(wilcox.test(A, B)));
      Normalidade <- "Pelo menos um dos grupos não tem distribuição normal";
      RESULT <- "Medianas"
      }

  if(pvteste$p.value > pvalor) {result <- paste0(RESULT, " iguais")
  } else {
    result <- paste0(RESULT, " diferentes")}

  spreadMedia <- mean(A, na.rm = T) - mean(B, na.rm = T)
  spreadMediana <- median(A, na.rm = T) - median(B, na.rm = T)

  dados <- data.frame(
    normA = formatC(pvNormA, digits = 3, format = "f", decimal.mark = ","),
    normB = formatC(pvNormB, digits = 3, format = "f", decimal.mark = ","),
    Normalidade = Normalidade,
    teste =  difmedia,
    pvalue = formatC(pvteste$p.value, digits = 3, format = "f", decimal.mark = ","),
    resultado = result,
    mediaA = formatC(mean(A, na.rm = T), digits = 3, format = "f", decimal.mark = ","),
    mediaB = formatC(mean(B, na.rm = T), digits = 3, format = "f", decimal.mark = ","),
    spreadMedia = formatC(spreadMedia, digits = 3, format = "f", decimal.mark = ","),
    medianaA = formatC(median(A, na.rm = T), digits = 3, format = "f", decimal.mark = ","),
    medianaB = formatC(median(B, na.rm = T), digits = 3, format = "f", decimal.mark = ","),
    spreadMediana = formatC(spreadMediana, digits = 3, format = "f", decimal.mark = ","),
    qdeObs = paste0(name_a, ": ", qdeA, " | ", name_b, ": ", qdeB))

  tabDados <- as.data.frame(t(dados))
  rownames(tabDados) <-
    c(
      paste0("pValue Normalidade de ", name_a),
    paste0("pValue Normalidade de ", name_b),
    "Resultado Normalidade",
    "Teste Usado",
    paste0("pValue ", difmedia),
    paste0("Resultado ", difmedia),
    paste0("Média de ", name_a), paste0("Média de ", name_b),
    paste0("Média: ", name_a, " menos ", name_b),
    paste0("Mediana de ", name_a), paste0("Mediana de ", name_b),
    paste0("Mediana: ", name_a, " menos ", name_b), "Qde de Obs"
  )

  colnames(tabDados) <- "Resultados"

  return(tabDados)

  }
