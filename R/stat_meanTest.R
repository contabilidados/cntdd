#' Teste de Medias ou Medianas
#'
#' @description
#' Esta funcao realiza o teste de diferenca de medias ou mediana de dois vetores numericos
#' e apresenta os resultados em uma tabela.
#'
#' @details
#' A escolha entre o teste de media ou media e realizada de forma automatica, de acordo com a distribuicao
#' das variaveis numericas informadas.
#'
#' Informacoes adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <http://contabilidados.com.br>.
#' Ao acessar, fazer busca pelo nome da funcao `utl_tStarSig`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param value_Y Um vetor numerico representando o primeiro grupo
#' @param name_Y Um nome para o vetor numerico que representa o primeiro grupo
#' @param value_X Um vetor numerico representando o segundo grupo
#' @param name_X Um nome para o vetor numerico que representa o segundo grupo
#' @param pvalor O valor cretico a ser considerado nos testes. O padrao e 5%
#'
#' @returns
#' Um data.frame com a analise sobre o teste de media dos dois grupos de valores.
#'
#' @import ggplot2
#' @import readxl
#' @import dplyr
#' @importFrom stats var.test
#' @importFrom stats t.test
#' @importFrom stats wilcox.test
#' @importFrom stats median
#' @importFrom nortest lillie.test
#' @export

stat_meanTest <- function(value_Y, name_Y = "Y", value_X, name_X = "X", pvalor = 0.05){

  A <- value_Y
  B <- value_X

  qdeA <- length(A); qdeB <- length(B)
  name_a <- name_Y
  name_b <- name_X

  norm.A <- lillie.test(A)
  norm.B <- lillie.test(B)
  pvNormA <- norm.A$p.value
  pvNormB <- norm.B$p.value

  varAB <- var.test(A, B)
  pvVarAB <- varAB$p.value

  if(pvVarAB > pvalor) {parVar <- T} else {parVar <- F}

  if(pvNormA > pvalor & pvNormB > pvalor) {
    difmedia <- "Teste t";
    pvteste <- t.test(A, B, var.equal = parVar);
    Normalidade <- "Ambos grupos tem distribuicao normal";
    RESULT <- "Medias"
    } else {
      difmedia <- "Mann-Whitney";
      pvteste <- suppressMessages(suppressWarnings(wilcox.test(A, B)));
      Normalidade <- "Pelo menos um dos grupos nao tem distribuicao normal";
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
    paste0("Media de ", name_a), paste0("Media de ", name_b),
    paste0("Media: ", name_a, " menos ", name_b),
    paste0("Mediana de ", name_a), paste0("Mediana de ", name_b),
    paste0("Mediana: ", name_a, " menos ", name_b), "Qde de Obs"
  )

  colnames(tabDados) <- "Resultados"

  return(tabDados)

  }
