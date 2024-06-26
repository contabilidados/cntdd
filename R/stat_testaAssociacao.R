stat_testaAssociacao <- function(tabela, CORREC = T){

  pvalor <- suppressMessages(suppressWarnings(chisq.test(tabela, correct = CORREC)$p.value))

  pvalor <- format(round(pvalor, 2), nsmall = 2)

  if(pvalor <= 0.05){
    paste0("Ha associacao entre os dois grupos (p-Valor: ", pvalor, ")")
  } else {
    paste0("Nao ha associacao entre os dois grupos (p-Valor: ", pvalor, ")")
  }
}
