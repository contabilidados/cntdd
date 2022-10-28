#' Importa dados da CVM
#'
#' @details
#'
#' A função possui a utilidade de realizar a extração de dados do repositório da CVM,
#' a partir de 5 parâmetros escolhidos pelo usuário.
#'
#' @param grupoconta Demonstrações contábeis: 'ativo', 'passivo' e 'dfc'
#' @param tipo 'consolidado' ou 'individual'
#' @param metDFC 'direto' ou 'indireto'
#' @param ano Ano da tabela que se deseja realizar a extração
#' @param path A pasta (link) em que se encontra o conjunto das tabelas
#'
#'
#' @examples
#'
#' library(cntdd)
#'
#' cvm_importaDFP()
#'
#' # Da forma acima, será chamada automaticamente os parametros: ativo', 'consolidado', 'direto', '2020' e NULL
#'
#' Outra forma de se chamar a função:
#'
#' cmv_importaDFP(grupoconta = "passivo", tipo = "individual", "indireto","2019")
#'
#'
#' @export

  # cvm_importaDFP irá guardar uma 'function' de 5 parâmetros: grupoconta, tipo, metDFC, ano, path.

cvm_importaDFP <- function(grupoconta = "ativo", tipo = "consolidado", metDFC = "direto",
                         ano = 2020, path = NULL){

  # Declarando duas variáveis: 'demFin' e 'tipoDF' utilizando o método switch, que irá retornar os valores de
  # 'grupoconta' e 'tipo'.

  demFin <- switch(grupoconta, "ativo" = "BPA", "passivo" = "BPP", "dfc" = "DFC")

  tipoDF <- switch(tipo, "consolidado" = "con","individual" = "ind")

  # Estrutura condicional que irá tratar de atribuir o caminho das bases de dados no parâmetro 'path'.

  if(is.null(path)) {

    PATH <- getwd()

  } else {

    PATH <- path

  }

  # arquivoZip recebe o resultado dos parâmetros 'path' e 'ano

  arquivoZip <- paste0(PATH, "/dfp_cia_aberta_", ano, ".zip")

  # estrutura condicional para definir o bloco de código (ou valor) retornado
  # da variável 'tipoDFC'.

  if(grupoconta == "dfc"){

    tipoDFC <- switch(metDFC, "direto" = "MD_", "indireto" = "MI_")

  } else {

    tipoDFC <- NULL

  }

  # arquivoCsv recebe os valores referentes aos nomes dos arquivos (files) que estarão contidas
  # na pasta em que o usuário selecionou.

  arquivoCsv <- paste0("dfp_cia_aberta_", demFin, "_", tipoDFC, tipoDF, "_", ano, ".csv")

  # resultado irá ler o arquivo 'csv' baixado, e irá guardar as variáveis 'arquivoZip' e 'arquivoCsv'.

  resultado <- read.csv(unz(arquivoZip, arquivoCsv),header=T, sep=";")

  # 'return(resultado)' irá exibir o output (saída) gerado pelo bloco de código acima

  return(resultado)

}
