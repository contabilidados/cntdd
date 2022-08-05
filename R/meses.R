#' Meses do ano
#'
#' Esta função gera um data frame com os meses do anos em diversos formatos
#' e informações adicionais como bimestre, trimestre, quadrimestre e semestre
#'
#'
#' @export

  meses <- function(){

    df =
      data.frame(mes.num = 1:12,
                 mes.nome = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio",
                              "Junho", "Julho", "Agosto", "Setembro", "Outubro",
                              "Novembro", "Dezembro"),
                 mes.abb  = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                              "Jul", "Ago", "Set", "Out", "Nov", "Dez"),
                 month.name = month.name,
                 month.abb = month.abb,
                 mes.chr = c("01", "02", "03", "04", "05", "06", "07", "08",
                             "09", "10", "11", "12"),
                 bim  = rep(1:6, each = 2),
                 trim = rep(1:4, each = 3),
                 quad = rep(1:3, each = 4),
                 sem  = rep(1:2, each = 6))

    return(df)
  }
