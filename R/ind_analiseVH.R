#' Análise Vertical e Horizontal
#'
#' @description
#' Essa função calcula a análise vertical e horizontal de contas específicas de
#' um data.frame.
#'
#' @details
#' O usuário deve dispor de uma base de dados com, no mínimo, 4 colunas (variáveis):
#'
#' 1) Coluna com identificação da empresa.
#'
#' 2) Coluna com identificação do período da análise contendo, pelo menos 2 períodos,
#' para cálculo da análise horizontal.
#'
#' 3) Coluna da variável de interesse para a análise vertical
#'
#' 4) Coluna da variável de referência para a a análise vertical.
#'
#' Apresenta, como resultado, um data.frame com todas as colunas informadas, com
#' a análise horizotal ou com a anaálise vertical.
#'
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.com.br/>.
#' Ao acessar, fazer busca pelo nome da funcao `ind_analiseVH`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param .dt data.frame com, pelo menos, 4 colunas para preenchimento dos demais argumentos desta função
#' @param .contas Vetor tipo character com o nome das colunas referente às contas de interesse na análise
#' @param .contaReferenciaAV Nome da coluna com a conta de referência para a análise vertical
#' @param .colEmpresa Valor Nome da coluna que identifica a empresa (pode haver mais de uma empresa no banco de dados)
#' @param .colPeriodo Valor Nome da coluna que identifica o período da análise (pelo menos 2, por empresa, para a análise horizontal)
#' @param .output Informar `full` para o banco de dados completo, `AV` para mostrar apenas as análises verticais, `AH` para
#' mostrar apenas as análises horizontais, `AHAV` para mostrar, respectivamente, a análise horizontal e vertical ou
#' `AVAH` para mostrar, respectivamente, a análise vertical e horizontal
#'
#' @examples
#' library(cntdd)
#'
#' ind_analiseVH(
#' .dt = dt_contabil,
#' .contas = c("caixaEquiv", "clientesCP"),
#' .contaReferenciaAV = ativoTotal,
#' .colEmpresa = empresa,
#' .colPeriodo = ano,
#' .output = "AVAH"
#' )
#'
#' @import dplyr
#'
#' @export

ind_analiseVH <- function(
    .dt = dt_contabil, .contas = c("caixaEquiv", "clientesCP"),
    .contaReferenciaAV = ativoTotal, .colEmpresa= empresa, .colPeriodo = ano,
    .output = "AVAH"){
  res <-
    .dt %>%
    group_by(pick({{ .colEmpresa }})) %>%
    mutate(
      across(all_of(names(.dt)[match(.contas, names(.dt))]),
             ~ .x / {{.contaReferenciaAV}}, .names = "AV_{.col}")
    ) %>%
    mutate(
      across(all_of(names(.dt)[match(.contas, names(.dt))]),
             ~ .x / dplyr::lag(.x) - 1, .names = "AH_{.col}")
    )

  switch (.output,
          full = {res},
          AV = {res <- res %>% dplyr::select({{.colEmpresa}}, {{.colPeriodo}}, starts_with("AV"), {{.contaReferenciaAV}}) %>% ungroup()},
          AH = {res <- res %>% dplyr::select({{.colEmpresa}}, {{.colPeriodo}}, starts_with("AH")) %>% ungroup()},
          AHAV = {res <- res %>% dplyr::select({{.colEmpresa}}, {{.colPeriodo}}, starts_with( c("AH", "AV") ), {{.contaReferenciaAV}}) %>% ungroup()},
          AVAH = {res <- res %>% dplyr::select({{.colEmpresa}}, {{.colPeriodo}}, {{.contaReferenciaAV}}, starts_with( c("AV", "AH") )) %>% ungroup()}
  )

  return(res)

}

globalVariables(c("ativoTotal", "dt_contabil", "empresa"))
