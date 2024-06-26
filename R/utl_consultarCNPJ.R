#' Consulta de informações de CNPJ via API "minhareceita.org
#'
#' @description
#' Esta função consulta informações detalhadas de empresas a partir de uma lista de CNPJs,
#' utilizando a API "minhareceita.org". A função retorna um DataFrame contendo informações
#' como UF, CEP, razão social, entre outros. Inclui subfunções para obter dados gerais,
#' CNAEs secundários e dados societários.
#'
#' @param cnpj_lista Um vetor de CNPJs como caracteres.
#'
#' @return Um DataFrame com informações detalhadas das empresas consultadas.
#'
#' @details
#' A função utiliza várias subfunções para:
#' - **Dados Gerais**: Coletar informações como UF, CEP, razão social, porte, etc.
#' - **CNAEs Secundários**: Obter códigos e descrições de atividades econômicas secundárias.
#' - **Dados Societários**: Coletar informações sobre sócios e qualificações.
#'
#' Em caso de erros na consulta, como problemas de conexão ou dados ausentes, a função
#' retorna um DataFrame vazio e continua a consulta para os próximos CNPJs.
#'
#' @seealso [jsonlite()], [dplyr()]
#'
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <https://contabilidados.quarto.pub/>.
#' Ao acessar, fazer busca pelo nome da função `utl_consultarCNPJ`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @examples
#' library(cntdd)
#'
#' utl_consultarCNPJ(cnpj_lista  = "24529265000140")
#'
#' @import dplyr
#' @import jsonlite
#' @importFrom pbapply pblapply
#' @importFrom purrr pluck
#' @importFrom parallel detectCores
#' @encoding UTF-8
#' @export



utl_consultarCNPJ <- function(cnpj_lista = "24529265000140") {
  message("Para visualizar o Layout dos dados abertos do CNPJ acesse: https://www.gov.br/receitafederal/dados/cnpj-metadados.pdf")
  message("Consultando informações de CNPJ via API minhareceita.org")

  # Configurar o número de núcleos
  num_cores <- detectCores() - 1

  # Função para obter todos os dados
  obter_dados <- function(cnpj) {
    url <- paste0("https://minhareceita.org/", cnpj)
    tryCatch({
      json_data <- fromJSON(url)

      # Dados gerais
      dados_gerais <- tibble(
        uf = pluck(json_data, "uf", .default = NA),
        cep = pluck(json_data, "cep", .default = NA),
        cnpj = pluck(json_data, "cnpj", .default = NA),
        pais = pluck(json_data, "pais", .default = NA),
        email = pluck(json_data, "email", .default = NA),
        porte = pluck(json_data, "porte", .default = NA),
        bairro = pluck(json_data, "bairro", .default = NA),
        numero = pluck(json_data, "numero", .default = NA),
        ddd_fax = pluck(json_data, "ddd_fax", .default = NA),
        municipio = pluck(json_data, "municipio", .default = NA),
        logradouro = pluck(json_data, "logradouro", .default = NA),
        cnae_fiscal = pluck(json_data, "cnae_fiscal", .default = NA),
        codigo_pais = pluck(json_data, "codigo_pais", .default = NA),
        complemento = pluck(json_data, "complemento", .default = NA),
        codigo_porte = pluck(json_data, "codigo_porte", .default = NA),
        razao_social = pluck(json_data, "razao_social", .default = NA),
        nome_fantasia = pluck(json_data, "nome_fantasia", .default = NA),
        capital_social = pluck(json_data, "capital_social", .default = NA),
        ddd_telefone_1 = pluck(json_data, "ddd_telefone_1", .default = NA),
        ddd_telefone_2 = pluck(json_data, "ddd_telefone_2", .default = NA),
        opcao_pelo_mei = pluck(json_data, "opcao_pelo_mei", .default = NA),
        descricao_porte = pluck(json_data, "descricao_porte", .default = NA),
        codigo_municipio = pluck(json_data, "codigo_municipio", .default = NA),
        natureza_juridica = pluck(json_data, "natureza_juridica", .default = NA),
        situacao_especial = pluck(json_data, "situacao_especial", .default = NA),
        opcao_pelo_simples = pluck(json_data, "opcao_pelo_simples", .default = NA),
        situacao_cadastral = pluck(json_data, "situacao_cadastral", .default = NA),
        data_opcao_pelo_mei = pluck(json_data, "data_opcao_pelo_mei", .default = NA),
        data_exclusao_do_mei = pluck(json_data, "data_exclusao_do_mei", .default = NA),
        cnae_fiscal_descricao = pluck(json_data, "cnae_fiscal_descricao", .default = NA),
        codigo_municipio_ibge = pluck(json_data, "codigo_municipio_ibge", .default = NA),
        data_inicio_atividade = pluck(json_data, "data_inicio_atividade", .default = NA),
        data_situacao_especial = pluck(json_data, "data_situacao_especial", .default = NA),
        data_opcao_pelo_simples = pluck(json_data, "data_opcao_pelo_simples", .default = NA),
        data_situacao_cadastral = pluck(json_data, "data_situacao_cadastral", .default = NA),
        nome_cidade_no_exterior = pluck(json_data, "nome_cidade_no_exterior", .default = NA),
        codigo_natureza_juridica = pluck(json_data, "codigo_natureza_juridica", .default = NA),
        data_exclusao_do_simples = pluck(json_data, "data_exclusao_do_simples", .default = NA),
        motivo_situacao_cadastral = pluck(json_data, "motivo_situacao_cadastral", .default = NA),
        ente_federativo_responsavel = pluck(json_data, "ente_federativo_responsavel", .default = NA),
        identificador_matriz_filial = pluck(json_data, "identificador_matriz_filial", .default = NA),
        qualificacao_do_responsavel = pluck(json_data, "qualificacao_do_responsavel", .default = NA),
        descricao_situacao_cadastral = pluck(json_data, "descricao_situacao_cadastral", .default = NA),
        descricao_tipo_de_logradouro = pluck(json_data, "descricao_tipo_de_logradouro", .default = NA),
        descricao_motivo_situacao_cadastral = pluck(json_data, "descricao_motivo_situacao_cadastral", .default = NA),
        descricao_identificador_matriz_filial = pluck(json_data, "descricao_identificador_matriz_filial", .default = NA)
      )

      # CNAEs secundários
      cnaes_secundarios <- json_data$cnaes_secundarios %||% list(codigo = NA, descricao = NA)
      dados_cnaes_sec <- tibble(
        cnpj = json_data$cnpj,
        cnaes_sec_codigo = cnaes_secundarios[[1]] %||% NA,
        cnaes_sec_descricao = cnaes_secundarios[[2]] %||% NA
      )

      # Dados societários
      qsa <- json_data$qsa %||% list()
      dados_societarios <- tibble(
        cnpj = json_data$cnpj,
        razao_social = pluck(json_data, "razao_social", .default = NA),
        qsa_pais = pluck(qsa, 1, .default = NA),
        qsa_nome_socio = pluck(qsa, 2, .default = NA),
        qsa_faixa_etaria = pluck(qsa, 4, .default = NA),
        qsa_codigo_faixa_etaria = pluck(qsa, 7, .default = NA),
        qsa_data_entrada_sociedade = pluck(qsa, 8, .default = NA),
        qsa_identificador_de_socio = pluck(qsa, 9, .default = NA),
        qsa_codigo_qualificacao_socio = pluck(qsa, 12, .default = NA),
        qsa_codigo_qualificacao_representante_legal = pluck(qsa, 13, .default = NA),
        qsa_qualificacao_representante_legal = pluck(qsa, 14, .default = NA),
        qsa_qualificacao_socio = pluck(qsa, 6, .default = NA)
      )

      # Unificar os data frames
      df_final <- dados_gerais %>%
        select(-razao_social) %>%
        inner_join(dados_societarios, by = 'cnpj', relationship = "many-to-many") %>%
        inner_join(dados_cnaes_sec, by = 'cnpj', relationship = "many-to-many")

      df_final
    }, error = function(e) {
      tibble()
    })
  }

  # Executando todas as consultas em paralelo com barra de progresso única
  resultados <- pblapply(cnpj_lista , obter_dados, cl = num_cores)

  df_resultado <- bind_rows(resultados)

  # Agregação e união dos data frames conforme solicitado
  temp <- df_resultado %>%
    group_by(cnpj) %>%
    summarise(
      cnpj = paste(unique(cnpj), collapse = ", "),
      cnaes_sec_descricao = paste(unique(cnaes_sec_descricao), collapse = ", "),
      cnaes_sec_codigo = paste(unique(cnaes_sec_codigo), collapse = ", "),
      qsa_nome_socio = paste(unique(qsa_nome_socio), collapse = ", "),
      qsa_qualificacao_socio = list(qsa_qualificacao_socio),
      qsa_pais = paste(unique(qsa_pais), collapse = ", "),
      qsa_faixa_etaria = list(qsa_faixa_etaria),
      qsa_codigo_faixa_etaria = list(qsa_codigo_faixa_etaria),
      qsa_data_entrada_sociedade = list(qsa_data_entrada_sociedade),
      qsa_identificador_de_socio = list(qsa_identificador_de_socio),
      qsa_codigo_qualificacao_representante_legal = paste(unique(qsa_codigo_qualificacao_representante_legal),collapse = ", "),
      qsa_qualificacao_representante_legal = paste(unique(qsa_qualificacao_representante_legal), collapse = ", "))

  temp2 <- df_resultado %>%
    select(
      -cnaes_sec_descricao,
      -cnaes_sec_codigo,
      -qsa_nome_socio,
      -qsa_qualificacao_socio,
      -qsa_codigo_qualificacao_socio,
      -qsa_pais,
      -qsa_faixa_etaria,
      -qsa_codigo_faixa_etaria,
      -qsa_data_entrada_sociedade,
      -qsa_identificador_de_socio,
      -qsa_codigo_qualificacao_representante_legal,
      -qsa_qualificacao_representante_legal
    ) %>%
    distinct(cnpj, .keep_all = TRUE)

  df <- temp %>% left_join(temp2, by = "cnpj")

  rm(temp,temp2)
  gc()

  # Reorganizando as colunas
  df <- df %>% select(cnpj, razao_social, nome_fantasia, logradouro, numero, bairro, municipio, uf, cep,
                      data_inicio_atividade, natureza_juridica, porte, capital_social, everything())

  return(df)
}

globalVariables(
  c(
    "%||%", "bairro", "capital_social", "cep", "cnaes_sec_codigo",
    "cnaes_sec_descricao", "data_inicio_atividade", "detectCores",
    "logradouro", "municipio", "natureza_juridica", "nome_fantasia",
    "numero", "porte", "qsa_codigo_faixa_etaria", "qsa_nome_socio", "qsa_pais",
    "qsa_codigo_qualificacao_representante_legal", "qsa_codigo_qualificacao_socio",
    "qsa_data_entrada_sociedade", "qsa_faixa_etaria", "qsa_identificador_de_socio",
    "qsa_nome_socio", "qsa_pais", "qsa_qualificacao_representante_legal",
    "qsa_qualificacao_socio", "razao_social", "uf", "cnpj", "cnpj_lista")
)
