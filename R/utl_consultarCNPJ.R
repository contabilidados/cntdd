#' Consulta de informações de CNPJ via API "minhareceita.org
#'
#' @description
#'
#' Esta função consulta informações detalhadas de empresas a partir de uma lista de CNPJs,
#' utilizando a API "minhareceita.org". A função retorna um DataFrame contendo informações
#' como UF, CEP, razão social, entre outros. Inclui subfunções para obter dados gerais, CNAEs
#' secundários e dados societários.
#'
#' @param cnpj Um vetor de CNPJs como caracteres.
#'
#' @return Um DataFrame com informações detalhadas das empresas consultadas.
#'
#' @details
#' A função depende das bibliotecas `jsonlite` e `tidyverse`. Certifique-se de que estão instaladas
#' e carregadas.
#'
#' @examples
#' cnpj <- c("08350241000172", "24529265000140")
#' df_resultado <- utl_consultarCNPJ(cnpj)
#'
#' @examples
#' Função desenvolvida por Alexsandro Prado (alexsandro.prado@ufersa.edu.br) em colaboração com Kleber Formiga (mirandakf@ufersa.edu.br)
#' Informações adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: \href{http://contabilidados.com.br}{(Acesse Aqui)}. Ao acessar, fazer busca
#' pelo nome da função `ind_liqCorrente`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: \email{contabilidados@@ufersa.edu.br}
#' Siga-nos no Instagram: \href{https://www.instagram.com/contabilidados/}{@contabilidados}
#'
#'
#' @import tidyverse
#' @import jsonlite
#' @export

utl_consultarCNPJ <- function(cnpj = "24529265000140") {
  library(jsonlite)


# TODOS OS DADOS (EXCETO CNAES SECUNDARIOS E QSA) ----
obter_dados_gerais <- function(cnpj) {
lista <- list()
for (i in seq_along(cnpj)) {
  # Carregando o JSON da API
  json_data <- jsonlite::fromJSON(paste0("https://minhareceita.org/", cnpj[i]))

  # Extraindo apenas os elementos de interesse e adicionando à lista
  lista[[i]] <- list(
    uf                                        =      unlist(json_data[1]),
    cep                                       =      unlist(json_data[2]),
    cnpj                                      =      unlist(json_data[4]),
    pais                                      =      unlist(json_data[5]),
    email                                     =      unlist(json_data[6]),
    porte                                     =      unlist(json_data[7]),
    bairro                                    =      unlist(json_data[8]),
    numero                                    =      unlist(json_data[9]),
    ddd_fax                                   =      unlist(json_data[10]),
    municipio                                 =      unlist(json_data[11]),
    logradouro                                =      unlist(json_data[12]),
    cnae_fiscal                               =      unlist(json_data[13]),
    codigo_pais                               =      unlist(json_data[14]),
    complemento                               =      unlist(json_data[15]),
    codigo_porte                              =      unlist(json_data[16]),
    razao_social                              =      unlist(json_data[17]),
    nome_fantasia                             =      unlist(json_data[18]),
    capital_social                            =      unlist(json_data[19]),
    ddd_telefone_1                            =      unlist(json_data[20]),
    ddd_telefone_2                            =      unlist(json_data[21]),
    opcao_pelo_mei                            =      unlist(json_data[22]),
    descricao_porte                           =      unlist(json_data[23]),
    codigo_municipio                          =      unlist(json_data[24]),
    natureza_juridica                         =      unlist(json_data[26]),
    situacao_especial                         =      unlist(json_data[27]),
    opcao_pelo_simples                        =      unlist(json_data[28]),
    situacao_cadastral                        =      unlist(json_data[29]),
    data_opcao_pelo_mei                       =      unlist(json_data[30]),
    data_exclusao_do_mei                      =      unlist(json_data[31]),
    cnae_fiscal_descricao                     =      unlist(json_data[32]),
    codigo_municipio_ibge                     =      unlist(json_data[33]),
    data_inicio_atividade                     =      unlist(json_data[34]),
    data_situacao_especial                    =      unlist(json_data[35]),
    data_opcao_pelo_simples                   =      unlist(json_data[36]),
    data_situacao_cadastral                   =      unlist(json_data[37]),
    nome_cidade_no_exterior                   =      unlist(json_data[38]),
    codigo_natureza_juridica                  =      unlist(json_data[39]),
    data_exclusao_do_simples                  =      unlist(json_data[40]),
    motivo_situacao_cadastral                 =      unlist(json_data[41]),
    ente_federativo_responsavel               =      unlist(json_data[42]),
    identificador_matriz_filial               =      unlist(json_data[43]),
    qualificacao_do_responsavel               =      unlist(json_data[44]),
    descricao_situacao_cadastral              =      unlist(json_data[45]),
    descricao_tipo_de_logradouro              =      unlist(json_data[46]),
    descricao_motivo_situacao_cadastral       =      unlist(json_data[47]),
    descricao_identificador_matriz_filial     =      unlist(json_data[48])
  )
}


nomes <- names(lista[[1]])

lista <- lapply(lista, function(x) {
  # Filtrando elementos da lista interna que contêm os nomes desejados
  x[names(x) %in% nomes]
})

# Criando uma lista de data frames temporários
temp_list <- lapply(lista, function(x) {
  # Determina o maior tamanho entre as colunas
  max_length <- max(length(x$uf), length(x$cep), length(x$cnpj), length(x$pais), length(x$email), length(x$porte), length(x$bairro), length(x$numero), length(x$ddd_fax), length(x$municipio), length(x$logradouro), length(x$cnae_fiscal), length(x$codigo_pais), length(x$complemento), length(x$codigo_porte), length(x$razao_social), length(x$nome_fantasia), length(x$capital_social), length(x$ddd_telefone_1), length(x$ddd_telefone_2), length(x$opcao_pelo_mei), length(x$descricao_porte), length(x$codigo_municipio), length(x$natureza_juridica), length(x$situacao_especial), length(x$opcao_pelo_simples), length(x$situacao_cadastral), length(x$data_opcao_pelo_mei), length(x$data_exclusao_do_mei), length(x$cnae_fiscal_descricao), length(x$codigo_municipio_ibge), length(x$data_inicio_atividade), length(x$data_situacao_especial), length(x$data_opcao_pelo_simples), length(x$data_situacao_cadastral), length(x$nome_cidade_no_exterior), length(x$codigo_natureza_juridica), length(x$data_exclusao_do_simples), length(x$motivo_situacao_cadastral), length(x$ente_federativo_responsavel), length(x$identificador_matriz_filial), length(x$qualificacao_do_responsavel), length(x$descricao_situacao_cadastral), length(x$descricao_tipo_de_logradouro), length(x$descricao_motivo_situacao_cadastral), length(x$descricao_identificador_matriz_filial), na.rm = TRUE)

  # Ajusta o tamanho de cada coluna, preenchendo com NA se necessário
  x$uf <- ifelse(length(x$uf) < max_length, c(x$uf, rep(NA, max_length - length(x$uf))), x$uf)
  x$cep <- ifelse(length(x$cep) < max_length, c(x$cep, rep(NA, max_length - length(x$cep))), x$cep)
  x$cnpj  <- ifelse(length(x$cnpj) < max_length, c(x$cnpj, rep(NA, max_length - length(x$cnpj))), x$cnpj)
  x$pais <- ifelse(length(x$pais) < max_length, c(x$pais, rep(NA, max_length - length(x$pais))), x$pais)
  x$email <- ifelse(length(x$email) < max_length, c(x$email, rep(NA, max_length - length(x$email))), x$email)
  x$porte <- ifelse(length(x$porte) < max_length, c(x$porte, rep(NA, max_length - length(x$porte))), x$porte)
  x$bairro<- ifelse(length(x$bairro) < max_length, c(x$bairro, rep(NA, max_length - length(x$bairro))), x$bairro)
  x$numero<- ifelse(length(x$numero) < max_length, c(x$numero, rep(NA, max_length - length(x$numero))), x$numero)
  x$ddd_fax<- ifelse(length(x$ddd_fax) < max_length, c(x$ddd_fax, rep(NA, max_length - length(x$ddd_fax))), x$ddd_fax)
  x$municipio<- ifelse(length(x$municipio) < max_length, c(x$municipio, rep(NA, max_length - length(x$municipio))), x$municipio)
  x$logradouro<- ifelse(length(x$logradouro) < max_length, c(x$logradouro, rep(NA, max_length - length(x$logradouro))), x$logradouro)
  x$cnae_fiscal<- ifelse(length(x$cnae_fiscal) < max_length, c(x$cnae_fiscal, rep(NA, max_length - length(x$cnae_fiscal))), x$cnae_fiscal)
  x$codigo_pais<- ifelse(length(x$codigo_pais) < max_length, c(x$codigo_pais, rep(NA, max_length - length(x$codigo_pais))), x$codigo_pais)
  x$complemento<- ifelse(length(x$complemento) < max_length, c(x$complemento, rep(NA, max_length - length(x$complemento))), x$complemento)
  x$codigo_porte<- ifelse(length(x$codigo_porte) < max_length, c(x$codigo_porte, rep(NA, max_length - length(x$codigo_porte))), x$codigo_porte)
  x$razao_social<- ifelse(length(x$razao_social) < max_length, c(x$razao_social, rep(NA, max_length - length(x$razao_social))), x$razao_social)
  x$nome_fantasia<- ifelse(length(x$nome_fantasia) < max_length, c(x$nome_fantasia, rep(NA, max_length - length(x$nome_fantasia))), x$nome_fantasia)
  x$capital_social<- ifelse(length(x$capital_social) < max_length, c(x$capital_social, rep(NA, max_length - length(x$capital_social))), x$capital_social)
  x$ddd_telefone_1<- ifelse(length(x$ddd_telefone_1) < max_length, c(x$ddd_telefone_1, rep(NA, max_length - length(x$ddd_telefone_1))), x$ddd_telefone_1)
  x$ddd_telefone_2<- ifelse(length(x$ddd_telefone_2) < max_length, c(x$ddd_telefone_2, rep(NA, max_length - length(x$ddd_telefone_2))), x$ddd_telefone_2)
  x$opcao_pelo_mei<- ifelse(length(x$opcao_pelo_mei) < max_length, c(x$opcao_pelo_mei, rep(NA, max_length - length(x$opcao_pelo_mei))), x$opcao_pelo_mei)
  x$descricao_porte<- ifelse(length(x$descricao_porte) < max_length, c(x$descricao_porte, rep(NA, max_length - length(x$descricao_porte))), x$descricao_porte)
  x$codigo_municipio<- ifelse(length(x$codigo_municipio) < max_length, c(x$codigo_municipio, rep(NA, max_length - length(x$codigo_municipio))), x$codigo_municipio)
  x$natureza_juridica<- ifelse(length(x$natureza_juridica) < max_length, c(x$natureza_juridica, rep(NA, max_length - length(x$natureza_juridica))), x$natureza_juridica)
  x$situacao_especial<- ifelse(length(x$situacao_especial) < max_length, c(x$situacao_especial, rep(NA, max_length - length(x$situacao_especial))), x$situacao_especial)
  x$opcao_pelo_simples<- ifelse(length(x$opcao_pelo_simples) < max_length, c(x$opcao_pelo_simples, rep(NA, max_length - length(x$opcao_pelo_simples))), x$opcao_pelo_simples)
  x$situacao_cadastral<- ifelse(length(x$situacao_cadastral) < max_length, c(x$situacao_cadastral, rep(NA, max_length - length(x$situacao_cadastral))), x$situacao_cadastral)
  x$data_opcao_pelo_mei<- ifelse(length(x$data_opcao_pelo_mei) < max_length, c(x$data_opcao_pelo_mei, rep(NA, max_length - length(x$data_opcao_pelo_mei))), x$data_opcao_pelo_mei)
  x$data_exclusao_do_mei<- ifelse(length(x$data_exclusao_do_mei) < max_length, c(x$data_exclusao_do_mei, rep(NA, max_length - length(x$data_exclusao_do_mei))), x$data_exclusao_do_mei)
  x$cnae_fiscal_descricao<- ifelse(length(x$cnae_fiscal_descricao) < max_length, c(x$cnae_fiscal_descricao, rep(NA, max_length - length(x$cnae_fiscal_descricao))), x$cnae_fiscal_descricao)
  x$codigo_municipio_ibge<- ifelse(length(x$codigo_municipio_ibge) < max_length, c(x$codigo_municipio_ibge, rep(NA, max_length - length(x$codigo_municipio_ibge))), x$codigo_municipio_ibge)
  x$data_inicio_atividade<- ifelse(length(x$data_inicio_atividade) < max_length, c(x$data_inicio_atividade, rep(NA, max_length - length(x$data_inicio_atividade))), x$data_inicio_atividade)
  x$data_situacao_especial<- ifelse(length(x$data_situacao_especial) < max_length, c(x$data_situacao_especial, rep(NA, max_length - length(x$data_situacao_especial))), x$data_situacao_especial)
  x$data_opcao_pelo_simples<- ifelse(length(x$data_opcao_pelo_simples) < max_length, c(x$data_opcao_pelo_simples, rep(NA, max_length - length(x$data_opcao_pelo_simples))), x$data_opcao_pelo_simples)
  x$data_situacao_cadastral<- ifelse(length(x$data_situacao_cadastral) < max_length, c(x$data_situacao_cadastral, rep(NA, max_length - length(x$data_situacao_cadastral))), x$data_situacao_cadastral)
  x$nome_cidade_no_exterior<- ifelse(length(x$nome_cidade_no_exterior) < max_length, c(x$nome_cidade_no_exterior, rep(NA, max_length - length(x$nome_cidade_no_exterior))), x$nome_cidade_no_exterior)
  x$codigo_natureza_juridica<- ifelse(length(x$codigo_natureza_juridica) < max_length, c(x$codigo_natureza_juridica, rep(NA, max_length - length(x$codigo_natureza_juridica))), x$codigo_natureza_juridica)
  x$data_exclusao_do_simples<- ifelse(length(x$data_exclusao_do_simples) < max_length, c(x$data_exclusao_do_simples, rep(NA, max_length - length(x$data_exclusao_do_simples))), x$data_exclusao_do_simples)
  x$motivo_situacao_cadastral<- ifelse(length(x$motivo_situacao_cadastral) < max_length, c(x$motivo_situacao_cadastral, rep(NA, max_length - length(x$motivo_situacao_cadastral))), x$motivo_situacao_cadastral)
  x$ente_federativo_responsavel<- ifelse(length(x$ente_federativo_responsavel) < max_length, c(x$ente_federativo_responsavel, rep(NA, max_length - length(x$ente_federativo_responsavel))), x$ente_federativo_responsavel)
  x$identificador_matriz_filial<- ifelse(length(x$identificador_matriz_filial) < max_length, c(x$identificador_matriz_filial, rep(NA, max_length - length(x$identificador_matriz_filial))), x$identificador_matriz_filial)
  x$qualificacao_do_responsavel<- ifelse(length(x$qualificacao_do_responsavel) < max_length, c(x$qualificacao_do_responsavel, rep(NA, max_length - length(x$qualificacao_do_responsavel))), x$qualificacao_do_responsavel)
  x$descricao_situacao_cadastral<- ifelse(length(x$descricao_situacao_cadastral) < max_length, c(x$descricao_situacao_cadastral, rep(NA, max_length - length(x$descricao_situacao_cadastral))), x$descricao_situacao_cadastral)
  x$descricao_tipo_de_logradouro<- ifelse(length(x$descricao_tipo_de_logradouro) < max_length, c(x$descricao_tipo_de_logradouro, rep(NA, max_length - length(x$descricao_tipo_de_logradouro))), x$descricao_tipo_de_logradouro)
  x$descricao_motivo_situacao_cadastral<- ifelse(length(x$descricao_motivo_situacao_cadastral) < max_length, c(x$descricao_motivo_situacao_cadastral, rep(NA, max_length - length(x$descricao_motivo_situacao_cadastral))), x$descricao_motivo_situacao_cadastral)
  x$descricao_identificador_matriz_filial<- ifelse(length(x$descricao_identificador_matriz_filial) < max_length, c(x$descricao_identificador_matriz_filial, rep(NA, max_length - length(x$descricao_identificador_matriz_filial))), x$descricao_identificador_matriz_filial)

  # Cria um data frame com as colunas ajustadas
  return(data.frame(uf=x$uf, cep=x$cep, cnpj=x$cnpj, pais=x$pais, email=x$email, porte=x$porte, bairro=x$bairro, numero=x$numero, ddd_fax=x$ddd_fax, municipio=x$municipio, logradouro=x$logradouro, cnae_fiscal=x$cnae_fiscal, codigo_pais=x$codigo_pais, complemento=x$complemento, codigo_porte=x$codigo_porte, razao_social=x$razao_social, nome_fantasia=x$nome_fantasia, capital_social=x$capital_social, ddd_telefone_1=x$ddd_telefone_1, ddd_telefone_2=x$ddd_telefone_2, opcao_pelo_mei=x$opcao_pelo_mei, descricao_porte=x$descricao_porte, codigo_municipio=x$codigo_municipio, natureza_juridica=x$natureza_juridica, situacao_especial=x$situacao_especial, opcao_pelo_simples=x$opcao_pelo_simples, situacao_cadastral=x$situacao_cadastral, data_opcao_pelo_mei=x$data_opcao_pelo_mei, data_exclusao_do_mei=x$data_exclusao_do_mei, cnae_fiscal_descricao=x$cnae_fiscal_descricao, codigo_municipio_ibge=x$codigo_municipio_ibge, data_inicio_atividade=x$data_inicio_atividade, data_situacao_especial=x$data_situacao_especial, data_opcao_pelo_simples=x$data_opcao_pelo_simples, data_situacao_cadastral=x$data_situacao_cadastral, nome_cidade_no_exterior=x$nome_cidade_no_exterior, codigo_natureza_juridica=x$codigo_natureza_juridica, data_exclusao_do_simples=x$data_exclusao_do_simples, motivo_situacao_cadastral=x$motivo_situacao_cadastral, ente_federativo_responsavel=x$ente_federativo_responsavel, identificador_matriz_filial=x$identificador_matriz_filial, qualificacao_do_responsavel=x$qualificacao_do_responsavel, descricao_situacao_cadastral=x$descricao_situacao_cadastral, descricao_tipo_de_logradouro=x$descricao_tipo_de_logradouro, descricao_motivo_situacao_cadastral=x$descricao_motivo_situacao_cadastral, descricao_identificador_matriz_filial=x$descricao_identificador_matriz_filial))
})

# Combinando os data frames da lista em um único data frame
df_geral <- do.call(rbind, temp_list)
}

# APENAS CNAE_SENCUNDARIOS ----
obter_cnaes_secundarios <- function(cnpj) {
lista <- list()
for (i in seq_along(cnpj)) {
  # Carregando o JSON da API
  json_data <- fromJSON(paste0("https://minhareceita.org/", cnpj[i]))

  # Extraindo apenas os elementos de interesse e adicionando à lista
  lista[[i]] <- list(
    cnpj = cnpj[i],
    cnaes_sec_codigo = unlist(json_data$cnaes_secundarios[1]),
    cnaes_sec_descricao = unlist(json_data$cnaes_secundarios[2])
  )
}

nomes <- names(lista[[1]])

lista <- lapply(lista, function(x) {
  # Filtrando elementos da lista interna que contêm os nomes desejados
  x[names(x) %in% nomes]
})

# Criando uma lista de data frames temporários
temp_list <- lapply(lista, function(x) {
  # Determina o maior tamanho entre as colunas
  max_length <- max(length(x$cnpj), length(x$cnaes_sec_codigo), length(x$cnaes_sec_descricao), na.rm = TRUE)

  # Ajusta o tamanho de cada coluna, preenchendo com NA se necessário
  x$cnpj <- ifelse(length(x$cnpj) < max_length, c(x$cnpj, rep(NA, max_length - length(x$cnpj))), x$cnpj)
  x$cnaes_sec_codigo <- ifelse(length(x$cnaes_sec_codigo) < max_length, c(x$cnaes_sec_codigo, rep(NA, max_length - length(x$cnaes_sec_codigo))), x$cnaes_sec_codigo)
  x$cnaes_sec_descricao <- ifelse(length(x$cnaes_sec_descricao) < max_length, c(x$cnaes_sec_descricao, rep(NA, max_length - length(x$cnaes_sec_descricao))), x$cnaes_sec_descricao)

  # Cria um data frame com as colunas ajustadas
  return(data.frame(cnpj = x$cnpj, cnaes_sec_codigo = x$cnaes_sec_codigo, cnaes_sec_descricao = x$cnaes_sec_descricao))
})

# Combinando os data frames da lista em um único data frame
df_qsa_cnae_sec <- do.call(rbind, temp_list)
}
# APENAS DADOS SOCIETÁRIOS ----
obter_dados_societarios <- function(cnpj) {
  lista <- list()
  for (i in seq_along(cnpj)) {
    # Carregando o JSON da API
    json_data <-
      fromJSON(paste0("https://minhareceita.org/", cnpj[i]))

    # Extraindo apenas os elementos de interesse e adicionando à lista
    lista[[i]] <- list(
      qsa_pais = unlist(json_data$qsa[1]),
      qsa_nome_socio = unlist(json_data$qsa[2]),
      cnpj = unlist(json_data$cnpj),
      razao_social = unlist(json_data$razao_social),
      qsa_faixa_etaria = unlist(json_data$qsa[4]),
      qsa_codigo_faixa_etaria = unlist(json_data$qsa[7]),
      qsa_data_entrada_sociedade = unlist(json_data$qsa[8]),
      qsa_identificador_de_socio = unlist(json_data$qsa[9]),
      qsa_codigo_qualificacao_socio = unlist(json_data$qsa[12]),
      qsa_codigo_qualificacao_representante_legal = unlist(json_data$qsa[13]),
      qsa_qualificacao_representante_legal = unlist(json_data$qsa[14]),
      qsa_qualificacao_socio = unlist(json_data$qsa[6])
    )
  }

  nomes <- names(lista[[1]])

  lista <- lapply(lista, function(x) {
    # Filtrando elementos da lista interna que contêm os nomes desejados
    x[names(x) %in% nomes]
  })

  # Criando uma lista de data frames temporários
  temp_list <- lapply(lista, function(x) {
    # Determina o maior tamanho entre as colunas
    max_length <-
      max(
        length(x$cnpj),
        length(x$razao_social),
        length(x$qsa_pais),
        length(x$qsa_nome_socio),
        length(x$qsa_data_entrada_sociedade),
        length(x$qsa_codigo_faixa_etaria),
        length(x$qsa_identificador_de_socio),
        length(x$qsa_faixa_etaria),
        length(x$qsa_codigo_qualificacao_socio),
        length(x$qsa_codigo_qualificacao_representante_legal),
        length(x$qsa_qualificacao_socio),
        na.rm = TRUE
      )

    # Ajusta o tamanho de cada coluna, preenchendo com NA se necessário
    x$cnpj <- ifelse(length(x$cnpj) < max_length, c(x$cnpj, rep(NA, max_length - length(x$cnpj))), x$cnpj)
    x$razao_social <-ifelse(length(x$razao_social) < max_length,c(x$razao_social, rep(NA, max_length - length(x$razao_social))),x$razao_social)
    x$qsa_pais <-ifelse(length(x$qsa_pais) < max_length,c(x$qsa_pais, rep(NA, max_length - length(x$qsa_pais))), x$qsa_pais)
    x$qsa_nome_socio <-ifelse(length(x$qsa_nome_socio) < max_length,c(x$qsa_nome_socio, rep(NA, max_length - length(x$qsa_nome_socio))),x$qsa_nome_socio)
    x$qsa_data_entrada_sociedade <-ifelse(length(x$qsa_data_entrada_sociedade) < max_length,c(x$qsa_data_entrada_sociedade, rep(NA, max_length - length(x$qsa_data_entrada_sociedade))),x$qsa_data_entrada_sociedade)
    x$qsa_identificador_de_socio <- ifelse(length(x$qsa_identificador_de_socio) < max_length,c(x$qsa_identificador_de_socio, rep(NA, max_length - length(x$qsa_identificador_de_socio))),x$qsa_identificador_de_socio)
    x$qsa_faixa_etaria <-ifelse(length(x$qsa_faixa_etaria) < max_length,c(x$qsa_faixa_etaria, rep(NA, max_length - length(x$qsa_faixa_etaria))),x$qsa_faixa_etaria)
    x$qsa_codigo_qualificacao_socio <-ifelse(length(x$qsa_codigo_qualificacao_socio) < max_length,c(x$qsa_codigo_qualificacao_socio,rep(NA, max_length - length(x$qsa_codigo_qualificacao_socio))),x$qsa_codigo_qualificacao_socio)
    x$qsa_codigo_qualificacao_representante_legal <-ifelse(length(x$qsa_codigo_qualificacao_representante_legal) < max_length,c(x$qsa_codigo_qualificacao_representante_legal,rep(NA,max_length - length(x$qsa_codigo_qualificacao_representante_legal))),x$qsa_codigo_qualificacao_representante_legal)
    x$qsa_codigo_faixa_etaria <-ifelse(length(x$qsa_codigo_faixa_etaria) < max_length,c(x$qsa_codigo_faixa_etaria, rep(NA, max_length - length(x$qsa_codigo_faixa_etaria))),x$qsa_codigo_faixa_etaria)
    x$qsa_qualificacao_socio <-ifelse(length(x$qsa_qualificacao_socio) < max_length,c(x$qsa_qualificacao_socio, rep(NA, max_length - length(x$qsa_qualificacao_socio))),x$qsa_qualificacao_socio)

    # Cria um data frame com as colunas ajustadas
    return(
      data.frame(
        cnpj = x$cnpj,
        razao_social = x$razao_social,
        faixa_etaria = x$qsa_faixa_etaria,
        qsa_pais = x$qsa_pais,
        qsa_nome_socio = x$qsa_nome_socio,
        qsa_codigo_faixa_etaria = x$qsa_codigo_faixa_etaria,
        qsa_data_entrada_sociedade = x$qsa_data_entrada_sociedade,
        qsa_identificador_de_socio = x$qsa_identificador_de_socio,
        qsa_codigo_qualificacao_socio = x$qsa_codigo_qualificacao_socio,
        qsa_codigo_qualificacao_representante_legal = x$qsa_codigo_qualificacao_representante_legal,
        qsa_qualificacao_socio = x$qsa_qualificacao_socio
      )
    )
  })


  # Combinando os data frames da lista em um único data frame
  df_qsa_df <- do.call(rbind, temp_list)
}



# Executando as funções para cada conjunto de dados
df_geral <- obter_dados_gerais(cnpj)
df_qsa_cnae_sec <- obter_cnaes_secundarios(cnpj)
df_qsa<- obter_dados_societarios(cnpj)

# Unificando os data frames
df_final <- df_geral %>%
  select(-razao_social) %>%
  inner_join(df_qsa, by = 'cnpj', relationship = "many-to-many") %>%
  inner_join(., df_qsa_cnae_sec, by = 'cnpj', relationship = "many-to-many")

return(df_final)

}

