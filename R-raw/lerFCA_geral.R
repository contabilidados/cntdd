pacman::p_load(readxl, tidyverse)

# Leitura arquivo FCA ----

readCVM.fca <- function(ano){

  read.csv(
    unz(
      paste0("D:/dados/fca/fca_cia_aberta_", ano, ".zip"),
             paste0("fca_cia_aberta_geral_", ano, ".csv")),
             header = T, sep = ";", encoding = "latin1",
             col.names =
               c(
                 "cnpj", "dataReferencia", "versao", "idDocumento",
                 "nomeEmpresarial", "dataNomeEmpresarial", "nomeEmpresarialAnterior",
                 "dataConstituicao", "codigoCVM", "dataRegistroCVM", "categoriaRegistroCVM",
                 "dataCategoriaRegistroCVM", "situacaoRegistroCVM", "dataSituacaoRegistroCVM",
                 "paisOrigem", "paisCustodiaVrMobiliario", "setorCVM", "descricaoAtividade",
                 "situacaoEmissor", "dataSituacaoEmissor", "especieControleAcionario",
                 "dataEspecieControleAcionario", "diaEncerraExercSocial", "mesEncerraExercSocial",
                 "dataAlteracaoExercSocial", "site"
               )) %>%
        select(
          cnpj, dataReferencia, codigoCVM, dataConstituicao, dataSituacaoRegistroCVM,
          nomeEmpresarial, especieControleAcionario, mesEncerraExercSocial,
          setorCVM, descricaoAtividade) %>%
    mutate(
      dataReferencia = as.Date(dataReferencia),
      codigoCVM2 = sprintf("%05i",codigoCVM),
      codigoCVM = as.character(codigoCVM),
      codigoCVMsemDigito = str_remove(codigoCVM2, "^0+"),
      codigoCVMsemDigito = substr(codigoCVMsemDigito, 1, nchar(codigoCVMsemDigito)-1)
    ) %>%
    relocate(codigoCVM2, codigoCVMsemDigito, .after = codigoCVM)-> result

  return(result)

}

purrr::map_df(2010:2023, readCVM.fca) %>%
  group_by(cnpj) %>%
  filter(dataReferencia == max(dataReferencia)) %>%
  arrange(dataConstituicao) %>%
  ungroup() -> dtFCA

# Código B3 de várias fontes ----

rbind(
read_xlsx("D:/dados/cnpjsB3/dadosCNPJsB3.xlsx", sheet = 1, skip = 1,
          col_names = c("empresa", "codB3", "cnpj", "bancoEscriturador"))  %>%
  transmute(
    cnpj,
    codB3 = substr(codB3, 1, 4)
    ),

read_xlsx("D:/dados/cnpjsB3/dadosCNPJsB3.xlsx", sheet = 2, skip = 1,
          col_names = c("empresa", "classe", "codB3", "codCVM", "cnpj"))  %>%
  transmute(
    cnpj = cntdd::utl_CNPJ_Mascara(cnpj),
    codB3 = substr(codB3, 1, 4)
    ),

read_xlsx("D:/dados/cnpjsB3/dadosCNPJsB3.xlsx", sheet = 3, skip = 1,
          col_names = c("empresa", "codB3", "cnpj"))  %>%
  transmute(
    cnpj,
    codB3 = substr(codB3, 1, 4)
    ),

read_xlsx("D:/dados/cnpjsB3/dadosCNPJsB3.xlsx", sheet = 4, skip = 1,
          col_names = c("empresa", "codB3", "cnpj"))  %>%
  na.omit() %>%
  transmute(
    cnpj,
    codB3 = substr(codB3, 1, 4)
    ),

read_xlsx("D:/dados/cnpjsB3/dadosCNPJsB3.xlsx", sheet = 5, skip = 1,
          col_names = c("codB3", "nome", "nome2", "cnpj"))  %>%
  transmute(
    cnpj = cntdd::utl_CNPJ_Mascara(cnpj),
    codB3 = substr(codB3, 1, 4)
    )
) %>%
  unique() %>%
  filter(!cnpj %in% c("00.000.000/0000-NA", "00.000.000/000000-")) %>%
  arrange(cnpj, codB3) -> dtCodB3

# Gera base de dados para cntdd ----

dtFCA %>%
  full_join(dtCodB3, by = "cnpj") %>%
  filter(!is.na(dataReferencia)) %>%
  relocate(codB3, setorCVM, .after = "codigoCVMsemDigito") -> dtCVMB3


# write.csv(dtCVMB3, "data-raw/dtCVMB3.csv", row.names = F)

