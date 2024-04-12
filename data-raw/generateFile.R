# Load raw data from .csv file
dt_contabil <- read.csv("data-raw/ratio.csv", sep = ";", na.strings = "-")
names(dt_contabil)[1] <- "empresa"
dt_meses <- read.csv("data-raw/meses.csv", encoding = "UTF-8")
dt_cnpjCodB3 <-
  read.csv("data-raw/cnpj_codCVM_codB3.csv", sep = ";") %>%
  rowwise() %>%
  mutate(
    cnpj = cntdd::utl_CNPJ_Mascara(cnpj),
    codB3 = substr(codB3, 1, 4))
dt_ufRegiao <- read.csv("data-raw/ufRegiao.csv", encoding = "latim-1", sep = ";")
dt_cadastroCVM <- read.csv("data-raw/dtCadastroCVM.csv", encoding = "latim-1", sep = ",")
# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(dt_contabil, overwrite = T)
usethis::use_data(dt_meses, overwrite = T)
usethis::use_data(dt_cnpjCodB3, overwrite = T)
usethis::use_data(dt_ufRegiao, overwrite = T)
usethis::use_data(dt_cadastroCVM, overwrite = T)
load("data/dt_cadastroCVM.rda")
