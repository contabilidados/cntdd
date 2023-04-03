# Load raw data from .csv file
dt_contabil <- read.csv("data-raw/ratio.csv", sep = ";", na.strings = "-")
names(dt_contabil)[1] <- "empresa"
dt_meses <- read.csv("data-raw/meses.csv", encoding = "UTF-8")
dt_CNPJ <-
  read.csv("data-raw/cnpj_codCVM_codB3.csv", sep = ";") %>%
  rowwise() %>%
  mutate(
    cnpj = utl_CNPJ_Mascara(cnpj),
    codB3 = substr(codB3, 1, 4))
# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(dt_contabil, overwrite = T)
usethis::use_data(dt_meses, overwrite = T)
usethis::use_data(dt_CNPJ, overwrite = T)

