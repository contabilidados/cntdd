# Load raw data from .csv file
dt_contabil <- read.csv("data-raw/ratio.csv", sep = ";", na.strings = "-")
names(dt_contabil)[1] <- "empresa"
dt_meses <- read.csv("data-raw/meses.csv", encoding = "UTF-8")
dt_ufRegiao <- read.csv("data-raw/ufRegiao.csv", encoding = "latin1", sep = ";")
dt_cvmB3 <- read.csv("data-raw/dtCVMB3.csv", encoding = "latim-1", sep = ",")
dt_cvmB3$descricaoAtividade <- iconv(dt_cvmB3$descricaoAtividade, from = 'UTF-8', to = 'ASCII//TRANSLIT')
dt_cvmB3$setorCVM <- iconv(dt_cvmB3$setorCVM, from = 'UTF-8', to = 'ASCII//TRANSLIT')
dt_cvmB3$nomeEmpresarial <- iconv(dt_cvmB3$nomeEmpresarial, from = 'UTF-8', to = 'ASCII//TRANSLIT')
dt_cvmB3$descricaoAtividade <- stringr::str_to_sentence(dt_cvmB3$descricaoAtividade)
dt_cvmB3$setorCVM <- stringr::str_to_sentence(dt_cvmB3$setorCVM)
dt_cvmB3$nomeEmpresarial <- toupper(dt_cvmB3$nomeEmpresarial)

# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(dt_contabil, overwrite = T)
usethis::use_data(dt_meses, overwrite = T)
usethis::use_data(dt_ufRegiao, overwrite = T)
usethis::use_data(dt_cvmB3, overwrite = T)

