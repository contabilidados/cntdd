# Load raw data from .csv file
dt_contabil <- read.csv("data-raw/ratio.csv", sep = ";", na.strings = "-")
names(dt_contabil)[1] <- "empresa"
dt_meses <- read.csv("data-raw/meses.csv", encoding = "UTF-8")
dt_ufRegiao <- read.csv("data-raw/ufRegiao.csv", encoding = "latim-1", sep = ";")
# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(dt_contabil, overwrite = T)
usethis::use_data(dt_meses, overwrite = T)
usethis::use_data(dt_ufRegiao, overwrite = T)
