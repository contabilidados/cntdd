dados <- data.frame(a = rnorm(200), b = rnorm(200), ret = rnorm(200))



library(tidyverse)


tableStat <- function(bd, grp1, n_grp1, name_grp1, grp2, n_grp2, name_grp2, value, ...){

  bd %>% dplyr::select({{value}}) %>% names -> vlName

  bd %>%
    mutate(
      grpA = cut({{grp1}},
                   quantile({{grp1}}, probs = 0:n_grp1/n_grp1),
                   include.lowest = T,
                   labels = paste0(name_grp1, "_", 1:n_grp1)),
      grpB = cut({{grp2}},
                   quantile({{grp2}}, probs = 0:n_grp2/n_grp2),
                   include.lowest = T,
                   labels = paste0(name_grp2, "_", 1:n_grp2))
    ) %>%
    group_by(grpA, grpB) %>%
    summarise(across({{value}}, ...), .groups = "drop") %>%
    pivot_wider(names_from = grpB, values_from = {{value}}) -> result

  names(result)[1] <- paste0("Stat_", vlName)

  return(result)

}

tableStat(mtcars, qsec, 2, "sec", disp, 2, "disp", mpg, mean)


createGroups <- function(bd, col_Value, col_Grp, n_grp = 2, lbl_grp = "grp") {
  dplyr::mutate(.data = bd, {{col_Grp}} :=
                  cut(bd[[col_Value]],
                      quantile(bd[[col_Value]], probs = 0:n_grp/n_grp),
                      include.lowest = T,
                      labels = paste0(lbl_grp, "_", 1:n_grp)))
}
createGroups(mtcars, "mpg", "grp.mpg", 2)
mtcars %>%
  createGroups(., "mpg", "grp.mpg", 2) %>%
  createGroups(., "qsec", "grp.qsec", 2)
library(tidyverse)
analise <- ind_liqSeca(indicador = "Liquidez Seca", periodo = 2018:2020, estoque = c(2000,3000,4000), atvCirc = c(5000,6000,7000), pasCirc = c(1000,2000,3000))
analise$`Como Analisar` %>% sjPlot::tab_df(encoding = "Latim-1")

