ind_plots <- function(bd, ratio){
  bd %>%
    ggplot(aes(eval(parse(text = "factor(Periodo)")), .data[[ratio]])) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label=format(round(.data[[ratio]], 2), nsmall = 2)), vjust=1.6,
              color="white", size=6) +
    xlab("Periodos") + ylab(ratio) +
    labs(title = paste0("Analise da ", ratio),
         caption = "@2022 contabiliDados") +
    theme(title = element_text(face = "bold", colour = "orange"),
          axis.text.y =  element_blank(), panel.background = element_blank())
}

