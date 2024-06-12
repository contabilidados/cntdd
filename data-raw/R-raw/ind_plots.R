ind_plots <- function(bd, ratio, titulo = NULL, subtitulo = NULL, rodape = paste0("@", year(Sys.Date()), " contabiliDados")){

  if(min(bd[2]) < 0){
    minimo <- min(bd[2]) + min(bd[2])*2
  } else {
    minimo <- min(bd[2]) - min(bd[2])*2
  }

  if(max(bd[2]) < 0){
    maximo <- max(bd[2]) - max(bd[2])*2
  } else {
    maximo <- max(bd[2]) + max(bd[2])*2
  }

  bd %>%
    ggplot(aes(eval(parse(text = "factor(Periodo)")), .data[[ratio]])) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.2) +
    ylim(minimo, maximo) +
    geom_text(aes(label=format(round(.data[[ratio]], 2), nsmall = 2)), vjust=1.2,
              color="orange", size=6) +
    xlab("") + ylab("") +
    labs(title = titulo,
         subtitle = subtitulo,
         caption = rodape) +
    theme(title = element_text(face = "bold", colour = "orange"),
          panel.background = element_blank())
}



