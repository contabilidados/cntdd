cntdd_theme <- function(){

  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.y = element_text(color = "snow4"),
        axis.text.x = element_text(color = "snow4"),
        plot.title = element_text(color = 'lightblue3', size = 15),
        plot.subtitle = element_text(color = "snow4", size = 10),
        plot.caption = element_text(color = "snow3", size = 8),
        panel.background = element_blank(),
        panel.grid = element_line(color = "snow2"))

}
