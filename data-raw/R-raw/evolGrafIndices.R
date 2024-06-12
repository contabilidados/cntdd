evolGrafIndices <-
  function(
    titulo = titulo,
    subtitulo = subtitulo,
    rodape = rodape
    ){
    newggslopegraph(
      dtGraf,Periodo, ratio, Indicador,
      Title = 'titulo',
      SubTitle = "subtitulo",
      Caption = "rodape",
      DataLabelFillColor = "green",
      DataLabelPadding = 0.4, DataLabelLineSize = 0,
      WiderLabels = F, LineThickness = 1, LineColor = "black",
      XTextSize = 10, YTextSize = 4, TitleTextSize = 12,
      SubTitleTextSize = 10, CaptionTextSize = 8,
      TitleJustify = "left", SubTitleJustify = "left",
      CaptionJustify = "left", DataTextSize = 3.5,
      ThemeChoice = "bw"
    ) +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}


