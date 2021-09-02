theme_green_recovery <- function(...) {
    r2dii.plot::theme_2dii(
      base_family = "EB Garamond",
      base_size = 22,
      ...) +
    theme(
      panel.background = element_rect(fill = "#E9E4E3"),
      plot.background = element_rect(fill = "#E9E4E3"),
      legend.background = element_rect(fill = "#E9E4E3")
    )
}
