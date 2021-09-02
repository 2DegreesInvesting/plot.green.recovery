library(ggplot2)
library(rlang)

one_in1000_palette <- function() {
  palette_colours <- tibble::tribble(
      ~label,      ~hex,
     "black", "#000000",
     "white", "#FFFFFF",
       "red", "#F53D3F",
      "blue", "#3d9bf5",
     "green", "#5D9324",
    "yellow", "#f5f33d",
    "violet", "#973df5",
    "orange", "#f5973d",
      "pink", "#f53d9b",
      "grey", "#BAB6B5"
    )
}

one_in1000_pal <- function(labels = NULL) {
  palette_colours <- one_in1000_palette()
  labels <- labels %||% palette_colours$label
  values <- tibble(label = labels) %>%
    inner_join(palette_colours, by = "label") %>%
    pull(hex)
  max_n <- length(values)
  f <- scales:::manual_pal(values)
  attr(f, "max_n") <- max_n
  f
}

scale_colour_one_in1000 <- function(labels = NULL, ...) {
  discrete_scale("colour", "one_in1000", one_in1000_pal(labels), ...)
}

scale_color_one_in1000 <- scale_colour_one_in1000

scale_fill_one_in1000 <- function(labels = NULL, ...) {
  discrete_scale("fill", "one_in1000", one_in1000_pal(labels), ...)
}
