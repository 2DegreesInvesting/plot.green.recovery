library(ggplot2)
library(rlang)

palette_colours <- tibble::tribble(
       ~label,      ~hex,
        "red", "#fa9e9f",
      "green", "#CDFA9E",
       "grey", "#BAB6B5",
  "dark_grey", "#8B8888",
"darker_grey", "#656262"
  )

one_in1000_val_diff_pal <- function(labels = NULL) {
  labels <- labels %||% palette_colours$label
  values <- tibble(label = labels) %>%
    inner_join(palette_colours, by = "label") %>%
    pull(hex)
  max_n <- length(values)
  f <- scales:::manual_pal(values)
  attr(f, "max_n") <- max_n
  f
}

scale_colour_one_in1000_val_diff <- function(labels = NULL, ...) {
  discrete_scale("colour", "one_in1000", one_in1000_val_diff_pal(labels), ...)
}

scale_color_one_in1000_val_diff <- scale_colour_one_in1000_val_diff

scale_fill_one_in1000_val_diff <- function(labels = NULL, ...) {
  discrete_scale("fill", "one_in1000", one_in1000_val_diff_pal(labels), ...)
}
