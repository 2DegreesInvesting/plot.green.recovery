library(r2dii.plot)
library(ggplot2)
library(rlang)

r2dii_tech_pal <- function(sector, r2dii_labels = NULL) {
  r2dii_labels <- r2dii_labels %||%
    r2dii.plot:::technology_colours$technology
  values <- tibble(technology = r2dii_labels) %>%
    inner_join(
      r2dii.plot:::technology_colours %>% filter(.data$sector == !!sector),
      by = "technology") %>%
    pull(hex)
  max_n <- length(values)
  f <- scales:::manual_pal(values)
  attr(f, "max_n") <- max_n
  f
}

scale_colour_r2dii_tech <- function(sector, r2dii_labels = NULL, ...) {
  discrete_scale("colour", "r2dii", r2dii_tech_pal(sector, r2dii_labels), ...)
}

scale_color_r2dii_tech <- scale_colour_r2dii_tech

scale_fill_r2dii_tech <- function(sector, r2dii_labels = NULL, ...) {
  discrete_scale("fill", "r2dii", r2dii_tech_pal(sector, r2dii_labels), ...)
}
