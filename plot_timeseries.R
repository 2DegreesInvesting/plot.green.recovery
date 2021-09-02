library(showtext)
source("utils.R")
source("theme_green_recovery.R")

font_add_google("EB Garamond")

showtext_auto()

plot_timeseries <- function(
  data,
  colour_levels,
  colour_labels = NULL,
  size_values = NULL) {
  size_values <- size_values %||% rep(0.75, length(type_levels))

  p <- ggplot(
  data = data,
       aes(
         x = timestamp,
         y = value,
         colour = factor(type, levels = type_levels),
         size = factor(type, levels = type_levels)
       )
       ) +
  geom_line() +
  scale_colour_one_in1000(colour_labels) +
  scale_size_manual(values = size_values) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, NA)
  ) +
  theme_green_recovery()
}

library(rlang)
plot_timeseries_fin_pandemic <- function(
  data,
  type_levels,
  colour_labels,
  size_values = NULL,
  mark.end.month = FALSE) {

  p <- plot_timeseries(data, type_levels, colour_labels, size_values)
  p <- scale_dates_quarters(p, mark.end.month) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  p <- annotate_pandemic(p)

  data_annotate <- data %>%
    filter(
      important == "important",
      timestamp == max(data$timestamp)
    )

  value_span <- max(data$value, na.rm = TRUE) - min(data$value, na.rm = TRUE)
  p <- p +
    ggrepel::geom_text_repel(
      data = data_annotate,
      size = 6,
      aes(
        x = timestamp,
        y = value,
        label = type
      ),
      family = "EB Garamond",
      direction = "y",
      nudge_y = 0.01 * value_span,
      hjust = 0,
      xlim = c(as.Date("2018-01-01"), as.Date("2022-01-01"))
    ) +
    coord_cartesian(clip = 'off') +
    theme(
      legend.position = "none",
      plot.margin = unit(c(0.5, 3, 0.5, 0.5), "cm"))
  p
}
