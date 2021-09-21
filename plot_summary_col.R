library(showtext)
font_add_google("EB Garamond")
showtext_auto()

source("one_in1000_value_diff_palette.R")
source("theme_green_recovery.R")

find_colour_labels <- function(data) {
  labels_all <- c("grey", "green", "red", "dark_grey")
  colour_vals_all <- c("diff_grey", "diff_green", "diff_red", "value_bar")

  absent_values <- setdiff(colour_vals_all, as.character(unique(data$what_value)))
  if (length(absent_values) == 0) {
    return(labels_all)
  } else if (length(absent_values) == 1) {
    if (absent_values == "diff_grey") {
      return(c("green", "red", "dark_grey"))
    } else if (absent_values == "diff_green") {
      return(c("grey", "red", "dark_grey"))
    } else if (absent_values == "diff_red") {
      return(c("grey", "green", "dark_grey"))
    }
  } else if (length(absent_values) == 2) {
    if (all(absent_values == c("diff_grey", "diff_green"))) {
      return(c("red", "dark_grey"))
    } else if (all(absent_values == c("diff_grey", "diff_red"))) {
      return(c("green", "dark_grey"))
    } else if (all(absent_values == c("diff_green", "diff_red"))) {
      return(c("grey", "dark_grey"))
    }
  } else {
    return(c("dark_grey"))
  }
}

plot_summary_col <- function(
  data,
  show.strip = TRUE,
  scale.percent = FALSE,
  labels.x = NULL,
  parse.facet.labels = FALSE
  ) {
  font_size <- 6

  data_annotation <- data %>%
  filter(
        what_value == "value_bar"
  ) %>%
  group_by(category, indicator, value_type, what_value) %>%
  summarise(value = max(value)) %>%
  pivot_wider(
    names_from = value_type,
    values_from = value
  ) %>%
  mutate(
    percentage = if_else(
      indicator == "wacc",
      if_else(
        (after - before) * 100 > 1.5,
        round((after - before) * 100, digits = 0),
        round((after - before) * 100, digits = 1)
        ),
      round(((after - before)/before) * 100, digits = 0),
    ),
    label = ifelse(
      percentage > 0,
      paste0("+",percentage, "%"),
      paste0(percentage, "%")
    )
  )

  if (scale.percent) {
    data <- data %>%
      left_join(
        (data_annotation %>% select(category, indicator, what_value, percentage)),
        by = c("category", "indicator", "what_value")) %>%
      mutate(
        label = if_else(
          abs(percentage) > 1.5,
          paste0(as.character(round(value * 100, digits = 0)), "%"),
          paste0(as.character(round(value * 100, digits = 1)), "%")
        )
      )
  } else {
    data <- data %>%
      left_join(
        (data_annotation %>% select(category, indicator, what_value, percentage)),
        by = c("category", "indicator", "what_value")) %>%
      mutate(
        label = if_else(
          (abs(percentage) == 0) | (abs(percentage) > 1.5),
          scales::label_comma(accuracy = NULL)(value),
          scales::label_comma(accuracy  = .1)(value)
        )
      )
  }

  colour_labels <- find_colour_labels(data)
  p <- ggplot(
  data = data,
  aes(
    x = value_type,
    y = value_bar,
    fill = what_value
  )) +
  geom_bar(
      position = "stack",
      stat = "identity",
      width = 0.4
    ) +
    scale_fill_one_in1000_val_diff(labels = colour_labels)

  if (scale.percent) {
    p <- p +
    geom_text(
      data = data %>%
        filter(
          what_value == "value_bar"
        ),
      aes(
        label = label,
        x = value_type,
        y = value
      ),
      vjust = - 0.6,
      family = "EB Garamond",
      size = font_size
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.3))
    )
  } else {
    p <- p +
    geom_text(
      data = data %>%
        filter(
          what_value == "value_bar"
        ),
      aes(
        label = label,
        x = value_type,
        y = value
      ),
      vjust = - 0.6,
      family = "EB Garamond",
      size = font_size
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.3))
    )
  }

  p <- p +
  geom_segment(
    data = data_annotation,
    aes(
      x = 1.25,
      xend = 1.75,
      y = before,
      yend = after
    ),
    arrow = arrow(length = unit(0.3, "cm"))
  ) +
  geom_text(
    data = data_annotation,
    aes(
      label = label,
      x = 1.5,
      y = 0
    ),
    vjust = - 0.6,
    hjust = 0.5,
    family = "EB Garamond",
    size = font_size,
    fontface = "bold"
  ) +
  theme_green_recovery() +
  theme(
    legend.position = "none",
    axis.title = element_blank()
  )

  if (parse.facet.labels) {
    p <- p +
      facet_wrap(
        ~category,
        scales = "free_y",
        ncol = 1,
        strip.position = "left",
        labeller = label_parsed)
  } else {
    p <- p +
      facet_wrap(
        ~category,
        scales = "free_y",
        ncol = 1,
        strip.position = "left")
  }

  if (is.null(labels.x)) {
    p <- p +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank()
        )
  } else {
    p <- p +
      scale_x_discrete(labels = labels.x) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
        )
  }

  if (show.strip == TRUE) {
    p <- p +
      theme(
        strip.placement = "outside",
        strip.text = element_text(face = "bold")
        )
  } else {
    p <- p +
      theme(
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_blank()
      )
  }

  p
}

plot_summary_col_st <- function(data, show.strip = TRUE, labels.x = NULL) {
  font_size <- 6

  data_annotation <- data %>%
  filter(
        what_value == "value_bar"
  ) %>%
  group_by(category, indicator, what_value, value_type) %>%
  summarise(value = max(value)) %>%
  pivot_wider(
    names_from = value_type,
    values_from = value
  ) %>%
  mutate(
    percentage = round((after - before) * 100, digits = 0),
    label = ifelse(
      percentage > 0,
      paste0("+",percentage, "%"),
      paste0(percentage, "%")
    )
  )

  colour_labels <- find_colour_labels(data)
  p <- ggplot(
  data = data,
  aes(
    x = value_type,
    y = value_bar,
    fill = what_value
  )) +
  geom_bar(
      position = "stack",
      stat = "identity",
      width = 0.4
    ) +
    scale_fill_one_in1000_val_diff(labels = colour_labels)

  p <- p +
    geom_text(
      data = data %>%
        filter(
          what_value == "value_bar"
        ),
      aes(
        label = paste0(as.character(value * 100), "%"),
        x = value_type,
        y = value
      ),
      vjust = - 0.6,
      family = "EB Garamond",
      size = font_size
    )

  if(min(data_annotation$after, na.rm = TRUE) >= 0 & min(data_annotation$before, na.rm = TRUE) >= 0) {
    p <- p +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        expand = expansion(mult = c(0, 0.2))
      )
  } else if (min(data_annotation$after, na.rm = TRUE) < 0 & min(data_annotation$after, na.rm = TRUE) < 0) {
      p <- p +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        expand = expansion(mult = c(0.2, 0))
      )
  } else {
      p <- p +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        expand = expansion(mult = c(0.2, 0.2))
      )
    }


  p <- p +
  geom_segment(
    data = data_annotation,
    aes(
      x = 1.25,
      xend = 1.75,
      y = before,
      yend = after
    ),
    arrow = arrow(length = unit(0.3, "cm"))
  ) +
  geom_text(
    data = data_annotation,
    aes(
      label = label,
      x = 1.5,
      y = min(0, after)
    ),
    vjust = - 0.6,
    hjust = 0.5,
    family = "EB Garamond",
    size = font_size,
    fontface = "bold"
  ) +
  theme_green_recovery() +
  theme(
    legend.position = "none",
    axis.title = element_blank()
  ) +
  facet_wrap(~indicator, scales = "free_y", ncol = 1, strip.position = "left")

  if (is.null(labels.x)) {
    p <- p +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank()
        )
  } else {
    p <- p +
      scale_x_discrete(labels =labels.x) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
        )
  }

  if (show.strip == TRUE) {
    p <- p +
      theme(
        strip.placement = "outside",
        strip.text = element_text(face = "bold")
        )
  } else {
    p <- p +
      theme(
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_blank()
      )
  }

  p
}

plot_summary_col_dummy <- function(data, labels.x = NULL) {
  font_size <- 6

  data_annotation <- data %>%
  filter(
        what_value == "value_bar"
  ) %>%
  group_by(category, indicator, what_value, value_type) %>%
  summarise(value = max(value)) %>%
  pivot_wider(
    names_from = value_type,
    values_from = value
  ) %>%
  mutate(
    percentage = round((after - before) * 100, digits = 0),
    label = ifelse(
      percentage > 0,
      "+",
      "-"
    )
  )

  colour_labels <- find_colour_labels(data)
  p <- ggplot(
  data = data,
  aes(
    x = value_type,
    y = value_bar,
    fill = what_value
  )) +
  geom_bar(
      position = "stack",
      stat = "identity",
      width = 0.4
    ) +
    scale_fill_one_in1000_val_diff(labels = colour_labels)


  p <- p +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.2))
    )

  p <- p +
  geom_segment(
    data = data_annotation,
    aes(
      x = 1.25,
      xend = 1.75,
      y = before,
      yend = after
    ),
    arrow = arrow(length = unit(0.3, "cm"))
  ) +
  geom_text(
    data = data_annotation,
    aes(
      label = label,
      x = 1.5,
      y = min(0, after)
    ),
    vjust = - 0.6,
    hjust = 0.5,
    family = "EB Garamond",
    size = font_size,
    fontface = "bold"
  ) +
  theme_green_recovery() +
  theme(
    legend.position = "none",
    axis.title = element_blank()
  ) +
  facet_wrap(~indicator, scales = "free_y", ncol = 1, strip.position = "left")

  p <- p +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
      )

  p
}

