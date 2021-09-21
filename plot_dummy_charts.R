library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
font_add_google("EB Garamond")
showtext_auto()
source("plot_summary_col.R")
source("utils.R")

prep_data <- function(data) {
  data_out <- data %>%
    mutate(
      value_type = factor(value_type, levels = c("before", "after"))
    ) %>%
    arrange(category, indicator, value_type) %>%
    group_by(category, indicator) %>%
    mutate(
      diff = value - lag(value, default = first(value)),
      value_bar = if_else(
        diff >= 0,
        value - diff,
        value
      ),
      is_diff_positive = diff >= 0
      ) %>%
    mutate(
      diff = abs(diff)
    ) %>%
    pivot_longer(
      cols = c("value_bar", "diff"),
      names_to = "what_value",
      values_to = "value_bar"
    ) %>%
    rowwise() %>%
    mutate(
      what_value = colour_difference_fin(.data)
    ) %>%
    mutate(
      what_value = factor(
        what_value,
        levels = c("diff_grey", "diff_green", "diff_red", "value_bar")),
        category = factor(category, levels = c("Leaders", "Waverers", "Laggards"))
      )

  data_out
}

data_dummy <- readxl::read_excel(
  path = "data/data_dummy.xlsx",
  range = "A1:E13",
  col_names = TRUE
)

data_increase <- prep_data(
  data_dummy %>%
  filter(
    dummy_type == "increase"
  ))

data_decrease <- prep_data(
  data_dummy %>%
  filter(
    dummy_type == "decrease"
  ))

for (cat in list("Leaders", "Waverers", "Laggards")) {
  data_p <- data_increase %>%
    filter(
      category == cat
    )
  p <- plot_summary_col_dummy(data_p)
  print(p)
  Sys.sleep(2)

  data_p1 <- data_decrease %>%
    filter(
      category == cat
    )
  p1 <- plot_summary_col_dummy(data_p1)
  print(p1)
  Sys.sleep(2)
}
