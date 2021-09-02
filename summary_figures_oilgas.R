library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(showtext)
font_add_google("EB Garamond")
showtext_auto()
source("plot_summary_col.R")
source("utils.R")
data_path <- "/Users/monikafurdyna/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/1in1000/03_Content/01_Long-term team/05_Green recovery/Data/"
data_file <- "Summary of indicators.xlsx"

data_fin <- readxl::read_excel(
    path = paste0(data_path, data_file),
    sheet = "Oil&gas",
    range = "A6:J6",
    col_names = FALSE
  ) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  select(-2, -5, -8) %>%
  rename(
    category = 1,
    before_returns = 2,
    after_returns = 3,
    before_pe = 4,
    after_pe = 5,
    before_wacc = 6,
    after_wacc = 7
  ) %>%
  pivot_longer(
    cols = c("before_returns", "after_returns",
             "before_pe", "after_pe",
             "before_wacc", "after_wacc"),
    names_to = "what"
  ) %>%
  separate(
    col = what,
    into = c("value_type", "indicator"),
    sep = "_"
  ) %>%
  mutate(
    value_type = factor(value_type, levels = c("before", "after")),
    indicator = factor(indicator, levels = c("returns", "pe", "wacc"))
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
      category = factor(category, levels = c("Oil & gas companies"))
    )

levels(data_fin$category) <- c("bold('Oil & gas companies'^'i')")

labels_x_fin <- c("2019 Q4", expression("2020 Q4"^"iv"))

p_ret <- plot_summary_col(
  data_fin %>% filter(indicator == "returns"),
  labels.x = labels_x_fin,
  parse.facet.labels = TRUE)
p_ret <- p_ret + labs(title = "Stock returns")

p_pe <- plot_summary_col(
  data_fin %>% filter(indicator == "pe"),
  show.strip = FALSE,
  labels.x = labels_x_fin
  )
p_pe <- p_pe + labs(title = "PE ratio")

p_wacc <- plot_summary_col(
  data_fin %>% filter(indicator == "wacc"),
  show.strip = FALSE,
  scale.percent = TRUE,
  labels.x = labels_x_fin
  )
p_wacc <- p_wacc +
  labs(title = "WACC")

p_ret | p_pe | p_wacc

val_divisor <- 10 ^ 3

data_econ <- readxl::read_excel(
    path = paste0(data_path, data_file),
    sheet = "Oil&gas",
    range = "A17:G21",
    col_names = FALSE
  ) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  select(-2, -5) %>%
  rename(
    category = 1,
    before_sales = 2,
    after_sales = 3,
    before_production = 4,
    after_production = 5
  ) %>%
  pivot_longer(
    cols = c("before_sales", "after_sales",
             "before_production", "after_production"),
    names_to = "what"
  ) %>%
  drop_na() %>%
  separate(
    col = what,
    into = c("value_type", "indicator"),
    sep = "_"
  ) %>%
  mutate(
    value_type = factor(value_type, levels = c("before", "after")),
    indicator = factor(indicator, levels = c("sales", "production")),
    value = if_else(
      (category == "Total oil products") & (indicator == "production"),
      value,
      value / val_divisor
  )) %>%
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
    what_value = colour_difference_econ(.data)
  ) %>%
  mutate(what_value = factor(
    what_value,
    levels = c("diff_grey", "diff_green", "diff_red", "value_bar")),
    category = case_when(
      category == "Total oil products" ~ "Total\noil\nproducts",
      category == "Road fuels" ~ "Road\nfuels",
      category == "Aviation fuels" ~ "Aviation\nfuels",
      category == "Int. marine fuels" ~ "Int.\nmarine\nfuels",
      category == "Natural gas" ~ "Natural\ngas",
      TRUE ~ category
    ),
    category = factor(
      category,
      levels = c("Total\noil\nproducts", "Road\nfuels", "Aviation\nfuels",
                             "Int.\nmarine\nfuels", "Natural\ngas"))
    )

labels_x_sales <- c("2019 Q4", expression("2020 Q4"^"ii"))

p_s1 <- plot_summary_col(
  data_econ %>%
    filter(
      indicator == "sales",
      category != "Natural\ngas"),
  labels.x = labels_x_sales) +
scale_y_continuous(
      expand = expansion(mult = c(0, 0.35)),
      position = "left"
    )
p_s1 <- p_s1 +
  labs(
  title = "Sales",
  subtitle = "(millions tonnes)"
  )
p_s1

p_s2 <- plot_summary_col(
  data_econ %>%
    filter(
      indicator == "sales",
      category == "Natural\ngas"),
  labels.x = labels_x_sales) +
scale_y_continuous(
      expand = expansion(mult = c(0, 0.3)),
      position = "left"
    )
p_s2 <- p_s2 +
  labs(
  subtitle = "(billions m3)"
  )

labels_x_prod <- c("2019 Q4", expression("2020 Q4"^"iii"))

p_prod1 <- plot_summary_col(
  data_econ %>% filter(
    indicator == "production",
    category != "Natural\ngas"),
  labels.x = labels_x_prod) +
  scale_y_continuous(
      expand = expansion(mult = c(0, 0.3)),
      position = "left"
    )
p_prod1 <- p_prod1 +
  labs(
    title = "Production plans",
    subtitle = "(millions barrels)"
    )

p_prod2 <- plot_summary_col(
  data_econ %>% filter(
    indicator == "production",
    category == "Natural\ngas"),
  labels.x = labels_x_prod) +
  scale_y_continuous(
      expand = expansion(mult = c(0, 0.3)),
      position = "left"
    )
p_prod2 <- p_prod2 +
  labs(
    subtitle = "(billions m3)"
    )

p_s1 + p_prod1 + p_s2 + p_prod2 +
  plot_layout(nrow = 2, ncol = 2, widths = c(1, 1, 1, 1), heights = c(3, 1))

########### Stress Test ################

data_path <- "/Users/monikafurdyna/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/1in1000/03_Content/01_Long-term team/05_Green recovery/Data/Financial indicators/For Monika/"
data_file <- "Potential transition financial losses ind. 6.xlsx"

value_div <- 10^9

data_st_2019 <- get_data_xlx_stress_test(
  paste0(data_path, data_file),
  "Oil & gas",
  "A14:C16",
  2019)

data_st_2020 <- get_data_xlx_stress_test(
  paste0(data_path, data_file),
  "Oil & gas",
  "D14:F16",
  2020)

data_st <- rbind(data_st_2019, data_st_2020) %>%
  mutate(
    category = if_else(
      category == "technology",
      "Technology",
      category
    )
  ) %>%
  group_by(technology, year, category) %>%
  summarise(value = -max(perc_diff)/100) %>%
  group_by(technology, category) %>%
  mutate(
    diff = value - lag(value, default = first(value)),
    value_bar = case_when(
      diff >= 0 & value >= 0 ~ value - diff,
      diff < 0 & value >= 0 ~ value,
      diff >= 0 & value < 0 ~ value,
      diff < 0 & value < 0 ~ value + diff,
    ),
    is_diff_positive = diff >= 0
    ) %>%
  mutate(
    diff = if_else(
      value >= 0,
      abs(diff),
      -abs(diff)
    )
  ) %>%
  pivot_longer(
    cols = c("value_bar", "diff"),
    names_to = "what_value",
    values_to = "value_bar"
  ) %>%
  rename(
    indicator = category,
    category = technology
  ) %>%
  mutate(
    value_type = case_when(
      year == 2019 ~ "before",
      year == 2020 ~ "after"
    )
  ) %>%
  rowwise() %>%
  mutate(
    what_value = colour_difference_st(.data)
  ) %>%
  mutate(
    what_value = factor(what_value, levels = c("diff_grey", "diff_green", "diff_red", "value_bar")),
    value_type = factor(value_type, levels = c("before", "after"))
  )

labels_x_st <- c("2019 Q4", expression("2020 Q4"^"v"))

p1 <- plot_summary_col_st(
  data_st %>% filter(category == "Oil"),
  labels.x = labels_x_st
  )
p1 <- p1 +
  labs(subtitle = "Oil")
p1

p2 <- plot_summary_col_st(
  data_st %>% filter(category == "Gas"),
  show.strip = FALSE,
  labels.x = labels_x_st
  )
p2 <- p2 +
  labs(subtitle = "Gas")
p2

p_s <- plot_summary_col_st(
  data_st %>% filter(category == "Sector"),
  labels.x = labels_x_st)
p_s <- p_s
p_s

p_tech <- (p1 | p2 )
p_s + p_tech +
  plot_layout(ncol = 2, widths = c(0.9, 2)) +
  plot_annotation(
    title = "Potential transition financial loss",
    theme = theme_green_recovery()
  )

