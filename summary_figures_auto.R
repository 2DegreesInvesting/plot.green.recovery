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

data_auto_fin <- readxl::read_excel(
    path = paste0(data_path, data_file),
    sheet = "Automotive",
    range = "A10:J12",
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
      category = factor(category, levels = c("Leaders", "Waverers", "Laggards"))
    )

levels(data_auto_fin$category) <- c("bold('Leaders'^'i')", "bold(Waverers)", "bold(Laggards)")

labels_x_fin <- c("2019 Q4", expression("2020 Q4"^"iv"))

p_ret <- plot_summary_col(
  data_auto_fin %>% filter(indicator == "returns"),
  labels.x = labels_x_fin,
  parse.facet.labels = TRUE)
p_ret <- p_ret + labs(title = "Stock returns")

p_pe <- plot_summary_col(
  data_auto_fin %>% filter(indicator == "pe"),
  show.strip = FALSE,
  labels.x = labels_x_fin
  )
p_pe <- p_pe + labs(title = "PE ratio")

p_wacc <- plot_summary_col(
  data_auto_fin %>% filter(indicator == "wacc"),
  show.strip = FALSE,
  scale.percent = TRUE,
  labels.x = labels_x_fin
  )
p_wacc <- p_wacc +
  labs(title = "WACC")

p_ret | p_pe | p_wacc
ggsave(
  filename="charts/auto_summ_fin.png",
  width=20,
  height=10,
  units="cm",
  dpi=350)

val_divisor <- 10 ^ 3

data_auto_econ <- readxl::read_excel(
    path = paste0(data_path, data_file),
    sheet = "Automotive",
    range = "A22:G24",
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
  separate(
    col = what,
    into = c("value_type", "indicator"),
    sep = "_"
  ) %>%
  mutate(
    value_type = factor(value_type, levels = c("before", "after")),
    indicator = factor(indicator, levels = c("sales", "production")),
    value = round(value / val_divisor, digits = 0)
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
    what_value = colour_difference_econ(.data)
  ) %>%
  mutate(what_value = factor(
    what_value,
    levels = c("diff_grey", "diff_green", "diff_red", "value_bar")),
    category = factor(category, levels = c("ICE vehicles", "Electric vehicles", "Hybrid vehicles"))
    )

labels_x_sales <- c("2019 Q4", expression("2020 Q4"^"ii"))

p_s <- plot_summary_col(
  data_auto_econ %>% filter(indicator == "sales"),
  labels.x = labels_x_sales)
p_s <- p_s +
  labs(
  title = "Sales",
  subtitle = "(thousands)"
  )

labels_x_prod <- c("2019 Q4", expression("2020 Q4"^"iii"))

p_prod <- plot_summary_col(
  data_auto_econ %>% filter(indicator == "production"),
  show.strip = FALSE,
  labels.x = labels_x_prod)
p_prod <- p_prod +
  labs(
    title = "Production plans",
    subtitle = "(thousands)"
    )

p_s | p_prod
ggsave(
  filename="charts/auto_summ_economy.png",
  width=16,
  height=14,
  units="cm",
  dpi=350)

data_path <- "/Users/monikafurdyna/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/1in1000/03_Content/01_Long-term team/05_Green recovery/Data/Financial indicators/For Monika/"
data_file <- "Potential transition financial losses ind. 6.xlsx"

value_div <- 10^9

data_auto_st_2019 <- get_data_xlx_stress_test(
  paste0(data_path, data_file),
  "Automotive",
  "A7:C10",
  2019)

data_auto_st_2020 <- get_data_xlx_stress_test(
  paste0(data_path, data_file),
  "Automotive",
  "D7:F10",
  2020)

data_auto_st <- rbind(data_auto_st_2019, data_auto_st_2020) %>%
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

p_e <- plot_summary_col_st(
  data_auto_st %>% filter(category == "Electric"),
  show.strip = FALSE,
  labels.x = labels_x_st
  )
p_e <- p_e +
  labs(subtitle = "Electric")
p_e

p_h <- plot_summary_col_st(
  data_auto_st %>% filter(category == "Hybrid"),
  show.strip = FALSE,
  labels.x = labels_x_st
  )
p_h <- p_h +
  labs(subtitle = "Hybrid")
p_h

p_ice <- plot_summary_col_st(
  data_auto_st %>% filter(category == "ICE"),
  labels.x = labels_x_st
  )
p_ice <- p_ice +
  labs(subtitle = "ICE")
p_ice

p_s <- plot_summary_col_st(
  data_auto_st %>% filter(category == "Sector"),
  labels.x = labels_x_st)
p_s <- p_s
p_s

p_tech <- p_ice | p_h | p_e
p_s + p_tech +
  plot_layout(ncol = 2, widths = c(0.8, 3)) +
  plot_annotation(
    title = "Potential transition financial loss",
    theme = theme_green_recovery()
  )

