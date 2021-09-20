library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(r2dii.plot)
library(patchwork)
library(tidytext)
library(ggnewscale)
source("one_in1000_colour_palette.R")
source("theme_green_recovery.R")
source("r2dii_tech_colours.R")
source("utils.R")
source("plot_summary_col.R")
data_path <- "/Users/monikafurdyna/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/1in1000/03_Content/01_Long-term team/05_Green recovery/Data/Emerging countries/"
data_file <- "For Monika - Emerging and developing countries.xlsx"

val_divisor <- 1

data_power <- data <- readxl::read_excel(
  path = paste0(data_path, data_file),
  sheet = "Data",
  range = "A2:H4",
  col_names = TRUE) %>%
  pivot_longer(
    cols = c(2, 3, 4, 5, 6, 7, 8),
    names_to = "year"
  ) %>%
  rename(
    asset_level_timestamp = 1
  ) %>%
  mutate(
    year = as.numeric(year),
    value = value / val_divisor,
    asset_level_timestamp = case_when(
      asset_level_timestamp == "2019Q4" ~ "2019",
      asset_level_timestamp == "2020Q4" ~ "2020"
    )
    ) %>%
  group_by(year, asset_level_timestamp) %>%
  drop_na()

p1 <- ggplot(
  data_power,
  aes(
    x = year,
    y = value,
    colour = asset_level_timestamp
  )) +
  geom_line() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, NA),
    labels = scales::comma,
    n.breaks = 6
  ) +
  scale_colour_one_in1000(labels = c("black", "grey"), name = "Power\n(as of end of year)") +
  theme_green_recovery() %+replace%
  theme(
    legend.title = element_text(size = 16)
  ) +
  labs(
   # title = "Total production planned per year",
    y = "MW",
    x = ""
  )
p1

val_divisor <- 10 ^ 6

data_auto <- data <- readxl::read_excel(
  path = paste0(data_path, data_file),
  sheet = "Data",
  range = "A22:H24",
  col_names = TRUE) %>%
  pivot_longer(
    cols = c(2, 3, 4, 5, 6, 7, 8),
    names_to = "year"
  ) %>%
  rename(
    asset_level_timestamp = 1
  ) %>%
  mutate(
    year = as.numeric(year),
    value = value / val_divisor,
    asset_level_timestamp = case_when(
      asset_level_timestamp == "2019Q4" ~ "2019",
      asset_level_timestamp == "2020Q4" ~ "2020"
    )
    ) %>%
  group_by(year, asset_level_timestamp) %>%
  drop_na()

p2 <- ggplot(
  data_auto,
  aes(
    x = year,
    y = value,
    colour = asset_level_timestamp
  )) +
  geom_line() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, NA),
    n.breaks = 6
  ) +
  scale_colour_one_in1000(labels = c("black", "grey"), name = "Automotive\n(as of end of year)") +
  theme_green_recovery() %+replace%
  theme(
    legend.title = element_text(size = 16)
  ) +
  labs(
   # title = "Total production planned per year",
    y = "Millions vehicles",
    x = ""
  )
p2

val_divisor <- 10 ^ 3
data_oil <- data <- readxl::read_excel(
  path = paste0(data_path, data_file),
  sheet = "Data",
  range = "A39:H41",
  col_names = TRUE) %>%
  pivot_longer(
    cols = c(2, 3, 4, 5, 6, 7, 8),
    names_to = "year"
  ) %>%
  rename(
    asset_level_timestamp = 1
  ) %>%
  mutate(
    year = as.numeric(year),
    value = value / val_divisor,
    asset_level_timestamp = case_when(
      asset_level_timestamp == "2019Q4" ~ "2019",
      asset_level_timestamp == "2020Q4" ~ "2020"
    ),
    sector = "Oil\n(Barells)"
    ) %>%
  group_by(year, asset_level_timestamp) %>%
  drop_na()

val_divisor <- 10 ^ 9
data_gas <- data <- readxl::read_excel(
  path = paste0(data_path, data_file),
  sheet = "Data",
  range = "A44:H46",
  col_names = TRUE) %>%
  pivot_longer(
    cols = c(2, 3, 4, 5, 6, 7, 8),
    names_to = "year"
  ) %>%
  rename(
    asset_level_timestamp = 1
  ) %>%
  mutate(
    year = as.numeric(year),
    value = value / val_divisor,
    asset_level_timestamp = case_when(
      asset_level_timestamp == "2019Q4" ~ "2019",
      asset_level_timestamp == "2020Q4" ~ "2020"
    ),
    sector = "Gas\n(m3)"
    ) %>%
  group_by(year, asset_level_timestamp) %>%
  drop_na()

data_oilgas <- rbind(data_oil, data_gas)

p3 <- ggplot(
  data_oilgas,
  aes(
    x = year,
    y = value,
    colour = asset_level_timestamp
  )) +
  geom_line() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, NA),
    n.breaks = 6
  ) +
  scale_colour_one_in1000(labels = c("black", "grey"), name = "As of end of year") +
  theme_green_recovery() %+replace%
  theme(
    legend.title = element_text(size = 16),
    legend.position = "bottom"
  ) +
  facet_wrap(~sector, scales = "free") +
  labs(
    y = "Billions",
    x = element_blank()
  )
p3

val_divisor <- 10 ^ 3
data_summ_p <- readxl::read_excel(
    path = paste0(data_path, data_file),
    sheet = "Data",
    range = "K3:M7",
    col_names = FALSE
  ) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  rename(
    category = 1,
    before = 2,
    after = 3,
  ) %>%
  pivot_longer(
    cols = c("before", "after"),
    names_to = "value_type"
  ) %>%
  mutate(
    indicator = "production",
    value_type = factor(value_type, levels = c("before", "after")),
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
    what_value = colour_difference_econ(.data),
    category = if_else(
      category == "Renewables (incl. hydro)",
      "Renewables\n(incl. hydro)",
      category
    )
  ) %>%
  mutate(what_value = factor(
    what_value,
    levels = c("diff_grey", "diff_green", "diff_red", "value_bar")),
    category = factor(category, levels = c("Coal", "Oil", "Gas", "Nuclear", "Renewables\n(incl. hydro)"))
    )

labels_x_sales <- c("2019 Q4", "2020 Q4")

p_p <- plot_summary_col(
  data_summ_p,
  labels.x = labels_x_sales)
p_p <- p_p +
  scale_y_continuous(
      expand = expansion(mult = c(0, 0.3))
    ) +
  labs(
    title = "Power",
    subtitle = "(GW)"
  )
p_p

val_divisor <- 10 ^ 3
data_summ_a <- readxl::read_excel(
    path = paste0(data_path, data_file),
    sheet = "Data",
    range = "K13:M15",
    col_names = FALSE
  ) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  rename(
    category = 1,
    before = 2,
    after = 3,
  ) %>%
  pivot_longer(
    cols = c("before", "after"),
    names_to = "value_type"
  ) %>%
  mutate(
    indicator = "production",
    value_type = factor(value_type, levels = c("before", "after")),
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
    category = factor(category, levels = c("ICE", "Hybrid", "Electric"))
    )

labels_x_sales <- c("2019 Q4", "2020 Q4")

p_a <- plot_summary_col(
  data_summ_a,
  labels.x = labels_x_sales)
p_a <- p_a +
  scale_y_continuous(
      expand = expansion(mult = c(0, 0.3))
    ) +
  labs(
    title = "Automotive",
    subtitle = "(Thousands vehicles)"
  )
p_a
