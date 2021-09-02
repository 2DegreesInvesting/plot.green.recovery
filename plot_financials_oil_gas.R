library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(r2dii.plot)
library(patchwork)
source("one_in1000_colour_palette.R")
source("utils.R")
source("plot_timeseries.R")
source("data_functions.R")
data_path <- "/Users/monikafurdyna/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/1in1000/03_Content/01_Long-term team/05_Green recovery/Data/Financial indicators/For Monika/"
data_file <- "Stock returns ind. 3.xlsx"

companies <-
  c("Royal Dutch Shell", "ENI", "Repsol", "OMV", "PKNorlen", "Lundin Energy", "Galp Energia")
companies_average <- companies

data_ret <- readxl::read_excel(
  path = paste0(data_path, data_file),
  sheet = "Oil & gas",
  range = "AC13:AM50",
  col_names = TRUE) %>%
  as.data.frame() %>%
  select(-2) %>%
  rename(month = 1) %>%
  drop_na(month) %>%
  select(-Total) %>%
  pivot_longer(
    cols = c(companies, "Average"),
    names_to = "company"
  ) %>%
  mutate(
    important = case_when(
      company == "Average" ~ "important",
      TRUE ~ "not important"
    )
  ) %>%
  rename(type = company) %>%
  mutate(timestamp = as.Date(month))

data_file <- "PE ratio ind. 4.xlsx"
data_pe <- data <- readxl::read_excel(
  path = paste0(data_path, data_file),
  sheet = "Oil & gas",
  range = "A10:I57",
  col_names = TRUE) %>%
  as.data.frame() %>%
  rename(month = 1)

colnames(data_pe) <- c("month", "total", companies)

data_pe <- data_pe %>%
  select(-total) %>%
  transform_fin_data(companies,
                     companies_average
                     ) %>%
  filter(
    !(type %in% c("Leaders", "Laggards"))
  ) %>%
  mutate(
    type = case_when(
      type == "Waverers" ~ "Average",
      TRUE ~ type
    )
  ) %>%
  group_by(month) %>%
  mutate(
    timestamp = as.Date(month)
    ) %>%
  filter(
    timestamp >= as.Date("2018-01-01"),
    timestamp < as.Date("2021-01-01")
    )

data_file <- "WACC ind. 5.xlsx"
data_wacc <- readxl::read_excel(
  path = paste0(data_path, data_file),
  sheet = "Oil & gas",
  range = "A9:I27",
  col_names = TRUE,
  col_types = c("guess", rep("numeric", length(companies) + 1))) %>%
  as.data.frame() %>%
  rename(quarter = 1) %>%
  drop_na(quarter) %>%
  select(-Total) %>%
  transform_fin_data(companies,
                     companies_average
                     ) %>%
  filter(
    !(type %in% c("Leaders", "Laggards"))
  ) %>%
  mutate(
    type = case_when(
      type == "Waverers" ~ "Average",
      TRUE ~ type
    )
  ) %>%
  mutate(timestamp = as.Date(as.yearqtr(.data$quarter, format = "Q%q %Y"))) %>%
  filter(
    timestamp >= as.Date("2018-01-01"),
    timestamp < as.Date("2021-01-01"))

type_levels <- rev(c("Average", companies))
colour_labels <- rev(c("black", rep("grey", length(companies))))
size_values <- rev(c(0.75, rep(0.25, length(companies))))

p_ret <- plot_timeseries_fin_pandemic(
  data_ret,
  type_levels,
  colour_labels,
  size_values) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, NA),
    n.breaks = 8
  ) +
  labs(
    caption = "The grey lines show the underlying companies' results.",
    y = "Percentage return\n (Basis 100 in 2018 Q1)",
    x = ""
  )
p_ret

data_pe_cut <- data_pe %>%
  ungroup() %>%
  drop_na(value) %>%
  mutate(
    value_remove = if_else(
      value > 320,
      TRUE,
      FALSE
    )
  ) %>%
  filter(!value_remove) %>%
  select(-value_remove)

p_pe <- plot_timeseries_fin_pandemic(
  data_pe_cut,
  type_levels,
  colour_labels,
  size_values) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, NA),
    n.breaks = 6
  ) +
  labs(
    caption = "The grey lines show the underlying companies' results.",
    y = "PE ratio",
    x = ""
  )
p_pe

p_wacc <- plot_timeseries_fin_pandemic(
  data_wacc,
  type_levels,
  colour_labels,
  size_values) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, NA),
    n.breaks = 8
  ) +
  labs(
    caption = "The grey lines show the underlying companies' results.",
    y = "WACC",
    x = ""
  )
p_wacc
