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

car_companies <- c("Renault", "BMW", "Daimler", "Volkswagen", "Ferrari", "Porsche", "Volvo")
companies_average <- c("Renault", "BMW", "Daimler", "Porsche")
companies_leaders <- c("Volkswagen", "Volvo")
companies_laggards <- c("Ferrari")

data_ret <- readxl::read_excel(
  path = paste0(data_path, data_file),
  sheet = "Automotive",
  range = "AB7:AM43",
  col_names = TRUE) %>%
  as.data.frame() %>%
  select(-2) %>%
  rename(month = 1) %>%
  pivot_longer(
    cols = c(car_companies, "Waverers", "Leaders", "Laggards"),
    names_to = "company"
  ) %>%
  mutate(
    important = case_when(
      company %in% c("Laggards", "Waverers", "Leaders") ~ "important",
      company %in% c(companies_average, companies_leaders, companies_laggards) ~ "not important"
    )
  ) %>%
  rename(type = company) %>%
  mutate(
    timestamp = as.Date(month),
    value = value * 100) %>%
  filter(
    timestamp < as.Date("2021-01-01")
  )

data_file <- "PE ratio ind. 4.xlsx"
data_pe <- data <- readxl::read_excel(
  path = paste0(data_path, data_file),
  sheet = "Automotive",
  range = "B5:I59",
  col_names = TRUE) %>%
  as.data.frame() %>%
  rename(month = 1) %>%
  drop_na(month) %>%
  transform_fin_data(car_companies,
                     companies_average,
                     companies_leaders,
                     companies_laggards
                     ) %>%
  mutate(timestamp = as.Date(month)) %>%
  filter(
    timestamp >= as.Date("2018-01-01"),
    timestamp < as.Date("2021-01-01")
    )

data_file <- "WACC ind. 5.xlsx"
data_wacc <- readxl::read_excel(
  path = paste0(data_path, data_file),
  sheet = "Auto",
  range = "A5:H25",
  col_names = TRUE,
  col_types = c("guess", rep("numeric", length(car_companies)))) %>%
  as.data.frame() %>%
  rename(quarter = 1) %>%
  drop_na(quarter) %>%
  transform_fin_data(car_companies,
                     companies_average,
                     companies_leaders,
                     companies_laggards
                     ) %>%
  mutate(timestamp = as.Date(as.yearqtr(.data$quarter, format = "Q%q %Y"))) %>%
  filter(
    timestamp >= as.Date("2018-01-01"),
    timestamp < as.Date("2021-01-01")
    )

type_levels <- rev(c("Waverers", "Leaders", "Laggards", car_companies))
colour_labels <- rev(c(c("black", "blue", "red"), rep("grey", length(car_companies))))
size_values <- rev(c(rep(0.75, 3), rep(0.25, length(car_companies))))

p_ret <- plot_timeseries_fin_pandemic(
  data_ret,
  type_levels,
  colour_labels,
  size_values,
  mark.end.month = TRUE) +
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
ggsave(
  filename="charts/auto_returns.png",
  width=22,
  height=15,
  units="cm",
  dpi=350)

p_pe <- plot_timeseries_fin_pandemic(
  data_pe,
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
ggsave(
  filename="charts/auto_pe.png",
  width=22,
  height=15,
  units="cm",
  dpi=350)

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
ggsave(
  filename="charts/auto_wacc.png",
  width=22,
  height=15,
  units="cm",
  dpi=350)
