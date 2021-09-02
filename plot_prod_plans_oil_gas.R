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
data_path <- "/Users/monikafurdyna/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/1in1000/03_Content/01_Long-term team/05_Green recovery/Data/Indicator 2 - Production plans/"
data_file <- "Production plans data for Monika.xlsx"

val_divisor <- 10^3

data <- readxl::read_excel(
  path = paste0(data_path, data_file),
  sheet = "Oil & gas",
  range = "I3:M26",
  col_names = FALSE) %>%
  as.data.frame()

colnames(data) <- c("sector", "technology", "asset_level_timestamp", "year", "value")

data <- data %>%
  mutate(
    time = as.Date(as.yearmon(year)),
    value = value / val_divisor,
    asset_level_timestamp = case_when(
      asset_level_timestamp == "2019Q4" ~ "2019",
      asset_level_timestamp == "2020Q4" ~ "2020"
    )
    ) %>%
  filter(!(year %in% c(2019, 2025))) %>%
  group_by(sector, technology, year, asset_level_timestamp, time) %>%
  summarise(value = sum(value)) %>%
  group_by(year, asset_level_timestamp) %>%
  mutate(perc_share = value / sum(value))

p1 <- ggplot(
  data %>%
    filter(technology == "Gas"),
       aes(
         x = year,
         y = value / 10^6,
        colour = asset_level_timestamp
          )) +
  geom_line() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, NA),
    n.breaks = 6
  ) +
  scale_colour_one_in1000(labels = c("black", "grey"), name = "Gas\n(as of end of year)") +
  theme_green_recovery() %+replace%
  theme(
    legend.title = element_text(size = 16)
  ) +
  labs(
    y = "Billion m3",
    x = ""
  )
p1

p2 <- ggplot(
  data %>%
    filter(technology == "Oil"),
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
  scale_colour_one_in1000(
    labels = c("black", "grey"),
    name = "Oil\n(as of end of year)") +
  theme_green_recovery() %+replace%
  theme(
    legend.title = element_text(size = 16)
  ) +
  labs(
    y = "Billion barrels",
    x = ""
  )
p2
