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

val_divisor <- 1

data <- readxl::read_excel(
  path = paste0(data_path, data_file),
  sheet = "Power",
  range = "A3:I1408",
  col_names = TRUE) %>%
  as.data.frame() %>%
  drop_na(technology, asset_level_timestamp) %>%
  mutate(
    year = as.Date(as.yearmon(variable)),
    value = value / val_divisor,
    asset_level_timestamp = case_when(
      asset_level_timestamp == "2019Q4" ~ "2019",
      asset_level_timestamp == "2020Q4" ~ "2020"
    )
    ) %>%
  group_by(sector, technology, year, asset_level_timestamp, variable) %>%
  summarise(value = sum(value))

data_green_brown <- data %>%
  mutate(technology_type = case_when(
    technology %in% c("CoalCap", "GasCap", "OilCap") ~ "High Carbon",
    technology %in% c("HydroCap", "NuclearCap", "RenewablesCap") ~ "Low Carbon"
  )) %>%
  group_by(variable, technology_type, asset_level_timestamp, year) %>%
  summarise(value = sum(.data$value))

green <- one_in1000_palette() %>%
  filter(label == "green") %>%
  pull(hex)
red <- one_in1000_palette() %>%
  filter(label == "red") %>%
  pull(hex)
green_colours <- c(darker.col(green, how.much = 50), green)
red_colours <- c(darker.col(red, how.much = 50), red)

p2 <- ggplot(
  data_green_brown,
  aes(
    x = year,
    y = value
  )) +
  geom_line(
    data = data_green_brown %>% filter(technology_type == "High Carbon"),
    aes(colour = factor(asset_level_timestamp, levels = c("2019", "2020")))
  ) +
  scale_colour_manual("High Carbon\n(as of end of year)", values = red_colours) +
  new_scale_colour() +
  geom_line(
    data = data_green_brown %>% filter(technology_type == "Low Carbon"),
    aes(colour = factor(asset_level_timestamp, levels = c("2019", "2020")))
  ) +
  scale_colour_manual("Low Carbon  \n(as of end of year)", values = green_colours) + # space because legend order is guided by the number of characters
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, NA),
    labels = scales::comma,
    n.breaks = 6
  ) +
  theme_green_recovery() %+replace%
  theme(
    legend.title = element_text(size = 16)
  )

p2 +
  labs(
   # title = "Production planned in low and high-carbon category per year",
    y = "MW",
    x = ""
  )

data_total <- data_green_brown %>%
  group_by(year, asset_level_timestamp) %>%
  summarise(value = sum(.data$value)) %>%
  mutate(technology_type = "Total")

p1 <- ggplot(
  data_total,
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
  scale_colour_one_in1000(labels = c("black", "grey"), name = "Total\n(as of end of year)") +
  theme_green_recovery() %+replace%
  theme(
    legend.title = element_text(size = 16)
  ) +
  labs(
    y = "MW",
    x = ""
  )
p1

tech_order <- c("CoalCap", "OilCap", "GasCap", "NuclearCap", "RenewablesCap")
data_bars <- data %>%
  filter(!(variable %in% c(2019, 2025))) %>%
  mutate(
    technology = case_when(
      technology %in% c("HydroCap", "RenewablesCap") ~ "RenewablesCap",
      TRUE ~ technology
    )
  ) %>%
  group_by(sector, technology, year, asset_level_timestamp, variable) %>%
  summarise(value = sum(value)) %>%
  mutate(
    technology = factor(technology, levels = tech_order)
  )

p3 <- ggplot(
  data_bars,
  aes(
    x = factor(
      asset_level_timestamp,
      levels = c("2020", "2019")),
    y = value / 10^3,
    fill = factor(
      technology,
      levels = tech_order
    )
  )) +
  geom_bar(
    stat = "identity",
    width = 0.5
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    n.breaks = 3
  ) +
  scale_fill_r2dii_tech(
    sector = "power",
    r2dii_labels = c("coalcap", "oilcap", "gascap", "nuclearcap", "renewablescap")) +
  coord_flip() +
  theme_green_recovery() +
  facet_grid(technology ~ variable) +
  theme(strip.text.y = element_blank()) +
  labs(
    y = "GW",
    x = element_blank()
  ) +
  theme(
    legend.position = "bottom",
    axis.title.y = element_blank(),
    panel.spacing.x = unit(2, "lines")
    ) +
  guides(fill = guide_legend(nrow = 2))
p3
