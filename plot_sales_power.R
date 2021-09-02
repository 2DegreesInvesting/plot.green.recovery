library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(r2dii.plot)
library(patchwork)
library(tidytext)
source("one_in1000_colour_palette.R")
source("r2dii_tech_colours.R")
source("theme_green_recovery.R")
source("utils.R")
data_path <- "/Users/monikafurdyna/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/1in1000/03_Content/01_Long-term team/05_Green recovery/Data/Indicator 1 - Sales/For Monika/"
data_file <- "Indicator 1 - Sales.xlsx"

data <- readxl::read_excel(
  path = paste0(data_path, data_file),
  sheet = "Power",
  range = "B3:S11",
  col_names = FALSE) %>%
  t() %>%
  as.data.frame()

colnames(data) <- c("time", "Total", "High Carbon", "Coal", "Gas", "Other fuels", "Low Carbon", "Renewables", "Nuclear")

val_divisor <- 10 ^ 3

data <- data %>%
  drop_na() %>%
  pivot_longer(
    cols = c("Total", "High Carbon", "Coal", "Gas", "Other fuels", "Low Carbon", "Renewables", "Nuclear"),
    names_to = "technology"
  ) %>%
  mutate(value = as.integer(value) / val_divisor) %>%
  group_by(time) %>%
  mutate(
    quarter = strsplit(time, " ")[[1]][2],
    year = as.integer(strsplit(time, " ")[[1]][1]),
    timestamp = as.Date(as.yearqtr(.data$time))
  ) %>%
  filter(
    year >= 2018,
    year < 2021)

data_green_brown <- data %>%
  filter(
    technology %in% c("High Carbon", "Low Carbon")
  )

data_annotation <- data_green_brown %>%
  filter(timestamp == max(data_green_brown$timestamp))
value_span <- max(data$value, na.rm = TRUE) - min(data$value, na.rm = TRUE)

p2 <- ggplot(data_green_brown,
       aes(
         x = timestamp,
         y = value,
        colour = factor(technology, levels = c("High Carbon", "Low Carbon"))
       )) +
  geom_line() +
  scale_x_date(
    breaks = as.Date(c("2018-01-01", "2018-07-07", "2019-01-01", "2019-07-01", "2020-01-01", "2020-07-01")),
    date_labels = as.yearqtr) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, NA)
  ) +
  scale_colour_one_in1000(labels = c("red", "green")) +
  theme_green_recovery() +
  labs(
    y = "Thousands of GwH",
    x = ""
  ) +
  ggrepel::geom_text_repel(
      data = data_annotation,
      size = 6,
      aes(
        x = timestamp,
        y = value,
        label = technology
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
      plot.margin = unit(c(0.5, 3, 0.5, 0.5), "cm")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

p2 <- annotate_pandemic(p2, height = 1)
p2

data_total <- data %>%
  filter(technology == "Total")

data_annotation <- data_total %>%
  filter(timestamp == max(data_total$timestamp))
value_span <- max(data$value, na.rm = TRUE) - min(data$value, na.rm = TRUE)

p1 <- ggplot(data_total,
       aes(
         x = timestamp,
         y = value,
         colour = technology
       )) +
  geom_line() +
  scale_x_date(
    breaks = as.Date(c("2018-01-01", "2018-07-07", "2019-01-01", "2019-07-01", "2020-01-01", "2020-07-01")),
    date_labels = as.yearqtr) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, NA)
  ) +
  scale_colour_one_in1000(labels = c("black")) +
  theme_green_recovery() +
  labs(
    y = "Thousands of GwH",
    x = ""
  ) +
  ggrepel::geom_text_repel(
      data = data_annotation,
      size = 6,
      aes(
        x = timestamp,
        y = value,
        label = technology
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
      plot.margin = unit(c(0.5, 3, 0.5, 0.5), "cm")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

p1 <- annotate_pandemic(p1, height = 1)
p1

tech_order <- c("Coal", "Other fuels", "Gas", "Nuclear", "Renewables")

data_bars <- data %>%
  filter(technology %in% tech_order) %>%
  group_by(time, quarter, year, technology) %>%
  summarise(value = sum(value)) %>%
  group_by(time, quarter, year) %>%
  mutate(
    value = value / 100
  )

highlights <- data_bars %>%
  filter(
    time %in% c("2020 Q2", "2020 Q3", "2020 Q4")
  )

p3 <- ggplot(
  data_bars,
  aes(
    x = factor(technology, levels = rev(tech_order)),
    y = value,
    fill = factor(technology, levels = rev(tech_order))
  )) +
  geom_rect(
    data = highlights,
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill='grey',
    alpha=0.2) +
  geom_bar(
    stat = "identity",
    width = 0.6
  ) +
  scale_x_reordered() +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0)),
    position = "right"
  ) +
  scale_fill_r2dii_tech(sector = "power", r2dii_labels = rev(c("coalcap", "oilcap", "gascap", "nuclearcap", "renewablescap"))) +
  coord_flip() +
  theme_green_recovery() +
  guides(fill = guide_legend(reverse = T)) +
  facet_grid(year ~ quarter, switch = "both") +
  theme(
    strip.placement = "outside",
    legend.position = "bottom",
    panel.spacing.x = unit(2, "lines")
    ) +
  labs(
    caption = "The greyed areas indicates quarters after the start of the pandemic.",
    y = "Hundred thousands of GwH",
    x = element_blank()
  )
p3

