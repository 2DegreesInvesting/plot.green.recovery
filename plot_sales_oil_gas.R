library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(r2dii.plot)
library(patchwork)
library(tidytext)
library(stringr)
library(ggnewscale)
source("one_in1000_colour_palette.R")
source("r2dii_tech_colours.R")
source("theme_green_recovery.R")
source("utils.R")
data_path <- "/Users/monikafurdyna/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/1in1000/03_Content/01_Long-term team/05_Green recovery/Data/Indicator 1 - Sales/For Monika/"
data_file <- "Indicator 1 - Sales.xlsx"

data_gas <- readxl::read_excel(
  path = paste0(data_path, data_file),
  sheet = "Natural gas",
  range = "A2:Q3",
  col_names = FALSE) %>%
  t() %>%
  as.data.frame()

colnames(data_gas) <- c("time", "value")

val_divisor <- 10 ^ 3

data_gas <- data_gas %>%
  drop_na() %>%
  mutate(
    technology = "Gas"
  ) %>%
  mutate(
    value = as.numeric(value) / val_divisor,
    chunks = stringr::str_split(time, stringr::fixed(" "), n=2)
    ) %>%
  mutate(
    quarter = purrr::map_chr(chunks, 2),
    year = purrr::map_chr(chunks, 1),
    timestamp = as.Date(as.yearqtr(.data$time))
  ) %>%
  select(-chunks) %>%
  filter(
    year >= 2018,
    )

data_annotation <- data_gas %>%
  filter(timestamp == max(data_gas$timestamp))
value_span <- max(data_gas$value, na.rm = TRUE) - min(data_gas$value, na.rm = TRUE)

p1 <- ggplot(data_gas,
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
    limits = c(0, NA),
    n.breaks = 6
  ) +
  scale_colour_one_in1000(labels = c("black")) +
  theme_green_recovery() +
  labs(
    y = "Billion m3",
    x = ""
  ) +
  theme(
    legend.position = "none"
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

data_oil_total <- readxl::read_excel(
  path = paste0(data_path, data_file),
  sheet = "Oil",
  range = "A2:Q3",
  col_names = FALSE) %>%
  t() %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  replace_na(list("V1" = "time"))

colnames(data_oil_total) <- c("time", "value")
data_oil_total = data_oil_total[-1,]

val_divisor <- 10 ^ 3

data_oil_total <- data_oil_total %>%
  mutate(
    technology = "Oil",
    value = as.numeric(value) / val_divisor,
    chunks = stringr::str_split(time, fixed(" "), n = 2)
    ) %>%
  mutate(
    quarter = purrr::map_chr(chunks, 2),
    year = purrr::map_chr(chunks, 1),
    timestamp = as.Date(as.yearqtr(.data$time))
  ) %>%
  select(-chunks) %>%
  filter(
    year >= 2018,
    year < 2021)

data_annotation <- data_oil_total %>%
  filter(timestamp == max(data_oil_total$timestamp))
value_span <- max(data_oil_total$value, na.rm = TRUE) - min(data_oil_total$value, na.rm = TRUE)

p2 <- ggplot(data_oil_total,
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
    limits = c(0, NA),
    n.breaks = 6
  ) +
  scale_colour_one_in1000(labels = c("black")) +
  theme_green_recovery() +
  labs(
    y = "Million Tonnes",
    x = ""
  ) +
  theme(
    legend.position = "none"
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

data_oil <- readxl::read_excel(
  path = paste0(data_path, data_file),
  sheet = "Oil",
  range = "A7:Q10",
  col_names = FALSE) %>%
  t() %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  replace_na(list("V1" = "time"))

colnames(data_oil) <- data_oil[1,]
data_oil = data_oil[-1,]

val_divisor <- 10 ^ 3

data_oil <- data_oil %>%
  pivot_longer(
    cols = colnames(data_oil)[-1],
    names_to = "technology"
  ) %>%
  mutate(
    value = as.numeric(value) / val_divisor,
    chunks = stringr::str_split(time, fixed(" "), n = 2)
    ) %>%
  mutate(
    quarter = purrr::map_chr(chunks, 2),
    year = purrr::map_chr(chunks, 1),
    timestamp = as.Date(as.yearqtr(.data$time))
  ) %>%
  select(-chunks) %>%
  filter(
    year >= 2018,
    year < 2021
    )

data_bars <- data_oil %>%
  group_by(time, quarter, year, technology) %>%
  summarise(value = sum(value)) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(time, quarter, year) %>%
  mutate(
    highlight = case_when(
      time %in% c("2020 Q2", "2020 Q3", "2020 Q4", "2021 Q1") ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  group_by(quarter) %>%
  mutate(
    id = cur_group_id()
    )

p3 <- ggplot(
  data_bars %>%
    filter(technology != "Int. marine fuels"),
    aes(
      x = technology,
      y = value)
    ) +
  scale_x_reordered() +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = highlight),
    alpha = 0.2,
    show.legend = FALSE)  +
  scale_fill_manual(values = c("#E9E4E3", "grey")) +
  new_scale_fill() +
  geom_bar(
    stat = "identity",
    width = 0.6,
    aes(fill = technology)
  ) +
  scale_fill_r2dii_tech(sector = "fossil fuels", r2dii_labels = c("oil","gas")) +
  facet_grid(year ~ quarter, switch = "x") +
  theme_green_recovery() +
  theme(
    strip.placement = "outside",
    legend.position = "bottom"
    ) +
  labs(
    y = "Million Tonnes",
    x = element_blank()
  )
p3

p4 <- ggplot(
  data_bars %>%
    filter(technology == "Int. marine fuels"),
    aes(
      x = quarter,
      y = value,
      fill = technology)
    ) +
  scale_x_discrete() +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  geom_rect(
    data = data_bars %>%
      filter(
        technology == "Int. marine fuels",
        highlight == "Yes"),
    aes(xmin = min(id) - 0.5, xmax = max(id) + 0.5, ymin = -Inf, ymax = Inf),
    alpha = 0.2,
    fill = "grey",
    show.legend = FALSE)  +
  geom_bar(
    stat = "identity",
    width = 0.6
  ) +
  scale_fill_r2dii_tech(sector = "fossil fuels", r2dii_labels = c("coal")) +
  facet_wrap(~ year, nrow = 3) +
  theme_green_recovery() +
  labs(
    caption = "The greyed areas indicate quarters after the start of the pandemic.",
    y = "Million Tonnes",
    x = element_blank()
  ) +
  theme(legend.position = "bottom")
p4


patchwork <- p3 + p4 +
  plot_layout(ncol = 2, widths = c(2, 0.75))
patchwork

