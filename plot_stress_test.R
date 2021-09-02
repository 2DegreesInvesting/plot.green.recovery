library(dplyr)
library(ggplot2)
library(patchwork)

library(showtext)
font_add_google("EB Garamond")
showtext_auto()
source("theme_green_recovery.R")
source("one_in1000_value_diff_palette.R")
source("utils.R")

data_path <- "/Users/monikafurdyna/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/1in1000/03_Content/01_Long-term team/05_Green recovery/Data/Financial indicators/For Monika/"
data_file <- "Potential transition financial losses ind. 6.xlsx"

value_div <- 10^9

find_colour_labels <- function(data) {
  labels_all <- c("darker_grey", "grey", "dark_grey")
  colour_vals_all <- c("increase", "decrease", "start")

  if (length(setdiff(colour_vals_all, as.character(unique(data$colour_var)))) == 0) {
    return(labels_all)
  } else if (setdiff(colour_vals_all, as.character(unique(data$colour_var))) == "decrease") {
    return(c("darker_grey", "dark_grey"))
  } else if (setdiff(colour_vals_all, as.character(unique(data$colour_var))) == "increase") {
    return(c("grey", "dark_grey"))
  }
}

plot_stress_test <- function(data, y.axis.position = "left") {
  p <- ggplot2::ggplot(
    data = data,
    aes(
      x = year,
      y = value,
      fill = colour_var
    )) +
    geom_bar(
      stat = "identity",
      position = "stack") +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.1)),
      position = y.axis.position
    ) +
    scale_fill_one_in1000_val_diff(labels = find_colour_labels(data)) +
    geom_text(
      data = data %>%
        filter(
            value_type == "start_val"
          ) %>%
        mutate(
          label = paste0(if_else(!is_positive_diff, "+", ""), -perc_diff, "%")
        ),
        aes(
          label = label,
          x = year,
          y = stressed_val
        ),
        vjust = - 0.6,
        family = "EB Garamond",
        size = 6
      ) +
    theme_green_recovery() +
    theme(legend.position = "none") +
    facet_wrap(~technology)
  p
}

######### Automotive ###########

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

data_auto_st <- rbind(data_auto_st_2019, data_auto_st_2020)


data_sec <- data_auto_st %>%
    filter(category == "Sector")
p_sec <- plot_stress_test(data_sec) +
  theme(axis.title.x = element_blank()) +
    labs(
      y = "Exposure (Billion \u20AC)"
    )
p_sec

data_tech <- data_auto_st %>%
    filter(category == "technology")
p_tech <- plot_stress_test(data_tech, y.axis.position = "right") +
    theme(axis.title = element_blank()) +
  labs(subtitle = "Technology")
p_tech

p_sec + p_tech +
  plot_layout(ncol = 2, widths = c(0.75, 2))

########### Power #############

data_power_st_2019 <- get_data_xlx_stress_test(
  paste0(data_path, data_file),
  "Power",
  "A8:C14",
  2019)

data_power_st_2020 <- get_data_xlx_stress_test(
  paste0(data_path, data_file),
  "Power",
  "E8:G14",
  2020)

data_power_st <- rbind(data_power_st_2019, data_power_st_2020) %>%
  mutate(
    technology = stringr::str_replace(technology, "Cap", "\nPower")
  )


data_sec <- data_power_st %>%
    filter(category == "Sector")
p_sec <- plot_stress_test(data_sec) +
  theme(axis.title.x = element_blank()) +
    labs(
      y = "Exposure (Billion \u20AC)"
    )
p_sec

data_tech <- data_power_st %>%
    filter(category == "technology")
p_tech <- plot_stress_test(data_tech, y.axis.position = "right") +
    theme(axis.title = element_blank()) +
  scale_y_continuous(
      expand = expansion(mult = c(0, 0.21)),
      position = "left"
    ) +
  labs(subtitle = "Technology")
p_tech

p_sec + p_tech +
  plot_layout(ncol = 2, widths = c(0.75, 2))

######### Oil & Gas #############

data_og_st_2019 <- get_data_xlx_stress_test(
  paste0(data_path, data_file),
  "Oil & gas",
  "A14:C16",
  2019)

data_og_st_2020 <- get_data_xlx_stress_test(
  paste0(data_path, data_file),
  "Oil & gas",
  "D14:F16",
  2020)

data_og_st <- rbind(data_og_st_2019, data_og_st_2020)

data_sec <- data_og_st %>%
    filter(category == "Sector")
p_sec <- plot_stress_test(data_sec) +
  theme(axis.title.x = element_blank()) +
    labs(
      y = "Exposure (Billion \u20AC)"
    )
p_sec

data_tech <- data_og_st %>%
    filter(category == "technology")
p_tech <- plot_stress_test(data_tech, y.axis.position = "right") +
    theme(axis.title = element_blank()) +
  labs(subtitle = "Technology")
p_tech

p_sec + p_tech +
  plot_layout(ncol = 2, widths = c(0.75, 2))
