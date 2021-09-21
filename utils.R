library(showtext)

font_add_google("EB Garamond")

showtext_auto()

annotate_pandemic <- function(p, height = 0.9) {
  pandemic_date <- p$data %>%
    filter(between(timestamp, as.Date("2020-03-15"), as.Date("2020-04-15"))) %>%
    pull(timestamp) %>%
    unique()

  p +
  annotate(
    "segment",
    x = pandemic_date,
    xend = pandemic_date,
    y = 0,
    yend = (height - 0.05) * max(p$data$value, na.rm = TRUE),
    colour = "grey",
    linetype = "dashed"
    ) +
  annotate(
    "text",
    x = pandemic_date,
    y = height * max(p$data$value, na.rm = TRUE),
    label = "Pandemic affects\n the EU",
    size = 6,
    family = "EB Garamond"
    )
}

scale_dates_quarters <- function(p, mark.end.month = FALSE) {
  if (mark.end.month) {
    p <- p +
    scale_x_date(
      breaks = as.Date(c("2018-01-31", "2018-07-31", "2019-01-31", "2019-07-31", "2020-01-31", "2020-07-31"), format = "%Y-%m-%d"),
      date_labels = as.yearqtr)
  } else {
    p <- p +
    scale_x_date(
      breaks = as.Date(c("2018-01-01", "2018-07-01", "2019-01-01", "2019-07-01", "2020-01-01", "2020-07-01"), format = "%Y-%m-%d"),
      date_labels = as.yearqtr)
  }
  p
}

darker.col = function(color, how.much = 30){
  colorRampPalette(c(color, "black"))(100)[how.much]
}

colour_difference_fin <- function(data) {
  if (data$what_value == "diff") {
    if (data$category %in% c("Laggards", "Oil & gas companies")) {
      if (data$indicator %in% c("returns", "pe", "dummy")) {
        if (data$is_diff_positive) {
          return("diff_red")
        } else {
          return("diff_green")
        }
      } else if (data$indicator == "wacc") {
        if (data$is_diff_positive) {
          return("diff_green")
        } else {
          return("diff_red")
        }
      }
    } else if (data$category == "Leaders") {
      if (data$indicator %in% c("returns", "pe", "dummy")) {
        if (data$is_diff_positive) {
          return("diff_green")
        } else {
          return("diff_red")
        }
      } else if (data$indicator == "wacc") {
        if (data$is_diff_positive) {
          return("diff_red")
        } else {
          return("diff_green")
        }
      }
    } else {
      return("diff_grey")
    }
  } else {
    return(data$what_value)
  }
}

colour_difference_econ <- function(data) {
  if (data$what_value == "diff") {
    if (data$category %in% c("ICE vehicles", "Coal", "Gas", "Other fuels",
                             "Total oil products", "Road fuels", "Aviation fuels",
                             "Int. marine fuels", "Natural gas", "Oil", "ICE")) {
      if (data$indicator %in% c("sales", "production")) {
        if (data$is_diff_positive) {
          return("diff_red")
        } else {
          return("diff_green")
        }
      }
    } else if (data$category %in% c("Electric vehicles", "Hybrid vehicles",
                                    "Electric", "Hybrid", "Nuclear", "Renewables",
                                    "Renewables (incl. hydro)")) {
      if (data$indicator == "sales") {
        if (data$is_diff_positive) {
          return("diff_green")
        } else {
          return("diff_red")
        }
      } else if (data$indicator == "production") {
        if (data$is_diff_positive) {
          return("diff_green")
        } else {
          return("diff_grey")
        }
      }
    } else {
      return("diff_grey")
    }
  } else {
    return(data$what_value)
  }
}

colour_difference_st <- function(data) {
  if (data$what_value == "diff") {
    if (data$category %in% c("ICE", "Sector", "CoalCap", "GasCap", "OilCap", "Oil", "Gas")) {
        if (data$is_diff_positive) {
          return("diff_green")
        } else {
          return("diff_red")
        }
    } else if (data$category %in% c("Electric", "Hybrid", "HydroCap", "NuclearCap", "RenewablesCap")) {
        if (data$is_diff_positive) {
          return("diff_red")
        } else {
          return("diff_green")
        }
    } else {
      return("diff_grey")
    }
  } else {
    return(data$what_value)
  }
}

get_data_xlx_stress_test <- function(path, sector, range, year) {
  data <- readxl::read_excel(
    path = path,
    sheet = sector,
    range = range,
    col_names = FALSE
  )
  colnames(data) <- c("technology", "exposure", "stressed_val")

  data <- data %>%
    mutate(
      year = as.character(year),
      category = if_else(technology == "Sector", "Sector", "technology"),
      exposure = exposure / value_div,
      stressed_val = stressed_val / value_div,
      val_diff = exposure - stressed_val,
      is_positive_diff = val_diff >= 0,
      start_val = if_else(
        is_positive_diff,
        stressed_val,
        exposure
      ),
      perc_diff = round((val_diff / exposure) * 100, digits = 0),
      val_diff = abs(val_diff)) %>%
    select(-c(exposure)) %>%
    tidyr::pivot_longer(
      cols = c("start_val", "val_diff"),
      names_to = "value_type"
    ) %>%
    mutate(
      colour_var = case_when(
        value_type == "start_val" ~ "start",
        !is_positive_diff ~ "increase",
        TRUE ~ "decrease"
      )
    ) %>%
    mutate(
      colour_var = factor(colour_var, levels = c("increase", "decrease", "start"))
    ) %>%
    arrange(technology, colour_var)

  data
}
