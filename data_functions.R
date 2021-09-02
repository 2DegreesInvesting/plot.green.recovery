transform_fin_data <- function(data,
                               car_companies,
                               companies_average,
                               companies_leaders = c(),
                               companies_laggards = c()) {
data <- data %>%
  mutate(
    "Waverers" = rowMeans(data[companies_average],na.rm = TRUE),
    "Leaders" = rowMeans(data[companies_leaders],na.rm = TRUE),
    "Laggards" = rowMeans(data[companies_laggards],na.rm = TRUE)
    ) %>%
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
  rename(type = company)
}
