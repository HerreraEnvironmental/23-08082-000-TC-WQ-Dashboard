## Temp comparison script
library(tidyverse)

wqp_data <- read.csv("wqp_data.csv") |>
  mutate(date_col = as_date(date_time))
streams_wq_dat <- readRDS("outputs/streams_wq_dat.RDS") |>
  mutate(date_col = as_date(2))

t_wqp <- wqp_data %>%
  select(parameter, date_col) %>%
  group_by(parameter) %>%
  summarise(
    n        = n(),
    earliest = min(date_col, na.rm = TRUE),
    latest   = max(date_col, na.rm = TRUE),
    .groups  = "drop"
  )

t_streams <- streams_wq_dat %>%
  select(parameter, date_col) %>%
  group_by(parameter) %>%
  summarise(
    n        = n(),
    earliest = min(date_col, na.rm = TRUE),
    latest   = max(date_col, na.rm = TRUE),
    .groups  = "drop"
  )

diffs <- t_wqp %>%
  full_join(t_streams, by = "parameter", suffix = c("_t_wqp", "_t_streams")) %>%
  filter(
    is.na(n_t_wqp)        | is.na(n_t_streams)        | n_t_wqp        != n_t_streams |
      is.na(earliest_t_wqp) | is.na(earliest_t_streams) | earliest_t_wqp != earliest_t_streams |
      is.na(latest_t_wqp)   | is.na(latest_t_streams)   | latest_t_wqp   != latest_t_streams
  ) %>%
  arrange(parameter)

diffs
