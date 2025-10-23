library(tidyverse)

## This is a small test script that compares the initial WQX (aka WQP)
## download to the data produced by the data_prep.R script.

# Comparison --------------------------------------------------------------

## This variable is the initial download called from WQP by WQP_r_script.R
wqp_data <- read.csv("inputs/wqp_data.csv") |>
  mutate(date_col = as_date(date_time))

## This variable is produced after the data_prep.R script.
streams_wq_dat <- readRDS("outputs/streams_wq_dat.RDS") |>
  mutate(date_col = as_date(DateTime))

## Test wqp data by record of parameter.
t_wqp <- wqp_data |>
  select(parameter, date_col) |>
  group_by(parameter) |>
  summarise(
    n = n(),
    startdate = min(date_col, na.rm = TRUE),
    enddate = max(date_col, na.rm = TRUE),
    .groups = "drop"
  )

## Test dashboard data by record of parameter
t_streams <- streams_wq_dat |>
  select(parameter, date_col) |>
  group_by(parameter) |>
  summarise(
    n = n(),
    startdate = min(date_col, na.rm = TRUE),
    enddate = max(date_col, na.rm = TRUE),
    .groups = "drop"
  )

## Create a difference table
diffs <- t_wqp |>
  full_join(t_streams, by = "parameter", suffix = c("_wqp", "_streams")) |>
  filter(
    is.na(n_wqp) |
      is.na(n_streams) |
      n_wqp != n_streams |
      is.na(startdate_wqp) |
      is.na(startdate_streams) |
      startdate_wqp != startdate_streams |
      is.na(enddate_wqp) |
      is.na(enddate_streams) |
      enddate_wqp != enddate_streams
  ) |>
  arrange(parameter)

diffs


number_check <- diffs %>%
  mutate(
    param_std = case_when(
      parameter %in% c("Dissolved oxygen (DO)") ~ "Dissolved Oxygen",
      parameter %in% c("Temperature") ~ "Temperature, water",
      parameter %in% c("Nitrate + Nitrite as N") ~ "Nitrite + Nitrate",
      parameter %in%
        c("Total Phosphorus, mixed forms", "Phosphorus", "Total Phosphorus") ~
        "Total Phosphorus",
      parameter %in% c("Escherichia coli") ~ "E. coli",
      parameter %in% c("Specific conductance", "Conductivity") ~ "Conductivity",
      parameter %in% c("Total suspended solids") ~ "Total Suspended Solids",
      TRUE ~ parameter
    )
  ) %>%
  group_by(param_std) %>%
  summarise(across(c(n_wqp, n_streams), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(equal = n_wqp == n_streams)
