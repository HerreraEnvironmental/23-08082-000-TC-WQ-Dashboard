library(tidyverse)
library(fuzzyjoin)

df_A <-read.csv('inputs/Herrera All Stream Data Dump 4 12 2023.csv') %>%
  tibble() %>%
  select(contains("SITE")) %>%
  unique() %>%
  arrange(SITE_NAME)



df_B <- read.csv("wqp_data.csv") %>%
  tibble() %>%
  select(contains("SITE")) %>%
  unique() %>%
  arrange(SITE_NAME)

t <- stringdist_join(df_A, df_B, 
                by = "name",
                mode = "left",
                ignore_case = FALSE, 
                method = "jw", 
                max_dist = 99, 
                distance_col = "dist") %>%
  group_by(name.x) %>%
  slice_min(order_by = dist, n = 1)


