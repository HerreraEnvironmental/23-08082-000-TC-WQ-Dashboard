# the scores appear to funcitoning as intended
# TC 10/23/2025
# AH - the Puget Sound streams argument was flipped!
library(dplyr)
library(arrow)
library(lubridate)
streams_wq_dat <- read_parquet("outputs/streams_wq_dat.parquet")

wqx_site_names <- unique(streams_wq_dat$SITE_CODE)

## Import external stream use designations
stream_use_designations <- readxl::read_xlsx(
  "inputs/Stream Use Designations Herrera.xlsx"
) |>
  transmute(
    SITE_CODE = `Site Code`,
    AquaticLifeUse = case_when(
      grepl("Spawn", `ALU (Temp. °C)`) ~
        "Salmonid Spawning, Rearing, and Migration",
      grepl("Core", `ALU (Temp. °C)`) ~ "Core Summer Salmonid Habitat",
      grepl("13", `ALU (Temp. °C)`) ~ "Marine",
      T ~ "ERROR"
    )
  ) |>
  filter(!is.na(SITE_CODE)) |>
  rowwise() |>
  mutate(SITE_CODE = wqx_site_names[which(grepl(SITE_CODE, wqx_site_names))[1]]) |>
  ungroup()


parm_table <- data.frame(rbind(
  c("FC", "Fecal Coliform"),
  c("FC", "E. coli"),
  c("FC", "Fecal Bacteria"),
  c("Oxygen", "Dissolved Oxygen"),
  c("pH", "pH"),
  c("TP_P", "Total Phosphorus"),
  c("SUSSOL", "Total Suspended Solids"),
  c("Temp", "Temperature, water"),
  c("TPN", "Total Nitrogen"),
  c("TPN", "Nitrate + Nitrite"),
  c("Turb", "Turbidity")
))
colnames(parm_table) <- c("shortParmName", "parameter")


parms_to_log <- c("FC", "SUSSOL", "TP_P", "Turb")
small_PS_stream <- T

ecology_curves <- read.csv("inputs/wqi_ecology_curves_v6.csv")

wqi_prep_test <- streams_wq_dat %>%
  mutate(Date = as_date(DateTime)) %>%
  filter(grepl("NISMC0000", SITE_CODE) & WaterYear == 2023) |>
  left_join(stream_use_designations, by = "SITE_CODE") |>
  left_join(parm_table, by = "parameter") |>
  filter(!is.na(shortParmName)) %>%
  arrange(SITE_CODE, Date, shortParmName) %>%
  mutate(
    TemperatureCode = ifelse(
      AquaticLifeUse == "Core Summer Salmonid Habitat",
      8,
      ifelse(
        AquaticLifeUse == "Salmonid Spawning, Rearing, and Migration",
        9,
        NA
      )
    ),
    OxygenCode = ifelse(
      AquaticLifeUse == "Core Summer Salmonid Habitat",
      26,
      ifelse(
        AquaticLifeUse == "Salmonid Spawning, Rearing, and Migration",
        21,
        NA
      )
    ),
    Year = year(Date),
    Month = month(Date),
    WaterYear = ifelse(Month >= 10, Year + 1, Year),
    MonthFraction = Month + day(Date) / days_in_month(Month)
  ) %>%
  # Average multiple samples within same month
  group_by(
    SITE_CODE, shortParmName, WaterYear, Month, MonthFraction,
    TemperatureCode, OxygenCode
  ) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    WQI_Value = ifelse(shortParmName %in% parms_to_log, log(value), value),
    rownumber = case_when(
      shortParmName == "Temp" ~ TemperatureCode,
      shortParmName == "Oxygen" ~ OxygenCode,
      shortParmName == "pH" ~ 41,
      shortParmName == "FC" ~ 51,
      shortParmName == "TPN" ~ ifelse(small_PS_stream, 262, 62),
      shortParmName == "TP_P" ~ ifelse(small_PS_stream, 272, 72),
      shortParmName == "SUSSOL" ~ 82,
      shortParmName == "Turb" ~ 92,
      TRUE ~ NA_real_
    )
  ) %>%
  left_join(ecology_curves,
    by = c("rownumber" = "ParamClassID"),
    relationship = "many-to-many"
  ) %>%
  filter(
    !is.na(LowerResult),
    value >= LowerResult,
    value <= UpperResult,
    MonthFraction >= Start.Month,
    MonthFraction < End.Month
  ) %>%
  mutate(
    WQI = a + b * WQI_Value + b2 * WQI_Value^2,
    WQI = pmin(WQI, 100),
    WQI = pmax(WQI, 1)
  ) %>%
  select(SITE_CODE, shortParmName, WaterYear, Month, value, WQI_Value, WQI) %>%
  group_by(SITE_CODE, shortParmName, WaterYear, Month) %>%
  summarise(
    value = mean(value, na.rm = TRUE),
    WQI_Value = mean(WQI_Value, na.rm = TRUE),
    WQI = mean(WQI, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Month = factor(Month, levels = c(10:12, 1:9))) %>%
  arrange(SITE_CODE, shortParmName, WaterYear, Month) %>%
  group_by(SITE_CODE, WaterYear) %>%
  mutate(nSamples = length(unique(Month))) %>%
  ungroup()

np_ratios_test <- wqi_prep_test %>%
  select(-WQI_Value, -WQI) %>%
  filter(shortParmName %in% c("TPN", "TP_P")) %>%
  group_by(SITE_CODE, WaterYear, Month, nSamples) %>%
  tidyr::pivot_wider(
    names_from = shortParmName,
    values_from = value,
    values_fn = mean
  ) %>%
  mutate(NPRatio = TPN / TP_P) %>%
  select(SITE_CODE, WaterYear, Month, NPRatio)

nutrient_score_test <- wqi_prep_test %>%
  filter(shortParmName %in% c("TPN", "TP_P")) %>%
  left_join(np_ratios_test, by = c("SITE_CODE", "WaterYear", "Month", "nSamples")) %>%
  group_by(SITE_CODE, WaterYear, Month, nSamples) %>%
  summarise(
    WQI = case_when(
      is.na(NPRatio) ~ min(WQI, na.rm = TRUE),
      NPRatio <= 10 ~ WQI[shortParmName == "TPN"],
      NPRatio > 20 ~ WQI[shortParmName == "TP_P"],
      TRUE ~ min(WQI, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  mutate(shortParmName = "Nutrient")
