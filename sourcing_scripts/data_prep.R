library(arrow)
library(tidyverse)

## This script uses data sourced from the WQP (Water Quality Portal Data Home)
source("sourcing_scripts/WQP_r_script.R")
source("helper_functions/wqi_function.R")

## Import the wqp data
wqp_data <- read_parquet("inputs/wqp_data.parquet") |>
  filter(parameter != "Depth to water from rim of well casing")

wqx_site_names<-unique(wqp_data$SITE_CODE)

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
  mutate(SITE_CODE=wqx_site_names[which(grepl(SITE_CODE,wqx_site_names))[1]]) |>
  ungroup()

## Create site use designations file
streams_sites <- wqp_data |>
  select(gid, SITE_CODE, SITE_NAME, Metro_ID, LAT, LON) |>
  distinct() |>
  arrange(SITE_NAME) |>
  left_join(stream_use_designations, by = "SITE_CODE") |>
  mutate(
    AquaticLifeUse = ifelse(
      is.na(AquaticLifeUse),
      "Core Summer Salmonid Habitat",
      AquaticLifeUse
    )
  )

write_parquet(streams_sites, "outputs/streams_sites.parquet")
#saveRDS(streams_sites, "outputs/streams_sites.RDS")
#write.csv(streams_sites, "outputs/streams_sites.csv", row.names = F)

## Create streams water quality data
streams_wq_dat <- wqp_data |>
  tibble() |>
  mutate(
    sample_utc_offset = if_else(dst(date_time), 7L, 8L),
    DateTime = with_tz(
      as_datetime(date_time, tz = "UTC") + hours(sample_utc_offset),
      tzone = "America/Los_Angeles"
    )
  ) |>
  select(
    SITE_CODE,
    DateTime,
    parameter,
    value,
    unit,
    depth_m,
    dup,
    mdl,
    pql,
    qualifier
  ) |>
  mutate(
    across(c(unit, qualifier), trimws),
    nonDetectFlag = grepl("U", qualifier, fixed = TRUE),
    newResultValue = if_else(nonDetectFlag, pql, value),
    newResultValue = if_else(
      parameter == "Turbidity" & newResultValue <= 0,
      0.01,
      newResultValue
    ),
    Year = year(DateTime),
    Month = month(DateTime),
    WaterYear = if_else(Month >= 10L, Year + 1L, Year),
    FakeDate = make_date(2000, Month, day(DateTime)),
    WY_FakeDate = as.Date(if_else(Month >= 10L, FakeDate - years(1), FakeDate)),
    parameter = dplyr::case_match(
      parameter,
      c("Dissolved oxygen (DO)") ~ "Dissolved Oxygen",
      c("Temperature") ~ "Temperature, water",
      c("Nitrate + Nitrite as N") ~ "Nitrite + Nitrate",
      c("Total Phosphorus, mixed forms", "Phosphorus", "Total Phosphorus") ~
        "Total Phosphorus",
      c("Escherichia coli") ~ "E. coli",
      c("Specific conductance", "Conductivity") ~ "Conductivity",
      c("Total suspended solids") ~ "Total Suspended Solids",
      .default = parameter
    )
  )

write_parquet(streams_wq_dat, "outputs/streams_wq_dat.parquet")
#saveRDS(streams_wq_dat, "outputs/streams_wq_dat.RDS")

## Create list of sites
sites_list <- setNames(
  streams_sites$SITE_CODE,
  paste0(streams_sites$SITE_NAME, " (", streams_sites$SITE_CODE, ")")
)

## Create list of parameters
parm_list <- unique(streams_wq_dat$parameter) |>
  factor(
    levels = c(
      "Temperature, water",
      "Dissolved Oxygen",
      "pH",
      "Conductivity",
      "Turbidity",
      "Total Phosphorus",
      "Nitrate + Nitrite",
      "Ammonia-nitrogen",
      "Total Suspended Solids",
      "Fecal Coliform",
      "E. coli",
      "Enterococcus",
      "Flow",
      "Copper",
      "Lead",
      "Zinc",
      "Hardness",
      "Petroleum hydrocarbons, total extractable"
    )
  ) |>
  levels()

## Create list of years
years_list <- sort(unique(streams_wq_dat$WaterYear), T)

saveRDS(sites_list, "outputs/sites_list.RDS")
saveRDS(parm_list, "outputs/parm_list.RDS")
saveRDS(years_list, "outputs/years_list.RDS")

unique(streams_wq_dat$depth_m) # all 0 or NA
unique(streams_wq_dat$dup) # there are dups
unique(streams_wq_dat$qualifier)

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


annual_wqi <- streams_wq_dat |>
  left_join(stream_use_designations, by = "SITE_CODE") |>
  left_join(parm_table, by = "parameter") |>
  filter(!is.na(shortParmName)) |>
  with(
    wqi_calc(
      site = SITE_CODE,
      value = newResultValue,
      shortParmName = shortParmName,
      date = as.Date(DateTime),
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
      small_PS_stream = T # assume all puget sound small streams
    )
  )
write_parquet(annual_wqi, "outputs/annual_wqi.parquet")
#saveRDS(annual_wqi, "outputs/annual_wqi.RDS")

annual_wqi_by_parameter <- streams_wq_dat |>
  left_join(streams_sites |> select(SITE_CODE, AquaticLifeUse), by = "SITE_CODE") |>
  left_join(parm_table, by = "parameter") |>
  filter(!is.na(shortParmName)) |>
  with(
    #.,
    wqi_calc(
      site = SITE_CODE,
      value = newResultValue,
      shortParmName = shortParmName,
      date = as.Date(DateTime),
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
      small_PS_stream = T, # assume all puget sound small streams
      summary_by = "ByParameter"
    )
  )
saveRDS(annual_wqi_by_parameter, "outputs/annual_wqi_by_parameter.RDS")
write_parquet(annual_wqi_by_parameter, "outputs/annual_wqi_by_parameter.parquet")

monthly_wqi_by_parameter <- streams_wq_dat |>
  left_join(streams_sites |> select(SITE_CODE, AquaticLifeUse), by = "SITE_CODE") |>
  left_join(parm_table, by = "parameter") |>
  filter(!is.na(shortParmName)) |>
  with(
    #.,
    wqi_calc(
      site = SITE_CODE,
      value = newResultValue,
      shortParmName = shortParmName,
      date = as.Date(DateTime),
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
      small_PS_stream = T, # assume all puget sound small streams
      summary_by = "ByParameter",
      period = "Monthly"
    )
  )
saveRDS(monthly_wqi_by_parameter, "outputs/monthly_wqi_by_parameter.RDS")
write_parquet(monthly_wqi_by_parameter, "outputs/monthly_wqi_by_parameter.parquet")

monthly_wqi <- streams_wq_dat |>
  left_join(streams_sites |> select(SITE_CODE, AquaticLifeUse), by = "SITE_CODE") |>
  left_join(parm_table, by = "parameter") |>
  filter(!is.na(shortParmName)) |>
  with(
    #.,
    wqi_calc(
      site = SITE_CODE,
      value = newResultValue,
      shortParmName = shortParmName,
      date = as.Date(DateTime),
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
      small_PS_stream = T, # assume all puget sound small streams
      period = "Monthly"
    )
  )
saveRDS(monthly_wqi, "outputs/monthly_wqi.RDS")
write_parquet(monthly_wqi, "outputs/monthly_wqi.parquet")
