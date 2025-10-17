library(tidyverse)
library(readxl)
library(lubridate)

## Additional Filtered Export of Most Recent WQI score

# Supplementary imports ----------------------------------------------------------
stream_use_designations <- read_xlsx("inputs/Stream Use Designations Herrera.xlsx") |>
  transmute(
    SITE_CODE=`Site Code`,
    AquaticLifeUse=case_when(
        grepl("Spawn",`ALU (Temp. °C)`) ~ "Salmonid Spawning, Rearing, and Migration",
        grepl('Core',`ALU (Temp. °C)`) ~ "Core Summer Salmonid Habitat",
        grepl("13",`ALU (Temp. °C)`) ~ "Marine",
        TRUE ~ "ERROR"
        )
    ) |>
  filter(!is.na(SITE_CODE))

parm_table <- tibble::tibble(
  shortParmName = c(
    "FC", "FC", "FC",
    "Oxygen",
    "pH",
    "TP_P",
    "SUSSOL",
    "Temp",
    "TPN", "TPN",
    "Turb"
  ),
  parameter = c(
    "Fecal Coliform",
    "E. coli",
    "Fecal Bacteria",
    "Dissolved Oxygen",
    "pH",
    "Total Phosphorus",
    "Total Suspended Solids",
    "Temperature, water",
    "Total Nitrogen",
    "Nitrate + Nitrite",
    "Turbidity"
  )
)



# WQI calc ----------------------------------------------------------------
source('wqi_function.R')


# Application of calcs ----------------------------------------
streams_wq_dat <- readRDS("outputs/streams_wq_dat.RDS")

wqx_site_names<-unique(streams_wq_dat$SITE_CODE)

annual_wqi_public<-streams_wq_dat %>% 
  left_join(stream_use_designations%>%
              rowwise() %>%
              mutate(SITE_CODE=wqx_site_names[which(grepl(SITE_CODE,wqx_site_names))[1]]) %>%
              ungroup()) %>%
  left_join(parm_table) %>%
  filter(!is.na(shortParmName)) %>%
  mutate(
    TemperatureCode = ifelse(AquaticLifeUse=='Core Summer Salmonid Habitat'|is.na(AquaticLifeUse),8,
                             ifelse(AquaticLifeUse=='Salmonid Spawning, Rearing, and Migration',9,8)),
    OxygenCode =  ifelse(AquaticLifeUse=='Core Summer Salmonid Habitat'|is.na(AquaticLifeUse),26,
                         ifelse(AquaticLifeUse=='Salmonid Spawning, Rearing, and Migration',21,26)), 
  ) %>%
  with(.,
       wqi_calc(period = "Annual", summary_by = "ByParameter", site=SITE_CODE,
                value=newResultValue,
                shortParmName = shortParmName,
                date=as.Date(DateTime),
                TemperatureCode = TemperatureCode, 
                OxygenCode =  OxygenCode, 
                small_PS_stream = T
       )) %>%
  ungroup() %>%
  filter(nSamples>=6) %>%
  select(site, WaterYear, nSamples:AnnualWQI) %>%
  arrange(site,WaterYear) %>%
  unique()

write_csv(annual_wqi_public, "public_dashboard_outputs_streams/recent_WQI.csv")

