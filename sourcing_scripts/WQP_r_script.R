library(arrow)
library(tidyverse)
library(dataRetrieval)

## Define Required Codes
WQP_COUNTY_CODE <- "US:53:067"
WQP_ORG_ID <- "THURSTONCOUNTY"
DEFAULT_TOD <- "00:00:00"
STREAM_SITE_TYPES <- c(
  "Other-Surface Water",
  "River/Stream",
  "Pipe, Unspecified Source",
  "Spring",
  "River/Stream Perennial",
  "Seep",
  "River/Stream Intermittent",
  "Storm Sewer",
  "Estuary",
  "River/Stream Ephemeral"
)

## Import data with required filters
thurston_stream_sites <- readWQPdata(
  organization = "THURSTONCOUNTY", # Sites owned by Thurston County
  service = "Station", # Sites that capture stream data
  Project = "Ambient_Water_Quality_Streams" # Routine stream sites
)

## Fetch all water quality data to be used
# Takes roughly 1 minute to download
wqp_data <- readWQPdata(
  siteid = thurston_stream_sites$MonitoringLocationIdentifier
)

## Print unique wqp parameters
unique(wqp_data$CharacteristicName)

## Fetch other metadata from thurston_stream_sites to combine with hec_data
gid_ref <- thurston_stream_sites %>%
  select(c(
    MonitoringLocationIdentifier,
    MonitoringLocationDescriptionText,
    MonitoringLocationName,
    LatitudeMeasure,
    LongitudeMeasure
  ))

gid_ref$gid <- str_match(gid_ref$MonitoringLocationDescriptionText,
                         "GData ID:\\s*(.*?)\\s*;")[, 2] |>
  as.numeric()

## Grab necessary columns for HEC format
hec_data <- wqp_data %>%
  select(
    c(
      MonitoringLocationIdentifier,
      ActivityStartDate,
      ActivityStartTime.Time,
      ResultMeasureValue,
      ResultMeasure.MeasureUnitCode,
      CharacteristicName,
      ResultAnalyticalMethod.MethodIdentifier,
      DetectionQuantitationLimitMeasure.MeasureValue,
      ActivityMediaSubdivisionName,
      MeasureQualifierCode,
      ActivityTypeCode,
      ResultDepthHeightMeasure.MeasureValue
    )
  )


## Merge hec_data with gid references
hec_data_merged <- merge(hec_data, gid_ref, by = "MonitoringLocationIdentifier")
unique(hec_data_merged$CharacteristicName)

## Tidy the hec_data_merged dataframe
hec_data_merged$MonitoringLocationIdentifier <- str_remove(
  hec_data_merged$MonitoringLocationIdentifier,
  "THURSTONCOUNTY-"
)

hec_data_merged$DT <- paste(
  hec_data_merged$ActivityStartDate,
  hec_data_merged$ActivityStartTime.Time,
  sep = " "
)

hec_data_merged <- hec_data_merged |>
  mutate(
    ResultMeasureValue = as.numeric(as.character(ResultMeasureValue)),
    DetectionQuantitationLimitMeasure.MeasureValue =
      as.numeric(DetectionQuantitationLimitMeasure.MeasureValue),
    SITE_CODE = MonitoringLocationIdentifier,
    sample_utc_offset = NA,
    pql = NA,
    lab_batch = NA
  )

hec_final <- hec_data_merged %>%
  select(c(
    SITE_CODE,
    MonitoringLocationIdentifier,
    DT,
    ResultMeasureValue,
    ResultMeasure.MeasureUnitCode,
    CharacteristicName,
    ResultAnalyticalMethod.MethodIdentifier,
    DetectionQuantitationLimitMeasure.MeasureValue,
    ActivityMediaSubdivisionName,
    MeasureQualifierCode,
    ActivityTypeCode,
    gid,
    MonitoringLocationName,
    LatitudeMeasure,
    LongitudeMeasure,
    sample_utc_offset,
    pql,
    lab_batch,
    ResultDepthHeightMeasure.MeasureValue
  ))

## Rename columns
colnames(hec_final) <- c(
  "SITE_CODE",
  "Metro_ID",
  "date_time",
  "value",
  "unit",
  "parameter",
  "method",
  "mdl",
  "matrix",
  "qualifier",
  "dup",
  "gid",
  "SITE_NAME",
  "LAT",
  "LON",
  "sample_utc_offset",
  "pql",
  "lab_batch",
  "depth_m"
)

hec_final <- hec_final |>
  mutate(
    date_time = date_time |>
      str_replace("NA", "00:00:00") |>
      ymd_hms()
  ) |>
  filter(!is.na(date_time))


## Transform data to format used in Herrera All Stream Data Dump 4 12 2023.csv
write.csv(hec_final, file = "inputs/wqp_data.csv")
write_parquet(hec_final, "inputs/wqp_data.parquet")
