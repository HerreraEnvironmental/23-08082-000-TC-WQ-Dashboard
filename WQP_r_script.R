library(tidyverse)
library(dataRetrieval)
library(stringr)
library(lubridate)

WQP_COUNTY_CODE <- "US:53:067"
WQP_ORG_ID <- "THURSTONCOUNTY"
DEFAULT_TOD <- "00:00:00"
STREAM_SITE_TYPES <- c("Other-Surface Water", "River/Stream", "Pipe, Unspecified Source", "Spring", "River/Stream Perennial", "Seep", "River/Stream Intermittent", "Storm Sewer", "Estuary", "River/Stream Ephemeral")

# Fetch all sites in Thurston County
# Result will contain sites that are not owned by Thurston County Gov but adding more filter params here do not work

all_county_sites <- whatWQPsites(
  countycode = WQP_COUNTY_CODE
)

# Filter list for:
#   Thurston County owned sites
#   Sites that capture stream data

thurston_stream_sites <- all_county_sites %>%
  filter(
    OrganizationIdentifier == WQP_ORG_ID,
    MonitoringLocationTypeName %in% STREAM_SITE_TYPES,
    !is.na(MonitoringLocationDescriptionText)
  )

# Select only the site ids to be used in next data fetch
# This list of ids will likely not change very often so the previous steps may not be necessary each time only to update site id list

thurston_stream_sites_ids <- thurston_stream_sites$MonitoringLocationIdentifier

# Now fetch all water quality data for the selected sites
# Takes roughly 1 minute to download

wqp_data <- readWQPdata(
  siteid = thurston_stream_sites_ids
)

# Fetch other meta data from thurston_stream_sites to combine with hec_data
gid_ref <- thurston_stream_sites %>%
  select(c(MonitoringLocationIdentifier,
           MonitoringLocationDescriptionText,
           MonitoringLocationName,
           LatitudeMeasure,
           LongitudeMeasure))

gid_ref$gid <- str_match(gid_ref$MonitoringLocationDescriptionText, "GData ID:\\s*(.*?)\\s*;")

gid_ref$gid <- as.numeric(gid_ref$gid[,2])

#gid_ref <- gid_ref[,c(1,5,3,4)]

# Grab necessary columns for HEC format

hec_data <- wqp_data %>%
  select(
    c(MonitoringLocationIdentifier, 
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
      ResultDepthHeightMeasure.MeasureValue)
  )
  

# Merging hec_data with gid_ref

hec_data_merged <- merge(hec_data, gid_ref, by = "MonitoringLocationIdentifier")

# Cleaning of hec_data_merged dataframe

hec_data_merged$MonitoringLocationIdentifier <- str_remove(hec_data_merged$MonitoringLocationIdentifier, "THURSTONCOUNTY-")
hec_data_merged$DT <- paste(hec_data_merged$ActivityStartDate, hec_data_merged$ActivityStartTime.Time, sep = " ")

hec_data_merged$ResultMeasureValue <- as.numeric(hec_data_merged$ResultMeasureValue)
hec_data_merged$DetectionQuantitationLimitMeasure.MeasureValue <- as.numeric(hec_data_merged$DetectionQuantitationLimitMeasure.MeasureValue)
hec_data_merged$SITE_CODE <- hec_data_merged$MonitoringLocationIdentifier

hec_data_merged$sample_utc_offset <- NA
hec_data_merged$pql <- NA
hec_data_merged$lab_batch <- NA


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


colnames(hec_final) <- c("SITE_CODE",
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
                         "depth_m")

hec_final$date_time <- ymd_hms(hec_final$date_time)
hec_final <- hec_final[!is.na(hec_final$date_time),]


# Transform data to format used in Herrera All Stream Data Dump 4 12 2023.csv

write.csv(hec_final, file = "wqp_data.csv")
