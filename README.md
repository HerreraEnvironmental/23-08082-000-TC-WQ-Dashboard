# 23-08082-000-TC-WQ-Dashboard
Thurston County Dashboard

This dashboard presents water quality monitoring data for Thurston County streams

https://herrerainc.shinyapps.io/ThurstonWQDashboard_Streams_Dev/

## Input files
* "Herrera All Stream Data Dump 4 12 2023.csv"
  * This file has two uses:
      1.  it is used to generate a table of SITE_CODE, SITE_NAME, LAT, and LON, which is used for mapping
      2.  it is used as the source of water quality data for plotting, calculating WQI and comparing to water quality criteria.
  * The following column structure is needed
    * date_time - MM/DD/YY HH:MM in local time (assumed to be text value)
    * SITE_CODE - text value designating the site - note that the SITE_CODE names on WQX appear to be slightly different, often haveing a prefix
    * SITE_NAME - long-form site name
    * Metro_ID - read, but not used
    * LAT - site location latitude numeric
    * LON - site location longitude, numeric
    * parameter - parameter, character - Dashboard was developed for the following distinct parameter names:
      * "Temperature, water", "Dissolved Oxygen", "Specific Conductivity (at 25 deg C)", "pH", "Turbidity", "E. coli", "Conductivity", "Nitrate-Nitrite as N", "Fecal Coliform", "Total Phosphorus", "Flow", "Enterococci", "Ammonia (NH3) as Nitrogen (N)", "Copper", "Total Petroleum Hydrocarbons", "Zinc", "Hardness, ion non-specific", "Lead", "Total Suspended Solids"
      * deviation from these parameter names may result in errors in the calculation of WI and water quality ceriteria
    * value - numeric result for specific parameter
    * unit - text value for result units (e.g, mg/L)
    * depth_m - numeric value of sampling depth, typically NA for streams
    * dup - TRUE/FALSE (1/0) value for whether sample is a field duplicate
    * mdl - numeric value of laboratory method detection limit
    * pql - numeric value of laboratory practical quantitation limit (or reporting limit)
    * qualifier - text value for laboratory qualifiers - "U" flags are used to designate non-detects (i.e., <MDL)
* "Stream Use Designations Herrera.xlsx"
  * This file is used to establish the aquatic life use designation for Stream Sites. This is used as a look-up table based on SITE_CODE.
  * The file is used for calculating site-specific WQI and comparing to relevant water quality criteria
 ## How to Update
1. Append or replace "Herrera All Stream Data Dump 4 12 2023.csv" with updated data. **We can change the name for ease of use**
2. If additional sites are added, modify the "Stream Use Designations Herrera.xlsx" with relevant information
3. Run "deploy_app.R"
