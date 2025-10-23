#based on Ecology Version 6 - (2014.06.11)
#note that this uses outdated designations for Fecal Coliform (Extraordinary, Primary, Secondary)
#and uses fecal coliform instead of e. coli
#for the purposes of this effort - E. coli and Fecal coliform are assumed to be one to one, and the the primary standard is used
wqi_calc <- function(period = c('Annual', 'Monthly'),
                     summary_by = c('Index', 'ByParameter'),
                     site = NULL,
                     date,
                     value,
                     shortParmName,
                     TemperatureCode = NULL,
                     OxygenCode = NULL,
                     small_PS_stream = TRUE,
                     curveSource = 'inputs/wqi_ecology_curves_v6.csv',
                     debug = FALSE) {
  
  # Validate inputs ----
  period <- period[1]
  summary_by <- summary_by[1]
  parms_to_log <- c('FC', 'SUSSOL', 'TP_P', 'Turb')
  
  if (!file.exists(curveSource)) {
    stop('Curve source file not found: ', curveSource)
  }
  
  if (!(period %in% c('Annual', 'Monthly'))) {
    stop('period must be either "Annual" or "Monthly"')
  }
  
  if (!(summary_by %in% c('Index', 'ByParameter'))) {
    stop('summary_by must be either "Index" or "ByParameter"')
  }
  
  if (missing(site)) {
    site <- 'A'
    warning('No site name provided. Assuming data are all from same site ("A").')
  }
  
  valid_parms <- c('FC', 'Oxygen', 'pH', 'TP_P', 'SUSSOL', 'Temp', 'TPN', 'Turb')
  if (!any(shortParmName %in% valid_parms)) {
    stop('shortParmName must include at least one of: ', paste(valid_parms, collapse = ', '))
  }
  
  # Check for required codes
  has_temp <- any(shortParmName == 'Temp')
  has_oxygen <- any(shortParmName == 'Oxygen')
  
  if (has_temp && (missing(TemperatureCode) || all(is.na(TemperatureCode)))) {
    stop('Temperature data present but TemperatureCode is missing or all NA')
  }
  
  if (has_oxygen && (missing(OxygenCode) || all(is.na(OxygenCode)))) {
    stop('Oxygen data present but OxygenCode is missing or all NA')
  }
  
  # Load curves ----
  ecology_curves <- read.csv(curveSource)
  
  # Build initial dataset ----
  input_data <- tibble(
    site = site,
    date = date,
    shortParmName = shortParmName,
    value = value,
    TemperatureCode = TemperatureCode,
    OxygenCode = OxygenCode
  ) %>%
    filter(!is.na(value), !is.na(date), !is.na(shortParmName))
  
  if (debug) {
    message("Initial data counts by parameter:")
    print(input_data %>% count(shortParmName))
  }
  
  # Calculate date components and aggregate to monthly averages ----
  wqi_prep <- input_data %>%
    arrange(site, date, shortParmName) %>%
    mutate(
      Year = year(date),
      Month = month(date),
      WaterYear = ifelse(Month >= 10, Year + 1, Year),
      MonthFraction = Month + day(date) / days_in_month(Month)
    ) %>%
    # Average multiple samples within same month
    group_by(site, shortParmName, WaterYear, Month, MonthFraction, 
             TemperatureCode, OxygenCode) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = 'drop')
  
  if (debug) {
    message("\nAfter monthly aggregation:")
    print(wqi_prep %>% count(shortParmName))
  }
  
  # Assign curve lookup codes ----
  wqi_prep <- wqi_prep %>%
    mutate(
      WQI_Value = ifelse(shortParmName %in% parms_to_log, log(value), value),
      rownumber = case_when(
        shortParmName == 'Temp' ~ TemperatureCode,
        shortParmName == 'Oxygen' ~ OxygenCode,
        shortParmName == 'pH' ~ 41,
        shortParmName == 'FC' ~ 51,
        shortParmName == 'TPN' ~ ifelse(small_PS_stream, 262, 62),
        shortParmName == 'TP_P' ~ ifelse(small_PS_stream, 272, 72),
        shortParmName == 'SUSSOL' ~ 82,
        shortParmName == 'Turb' ~ 92,
        TRUE ~ NA_real_
      )
    )
  
  
  # Remove records with missing codes
  wqi_prep <- wqi_prep %>%
    filter(!is.na(rownumber))
  
  if (debug) {
    message("\nAfter assigning rownumber codes:")
    print(wqi_prep %>% count(shortParmName))
    message("\nUnique rownumber values:")
    print(sort(unique(wqi_prep$rownumber)))
  }
  
  # Join with curves and calculate WQI ----
  wqi_prep <- wqi_prep %>%
    left_join(ecology_curves, by = c('rownumber' = 'ParamClassID'), 
              relationship = "many-to-many")
  
  if (debug) {
    message("\nAfter joining with curves:")
    print(wqi_prep %>% count(shortParmName))
    
    # Check for failed joins
    failed_joins <- wqi_prep %>%
      filter(is.na(LowerResult)) %>%
      distinct(shortParmName, rownumber)
    
    if (nrow(failed_joins) > 0) {
      message("\nFailed to match these parameters to curves:")
      print(failed_joins)
    }
  }
  
  # Apply curve filters
  wqi_prep <- wqi_prep %>%
    filter(
      !is.na(LowerResult),
      value >= LowerResult,
      value <= UpperResult,
      MonthFraction >= Start.Month,
      MonthFraction < End.Month
    )
  
  if (debug) {
    message("\nAfter filtering by curve ranges:")
    print(wqi_prep %>% count(shortParmName))
  }
  
  # Calculate WQI scores ----
  wqi_prep <- wqi_prep %>%
    mutate(
      WQI = a + b * WQI_Value + b2 * WQI_Value^2,
      WQI = pmin(WQI, 100),
      WQI = pmax(WQI, 1)
    ) %>%
    select(site, shortParmName, WaterYear, Month, value, WQI_Value, WQI) %>%
    group_by(site, shortParmName, WaterYear, Month) %>%
    summarise(
      value = mean(value, na.rm = TRUE),
      WQI_Value = mean(WQI_Value, na.rm = TRUE),
      WQI = mean(WQI, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Month = factor(Month, levels = c(10:12, 1:9))) %>%
    arrange(site, shortParmName, WaterYear, Month) %>%
    group_by(site, WaterYear) %>%
    mutate(nSamples = length(unique(Month))) %>%
    ungroup()
  
  if (debug) {
    message("\nFinal wqi_prep by parameter:")
    print(wqi_prep %>% count(shortParmName))
  }
  
  # Calculate sediment score (combined TSS and Turbidity) ----
  if (any(shortParmName %in% c('SUSSOL', 'Turb'))) {
    sediment_score <- wqi_prep %>%
      filter(shortParmName %in% c('SUSSOL', 'Turb')) %>%
      group_by(site, WaterYear, Month, nSamples) %>%
      summarise(
        shortParmName = 'Sediment',
        WQI = sum(WQI^-1)^-1 * length(WQI),
        .groups = 'drop'
      )
  } else {
    sediment_score <- NULL
  }
  
  # Calculate nutrient score (combined N and P) ----
  if (any(shortParmName %in% c('TPN', 'TP_P'))) {
    np_ratios <- wqi_prep %>%
      select(-WQI_Value, -WQI) %>%
      filter(shortParmName %in% c('TPN', 'TP_P')) %>%
      group_by(site, WaterYear, Month, nSamples) %>%
      tidyr::pivot_wider(
        names_from = shortParmName,
        values_from = value,
        values_fn = mean
      ) %>%
      mutate(NPRatio = TPN / TP_P) %>%
      select(site, WaterYear, Month, NPRatio)
    
    nutrient_score <- wqi_prep %>%
      filter(shortParmName %in% c('TPN', 'TP_P')) %>%
      left_join(np_ratios, by = c('site', 'WaterYear', 'Month','nSamples')) %>%
      group_by(site, WaterYear, Month, nSamples) %>%
      summarise(
        WQI = case_when(
          is.na(NPRatio) ~ min(WQI, na.rm = TRUE),
          NPRatio <= 10 ~ WQI[shortParmName == 'TPN'],
          NPRatio > 20 ~ WQI[shortParmName == 'TP_P'],
          TRUE ~ min(WQI, na.rm = TRUE)
        ),
        .groups = 'drop'
      ) %>%
      mutate(shortParmName = 'Nutrient')
  } else {
    nutrient_score <- NULL
  }
  
  # Calculate monthly WQI index ----
  monthly_wqi <- wqi_prep %>%
    filter(!(shortParmName %in% c('TP_P', 'TPN', 'Turb', 'SUSSOL'))) %>%
    select(-WQI_Value, -value) %>%
    bind_rows(sediment_score, nutrient_score) %>%
    group_by(site, WaterYear, Month, nSamples) %>%
    mutate(
      Penalty = pmax((85 - WQI) / 2, 0),
      Penalty = ifelse(WQI >= 80, 0, Penalty),
      Penalty = ifelse(shortParmName %in% c('Sediment', 'Nutrient') & Penalty > 20, 
                       20, Penalty)
    ) %>%
    summarise(
      WQI = mean(WQI, na.rm = TRUE) - sum(Penalty, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(WQI = pmax(WQI, 1))
  
  # Return monthly index if requested ----
  if (period == 'Monthly' & summary_by == 'Index') {
    return(monthly_wqi)
  }
  
  # Calculate annual WQI index (average of 3 lowest monthly scores) ----
  annual_wqi <- monthly_wqi %>%
    group_by(site, WaterYear, nSamples) %>%
    summarise(
      WQI = mean(sort(WQI)[1:min(3, length(WQI))], na.rm = TRUE),
      .groups = 'drop'
    )
  
  if (period == 'Annual' & summary_by == 'Index') {
    return(annual_wqi)
  }
  
  # Monthly by parameter ----
  if (period == 'Monthly' & summary_by == 'ByParameter') {
    monthly_wqi_by_parameter <- wqi_prep %>%
      select(-WQI_Value, -value) %>%
      bind_rows(sediment_score, nutrient_score) %>%
      group_by(site, WaterYear, Month, nSamples) %>%
      mutate(
        Penalty = ifelse(
          WQI < 80 & !(shortParmName %in% c('TP_P', 'TPN', 'Turb', 'SUSSOL')),
          (85 - WQI) / 2,
          0
        ),
        Penalty = pmax(Penalty, 0),
        Penalty = ifelse(shortParmName %in% c('Sediment', 'Nutrient') & Penalty > 20,
                         20, Penalty),
        TotalPenalty = sum(Penalty)
      ) %>%
      select(-Penalty) %>%
      mutate(
        shortParmName = factor(
          shortParmName,
          levels = c('Temp', 'Oxygen', 'pH', 'FC', 'TPN', 'TP_P', 
                     'Nutrient', 'Turb', 'SUSSOL', 'Sediment')
        )
      ) %>%
      tidyr::pivot_wider(
        values_from = WQI,
        names_from = shortParmName,
        values_fn = mean,
        names_expand = TRUE
      )
    
    return(monthly_wqi_by_parameter)
  }
  
  # Annual by parameter ----
  if (period == 'Annual' & summary_by == 'ByParameter') {
    annual_wqi_by_parameter <- wqi_prep %>%
      select(-WQI_Value, -value) %>%
      bind_rows(sediment_score, nutrient_score) %>%
      group_by(site, shortParmName, WaterYear, nSamples) %>%
      summarise(
        # For Temp, Oxygen, pH: take worst (minimum) month
        # For others: average of 3 worst months
        WQI = if_else(
          grepl('Oxygen|Temp|pH', shortParmName),
          min(WQI, na.rm = TRUE),
          mean(sort(WQI)[1:min(3, length(WQI))], na.rm = TRUE)
        ),
        .groups = 'drop'
      ) %>%
      group_by(site, shortParmName, WaterYear, nSamples) %>%
      summarise(WQI = mean(WQI, na.rm = TRUE), .groups = 'drop') %>%
      mutate(
        shortParmName = factor(
          shortParmName,
          levels = c('Temp', 'Oxygen', 'pH', 'FC', 'TPN', 'TP_P',
                     'Nutrient', 'Turb', 'SUSSOL', 'Sediment')
        )
      ) %>%
      tidyr::pivot_wider(
        values_from = WQI,
        names_from = shortParmName,
        values_fn = mean,
        names_expand = TRUE
      ) %>%
      left_join(
        annual_wqi %>% select(site, WaterYear, AnnualWQI = WQI),
        by = c('site', 'WaterYear')
      )
    
    return(annual_wqi_by_parameter)
  }
  
  # Should never reach here
  stop("Unexpected combination of period and summary_by")
}
