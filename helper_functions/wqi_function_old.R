## Based on Ecology Version 6 (2014-06-11)
# Note:
# - This uses historical designations for Fecal Coliform (Extraordinary, Primary, Secondary)
# - Uses fecal coliform (FC) rather than E. coli
# - For this effort, E. coli and fecal coliform are assumed 1-to-1, and the primary standard is used

library(readxl)
library(dplyr)
library(lubridate)

# ecology_curves <- read_excel("WQI_ecology_v6.xlsm", sheet = "Curves", n_max = 204)
# write.csv(ecology_curves, "wqi_ecology_curves_v6.csv", row.names = FALSE)

# Some coefficient sets depend on month.
# Need to bring in a lookup table that designates each of the stream uses:
# aquatic life, supplemental criteria dates for temperature and oxygen code.

wqi_calc <- function(
  period         = c("Annual", "Monthly"),
  summary_by     = c("Index", "ByParameter"),
  site           = NULL,
  date,
  value,
  shortParmName,
  TemperatureCode = NULL,
  OxygenCode       = NULL,
  small_PS_stream  = TRUE,
  curveSource      = "inputs/wqi_ecology_curves_v6.csv"
) {
  # Basic equation: WQI = a + b * [C] + b2 * [C]^2
  # where [C] is the constituent in question.
  # FC, Total Suspended Solids, Total Phosphorus, and Turbidity are
  # (natural) log-transformed prior to this equation.
  period     <- period[1]
  summary_by <- summary_by[1]
  parms_to_log <- c("FC", "SUSSOL", "TP_P", "Turb")

  if (!file.exists(curveSource)) {
    stop("Must provide lookup table")
  }

  if (!(period %in% c("Annual", "Monthly"))) {
    stop("Must select 'Annual' or 'Monthly' for WQI summary")
  }
  if (!(summary_by %in% c("Index", "ByParameter"))) {
    stop("Must select 'Index' or 'ByParameter' for output")
  }

  if (any(shortParmName == "Temp" & missing(TemperatureCode))) {
    stop("Must provide temperature code (depends on WQ Standards) to calculate WQI for temperature")
  }
  if (any(shortParmName == "Oxygen" & missing(OxygenCode))) {
    stop("Must provide oxygen code (depends on WQ Standards) to calculate WQI for oxygen")
  }

  if (missing(site)) {
    site <- "A"
    warning('No site name provided. Assuming all data are from the same site ("A").')
  }

  if (!any(shortParmName %in% c("FC", "Oxygen", "pH", "TP_P", "SUSSOL", "Temp", "TPN", "Turb"))) {
    stop(
      "Must provide parameter name in shortened form:\n",
      "  FC    - Fecal coliform\n",
      "  Oxygen- Dissolved Oxygen (mg/L)\n",
      "  pH    - pH\n",
      "  TP_P  - Total Phosphorus (mg/L)\n",
      "  SUSSOL- Total Suspended Solids (mg/L)\n",
      "  Temp  - Temperature (°C)\n",
      "  TPN   - Total (persulfate) nitrogen (mg/L)\n",
      "  Turb  - Turbidity (NTU)"
    )
  }

  ecology_curves <- read.csv(curveSource)

  wqi_prep <- tibble(
    site,
    date,
    shortParmName,
    value,
    TemperatureCode,
    OxygenCode
  ) %>%
    mutate(
      Year      = year(date),
      Month     = month(date),
      WaterYear = ifelse(Month >= 10, Year + 1, Year)
    ) %>%
    mutate(
      MonthFraction = Month + day(date) / days_in_month(Month)
    ) %>%
    group_by(site, shortParmName, WaterYear, Month, MonthFraction) %>%
    summarise(value = mean(value), .groups = "drop") %>%
    mutate(WQI_Value = ifelse(shortParmName %in% parms_to_log, log(value), value)) %>%
    mutate(
      rownumber = ifelse(
        shortParmName == "Temp",  TemperatureCode,
        ifelse(
          shortParmName == "TP_P", ifelse(small_PS_stream, 72, 272),
          ifelse(
            shortParmName == "Oxygen", OxygenCode,
            ifelse(
              shortParmName == "TPN", ifelse(small_PS_stream, 62, 262),
              ifelse(
                shortParmName == "Turb",   92,
                ifelse(
                  shortParmName == "SUSSOL", 82,
                  ifelse(
                    shortParmName == "FC",   51,
                    ifelse(shortParmName == "pH", 41, -9999)
                  )
                )
              )
            )
          )
        )
      )
    ) %>%
    left_join(
      ecology_curves,
      by = c("rownumber" = "ParamClassID"),
      relationship = "many-to-many"
    ) %>%
    filter(
      value >= LowerResult,
      value <= UpperResult,
      MonthFraction >= Start.Month,
      MonthFraction <  End.Month
    ) %>%
    mutate(WQI = a + b * WQI_Value + b2 * WQI_Value^2) %>%
    mutate(WQI = ifelse(WQI >= 100, 100, WQI)) %>%
    mutate(WQI = ifelse(WQI < 1, 1, WQI)) %>%
    select(site, shortParmName, WaterYear, Month, value, WQI_Value, WQI) %>%
    group_by(site, shortParmName, WaterYear, Month) %>%
    summarise(
      value     = mean(value),
      WQI_Value = mean(WQI_Value),
      WQI       = mean(WQI),
      .groups   = "drop"
    ) %>%
    mutate(Month = factor(Month, levels = c(10:12, 1:9))) %>%
    arrange(site, shortParmName, WaterYear, Month)

  # Monthly Score = FC + Oxygen + pH + Temp + NUTRIENT SCORE + SEDIMENT SCORE
  # Sediment score = 2 / (1/TSS + 1/Turb)  (harmonic-mean style aggregation)

  # Nutrient score is based on N:P ratio:
  # - If ratio <= 10, use TN (TPN)
  # - If ratio > 20, use TP (TP_P)
  # - Otherwise use the minimum of the two WQIs

  if (any(shortParmName %in% c("SUSSOL", "Turb"))) {
    sediment_score <- wqi_prep %>%
      filter(shortParmName %in% c("SUSSOL", "Turb")) %>%
      group_by(site, WaterYear, Month) %>%
      summarise(
        shortParmName = "Sediment",
        WQI = (sum(WQI^-1)^-1) * length(WQI),
        .groups = "drop"
      )
  } else {
    sediment_score <- NULL
  }

  if (any(shortParmName %in% c("TPN", "TP_P"))) {
    np_ratios <- wqi_prep %>%
      select(-WQI_Value, -WQI) %>%
      filter(shortParmName %in% c("TPN", "TP_P")) %>%
      group_by(site, WaterYear, Month) %>%
      tidyr::pivot_wider(
        names_from  = shortParmName,
        values_from = value,
        values_fn   = mean
      ) %>%
      mutate(NPRatio = TPN / TP_P) %>%
      select(site, WaterYear, Month, NPRatio)

    nutrient_score <- wqi_prep %>%
      filter(shortParmName %in% c("TPN", "TP_P")) %>%
      left_join(np_ratios, by = c("site", "WaterYear", "Month")) %>%
      group_by(site, WaterYear, Month) %>%
      reframe(
        # Choose which nutrient WQI to use based on NPRatio thresholds
        WQI = ifelse(
          is.na(NPRatio),
          min(WQI, na.rm = TRUE),
          ifelse(
            NPRatio <= 10,
            WQI[shortParmName == "TPN"],
            ifelse(
              NPRatio > 20,
              WQI[shortParmName == "TP_P"],
              min(WQI, na.rm = TRUE)
            )
          )
        )
      ) %>%
      mutate(shortParmName = "Nutrient") %>%
      group_by(site, WaterYear, Month, shortParmName) %>%
      summarise(WQI = mean(WQI), .groups = "drop")
  } else {
    nutrient_score <- NULL
  }

  if (summary_by == "Index") {
    monthly_wqi <- wqi_prep %>%
      filter(!(shortParmName %in% c("TP_P", "TPN", "Turb", "SUSSOL"))) %>%
      select(-WQI_Value, -value) %>%
      bind_rows(sediment_score, nutrient_score) %>%
      group_by(site, WaterYear, Month) %>%
      mutate(Penalty = ifelse(WQI < 80, (85 - WQI) / 2, 0)) %>%
      mutate(Penalty = ifelse(Penalty < 0, 0, Penalty)) %>%
      mutate(
        Penalty = ifelse(
          shortParmName %in% c("Sediment", "Nutrient") & Penalty > 20,
          20,
          Penalty
        )
      ) %>%
      summarise(WQI = mean(WQI) - sum(Penalty), .groups = "drop") %>%
      mutate(WQI = ifelse(WQI < 1, 1, WQI))
  }

  if (period == "Monthly" & summary_by == "Index") {
    return(monthly_wqi)
  }

  if (period == "Monthly" & summary_by == "ByParameter") {
    monthly_wqi_by_parameter <- wqi_prep %>%
      select(-WQI_Value, -value) %>%
      bind_rows(sediment_score, nutrient_score) %>%
      group_by(site, WaterYear, Month) %>%
      mutate(
        Penalty = ifelse(
          WQI < 80 & !(shortParmName %in% c("TP_P", "TPN", "Turb", "SUSSOL")),
          (85 - WQI) / 2,
          0
        )
      ) %>%
      mutate(Penalty = ifelse(Penalty < 0, 0, Penalty)) %>%
      mutate(
        Penalty = ifelse(
          shortParmName %in% c("Sediment", "Nutrient") & Penalty > 20,
          20,
          Penalty
        )
      ) %>%
      mutate(Penalty = sum(Penalty)) %>%
      mutate(
        shortParmName = factor(
          shortParmName,
          levels = c(
            "Temp", "Oxygen", "pH", "FC", "TPN", "TP_P",
            "Nutrient", "Turb", "SUSSOL", "Sediment"
          )
        )
      ) %>%
      tidyr::pivot_wider(
        values_from  = WQI,
        names_from   = shortParmName,
        values_fn    = mean,
        names_expand = TRUE
      )
    return(monthly_wqi_by_parameter)
  }

  if (period == "Annual" & summary_by == "ByParameter") {
    annual_wqi_by_parameter <- wqi_prep %>%
      select(-WQI_Value, -value) %>%
      bind_rows(sediment_score, nutrient_score) %>%
      group_by(site, shortParmName, WaterYear) %>%
      reframe(
        # For Oxygen/Temp/pH use the minimum monthly WQI;
        # for others use the mean of the three lowest monthly WQIs
        WQI = ifelse(
          grepl("Oxygen|Temp|pH", shortParmName),
          sort(WQI)[1],
          mean(sort(WQI)[1:3], na.rm = TRUE)
        )
      ) %>%
      group_by(site, shortParmName, WaterYear) %>%
      summarise(WQI = mean(WQI, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        shortParmName = factor(
          shortParmName,
          levels = c(
            "Temp", "Oxygen", "pH", "FC", "TPN", "TP_P",
            "Nutrient", "Turb", "SUSSOL", "Sediment"
          )
        )
      ) %>%
      tidyr::pivot_wider(
        values_from  = WQI,
        names_from   = shortParmName,
        values_fn    = mean,
        names_expand = TRUE
      )
    return(annual_wqi_by_parameter)
  }

  if (period == "Annual" & summary_by == "Index") {
    annual_wqi <- monthly_wqi %>%
      group_by(site, WaterYear) %>%
      summarise(WQI = mean(sort(WQI)[1:3], na.rm = TRUE), .groups = "drop")
    return(annual_wqi)
  }
}
#
# ## Run example
# streams_wq_dat <- readRDS("outputs/streams_wq_dat.RDS")
# streams_sites  <- readRDS("outputs/streams_sites.RDS")
#
# # Make a lookup list for parameter names
# parm_table <- data.frame(rbind(
#   c("FC",   "Fecal Coliform"),
#   c("FC",   "E. coli"),
#   c("FC",   "Fecal Bacteria"),
#   c("Oxygen","Dissolved Oxygen"),
#   c("pH",   "pH"),
#   c("TP_P", "Total Phosphorus"),
#   c("SUSSOL","Total Suspended Solids"),
#   c("Temp", "Water Temperature (°C)"),
#   # c("TPN", "Total Nitrogen"),
#   # For the sake of this example let's use nitrate
#   c("TPN",  "Nitrate-Nitrite as N"),
#   c("Turb", "Turbidity")
# ))
# colnames(parm_table) <- c("shortParmName", "parameter")
#
# tc_annual_wqi <- streams_wq_dat %>%
#   left_join(parm_table, by = "parameter") %>%
#   # filter(shortParmName != "Temp") %>%
#   filter(!is.na(shortParmName)) %>%
#   with(.,
#     wqi_calc(
#       period          = "Annual",
#       summary_by      = "ByParameter",
#       site            = SITE_CODE,
#       value           = newResultValue,
#       shortParmName   = shortParmName,
#       date            = as.Date(DateTime),
#       TemperatureCode = 8,   # assume Core Summer Salmonid Habitat, for example
#       OxygenCode      = 26,  # assume core for now
#       small_PS_stream = TRUE # assume all Puget Sound small streams
#     )
#   )
