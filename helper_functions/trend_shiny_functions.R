# Trend Plot and text summary for water quality data

trend_plot <- function(dataSubset, input) {
  # Takes user-selected parameter and a given data subset to create the trend plot
  season_ranges <- list(
    winter = 1:3,
    spring = 4:6,
    summer = 7:9,
    fall   = 10:12
  )

  dataSubset <- dataSubset |>
    dplyr::filter(Month %in% season_ranges[[input$rktSeason_oneSite]])

  trendplot <- dataSubset %>%
    ggplot(aes(x = DateTime, y = value)) +
    geom_point(
      data = ~ dplyr::filter(.x, WaterYear == input$data_year),
      color = "red",
      size = 4
    ) +
    geom_point() +
    geom_smooth(
      data = ~ dplyr::filter(
        .x,
        WaterYear >= input$trend_years[1] &
          WaterYear <= input$trend_years[2]
      ),
      se = FALSE
    ) +
    theme_bw() +
    scale_y_continuous(name = input$trend_parm)

  if (input$trend_parm == "Temperature, water") {
    temp_criteria <- wqc_finder(
      unique(dataSubset$AquaticLifeUse),
      input$trend_parm
    )
    trendplot <- trendplot +
      geom_hline(yintercept = temp_criteria)
  }

  if (input$trend_parm == "Dissolved Oxygen") {
    do_criteria <- wqc_finder(
      unique(dataSubset$AquaticLifeUse),
      input$trend_parm
    )
    trendplot <- trendplot +
      geom_hline(yintercept = do_criteria)
  }

  if (input$trend_parm == "pH") {
    trendplot <- trendplot +
      geom_hline(yintercept = c(6.5, 8.5))
  }

  if (input$trend_parm == "Fecal Coliform") {
    trendplot <- trendplot +
      geom_hline(yintercept = c(100, 200))
  }
  if (input$trend_parm == "E. coli") {
    trendplot <- trendplot +
      geom_hline(yintercept = c(100, 320))
  }

  if (input$data_log_scale) {
    trendplot <- trendplot +
      scale_y_log10(
        input$trend_parm,
        breaks = 10^(-4:4),
        minor_breaks = log10_minor_break()
      )
  }

  ggplotly(trendplot)
}


trend_text <- function(dataSubset, input) {
  # Takes user-selected parameter input and a data subset to add text to the trend plot
  season_months <- list(
    winter = 1:3,
    spring = 4:6,
    summer = 7:9,
    fall   = 10:12
  )

  dataSubset <- dataSubset |>
    dplyr::filter(Month %in% season_months[[input$rktSeason_oneSite]])

  trend_out <- dataSubset %>%
    filter(
      WaterYear >= input$trend_years[1] & WaterYear <= input$trend_years[2]
    ) %>%
    with(
      .,
      rkt::rkt(
        WaterYear,
        newResultValue,
        Month,
        correct = input$rktAuto_oneSite,
        rep = "a"
      )
    )

  trend_unit <- unique(dataSubset$unit)[1]

  p.value <- ifelse(input$rktAuto_oneSite, trend_out$sl.corrected, trend_out$sl)

  if (is.na(p.value)) {
    HTML(paste0(
      "<u>Mann-Kendall Trend Test:</u>",
      "<br/>",
      "There are not enough data to evaluate trends."
    ))
  } else {
    sigStatement <- paste0(
      ifelse(
        p.value <= 0.05,
        "a  significant trend",
        "insufficient evidence of a trend"
      ),
      " (p",
      ifelse(p.value < 0.001, "<0.001)", paste0("=", round(p.value, 3), ")"))
    )

    slopeStatement <- ifelse(
      p.value <= 0.05,
      paste(
        "The trend slope is",
        round(trend_out$B, 4),
        trend_unit,
        "per year"
      ),
      ""
    )

    HTML(paste0(
      "<u>Mann–Kendall Trend Test:</u><br/>",
      "Between water years <b>", input$trend_years[1], "</b> and <b>",
      input$trend_years[2], "</b>",
      ifelse(
        input$rktSeason_oneSite == "All",
        "",
        paste0(" (", input$rktSeason_oneSite, ")")
      ),
      " at ", input$main_site2, ", there is <b>", sigStatement, "</b> in <b>",
      input$trend_parm, "</b><br/>",
      slopeStatement
    ))
  }
}
