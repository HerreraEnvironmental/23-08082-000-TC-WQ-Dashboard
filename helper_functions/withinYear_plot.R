## Plot of data for a given parameter normalized across a water year
# Requires tidyverse

withinYear_plot <- function(dataSubset, input) {
  dataplot <- dataSubset %>%
    arrange(WY_FakeDate) %>%
    ggplot(aes(x = WY_FakeDate, y = value, group = WaterYear)) +
    geom_point(
      alpha = 0.2,
      aes(
        text = paste0(
          "Date: ", as.Date(DateTime), "\n",
          "Value: ", ifelse(nonDetectFlag, "<", ""), value, " ", unit, "\n",
          "Qualifier: ", ifelse(is.na(qualifier), "none", qualifier)
        )
      )
    ) +
    geom_path(data = ~ .x %>% filter(WaterYear == input$data_year)) +
    geom_point(
      data = ~ .x |>
        dplyr::filter(WaterYear == input$data_year),
      aes(
        text = paste0(
          "Date: ", as.Date(DateTime), "\n",
          "Value: ", ifelse(nonDetectFlag, "<", ""), value, " ", unit, "\n",
          "Qualifier: ", ifelse(is.na(qualifier), "none", qualifier)
        )
      )
    ) +
    theme_bw() +
    scale_x_date(
      "",
      date_breaks = "2 months",
      date_labels = "%b",
      # limits=as.Date(c('1999-9-25','2000-10-05')),
      date_minor_breaks = "1 month"
    ) +
    scale_y_continuous(input$trend_parm)

  criteria <- switch(input$trend_parm,
    "Temperature, water" = wqc_finder(unique(dataSubset$AquaticLifeUse), input$trend_parm),
    "Dissolved Oxygen" = wqc_finder(unique(dataSubset$AquaticLifeUse), input$trend_parm),
    "pH" = c(6.5, 8.5),
    "Fecal Coliform" = c(100, 200),
    "E. coli" = c(100, 320),
    NULL
  )

  if (!is.null(criteria)) {
    dataplot <- dataplot +
      geom_hline(yintercept = criteria)
  }

  if (input$data_log_scale) {
    dataplot <- dataplot +
      scale_y_log10(
        input$trend_parm,
        breaks = 10^(-4:4),
        minor_breaks = log10_minor_break()
      )
  }

  ggplotly(dataplot, tooltip = "text")
}
