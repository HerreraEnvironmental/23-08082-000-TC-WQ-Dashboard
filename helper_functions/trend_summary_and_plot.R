## Summary of water quality trends for a selected parameter
# Requires: tidyverse, leaflet

trend_summary_func <- function(streams_wq_dat, input) {
  temp_trend_data <- streams_wq_dat %>%
    filter(
      WaterYear >= input$trend_summary_years[1] &
        WaterYear <= input$trend_summary_years[2] &
        parameter == input$trend_summary_parm #&
      #    SITE_CODE %in% input$main_site3
    )
  
  season_months <- list(
    winter = 1:3,
    spring = 4:6,
    summer = 7:9,
    fall   = 10:12
  )

  if (input$rktSeason %in% names(season_months)) {
    temp_trend_data <- temp_trend_data |>
      dplyr::filter(Month %in% season_months[[input$rktSeason]])
  }

  temp_trend_data %>%
    group_by(SITE_CODE, parameter) %>%
    nest() %>%
    mutate(
      MK_Out = map(
        .x = data,
        .f = ~ {
          mk_out <- with(
            .x,
            rkt::rkt(
              WaterYear,
              newResultValue,
              Month,
              correct = input$rktAuto,
              rep = 'a'
            )
          )
          tibble(
            p = ifelse(input$rktAuto, mk_out$sl.corrected, mk_out$sl),
            Slope = mk_out$B
          ) %>%
            mutate(
              Statement = ifelse(
                is.na(Slope),
                'Test Not Run - insufficient data',
                ifelse(
                  p > 0.05 | Slope == 0,
                  'No Significant Trend',
                  ifelse(Slope > 0, 'Increasing Trend', 'Decreasing Trend')
                )
              )
            )
        }
      )
    ) %>%
    select(-data) %>%
    unnest(MK_Out) %>%
    mutate(
      Statement = factor(
        Statement,
        levels = rev(c(
          'Decreasing Trend',
          'Test Not Run - insufficient data',
          'No Significant Trend',
          'Increasing Trend'
        ))
      ),
      StartYear = max(
        input$trend_summary_years[1],
        min(temp_trend_data$WaterYear[SITE_CODE == temp_trend_data$SITE_CODE])
      ),
      EndYear = min(
        input$trend_summary_years[2],
        max(temp_trend_data$WaterYear[SITE_CODE == temp_trend_data$SITE_CODE])
      ),
      Season = input$rktSeason,
      CorrectedForAutocorrelation = input$rktAuto
    )
}


trend_summary_map <- function(trend_summary, stream_sites, input) {
  pal_trend <- colorFactor(
    c('blue', 'darkgrey', 'lightgrey', 'red'),
    levels = c(
      'Decreasing Trend',
      'Test Not Run - insufficient data',
      'No Significant Trend',
      'Increasing Trend'
    )
  )

  trend_summary %>%
    left_join(streams_sites) %>%
    leaflet() %>%
    addCircleMarkers(
      fillColor   = ~pal_trend(Statement),
      fillOpacity = 0.9,
      weight      = 1,
      color       = "black",
      popup = ~paste0(
        "<h6><b>", SITE_NAME, "</b></h6>",
        "<br>", SITE_CODE,
        "<br>Season: ", input$rktSeason,
        "<br>Corrected for Autocorrelation? ", input$rktAuto,
        "<br>", Statement, " (p = ", round(p, 4), ")",
        "<br>", StartYear, " to ", EndYear
      ),
      layerId = ~SITE_CODE,
      label   = ~SITE_NAME
    ) %>%
    addProviderTiles('Esri.NatGeoWorldMap') %>%
    addLegend(
      pal = pal_trend,
      values = factor(c(
        'Decreasing Trend',
        'Test Not Run - insufficient data',
        'No Significant Trend',
        'Increasing Trend'
      )),
      title = 'Long-term Trend',
      opacity = 1
    )
}

trend_summary_plot <- function(trend_summary, input) {
  plot <- trend_summary %>%
    ggplot(aes(x = parameter, fill = Statement)) +
    geom_bar(position = 'stack') +
    scale_fill_manual(
      values = c(
        'Decreasing Trend' = 'blue',
        'Test Not Run - insufficient data' = 'darkgrey',
        'No Significant Trend' = 'lightgrey',
        'Increasing Trend' = 'red'
      )
    ) +
    theme_bw() +
    theme(
      #axis.text.x = element_text(angle=45,hjust=1),
      legend.position = 'bottom'
    ) +
    coord_flip() +
    xlab('')

  ggplotly(plot)
}

trend_summary_trend_plot <- function(streams_wq_dat, input) {
  temp_trend_data <- streams_wq_dat %>%
    filter(
      #WaterYear>=input$trend_summary_years[1]&WaterYear<=input$trend_summary_years[2]&
      parameter == input$trend_summary_parm &
        SITE_CODE == input$trend_summary_site
    )
  
  season_months <- list(
    winter = 1:3,
    spring = 4:6,
    summer = 7:9,
    fall   = 10:12
  )

  if (input$rktSeason %in% names(season_months)) {
    temp_trend_data <- temp_trend_data |>
      dplyr::filter(Month %in% season_months[[input$rktSeason]])
  }

  trendplot <- temp_trend_data %>%
    ggplot(aes(x = DateTime, y = value)) +
    geom_point() +
    geom_smooth(
      data = ~ filter(
        .x,
        WaterYear >= input$trend_summary_years[1] &
          WaterYear <= input$trend_summary_years[2]
      ),
      se = F
    ) +
    theme_bw() +
    scale_y_continuous(input$trend_summary_parm)

  if (input$trend_summary_log_scale) {
    trendplot <- trendplot +
      scale_y_log10(
        input$trend_summary_parm,
        breaks = 10^(-4:4),
        minor_breaks = log10_minor_break()
      )
  }

  ggplotly(trendplot)
}

## TODO: Change Display to a Map
# maybe include start year and end year to avoid confusions

#set a limit for number of samples for selected time period
#let's go for 4
#if((input$trend_summary_years[2]-input$trend_summary_years[1])<4) "Please select at least 4 years" else{