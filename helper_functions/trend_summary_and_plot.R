#summary of Water quality trends for a selected parameter
# To Do - Change Display to a Map
# maybe include start year and end year to avoid confusions

#set a limit for number of samples for selected time period
#let's go for 4
#if((input$trend_summary_years[2]-input$trend_summary_years[1])<4) "Please select at least 4 years" else{

trend_summary_func <- function(streams_wq_dat, input) {
  temp_trend_data <- streams_wq_dat %>%
    filter(
      WaterYear >= input$trend_summary_years[1] &
        WaterYear <= input$trend_summary_years[2] &
        parameter == input$trend_summary_parm #&
      #    SITE_CODE %in% input$main_site3
    )
  if (input$rktSeason == 'winter') {
    temp_trend_data <- temp_trend_data %>% filter(Month >= 1 & Month <= 3)
  }
  if (input$rktSeason == 'spring') {
    temp_trend_data <- temp_trend_data %>% filter(Month >= 4 & Month <= 6)
  }
  if (input$rktSeason == 'summer') {
    temp_trend_data <- temp_trend_data %>% filter(Month >= 7 & Month <= 9)
  }
  if (input$rktSeason == 'fall') {
    temp_trend_data <- temp_trend_data %>% filter(Month >= 10 & Month <= 12)
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

# trend_summary_func(streams_wq_dat,
#                    input=list(trend_summary_years=c(2000,2020),
#                               trend_summary_parm='Total Phosphorus',
#                               rktAuto=F,
#                               rktSeason='All',
#                               main_site3='05b'))

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
      fillColor = ~ pal_trend(Statement),
      fillOpacity = 0.9,
      weight = 1,
      color = 'black',
      popup = ~ paste0(
        "<h6>",
        "<b>",
        SITE_NAME,
        '<br>',
        "</b>",
        "</h6>",
        '<br>',
        SITE_CODE,
        '<br>',
        'Season: ',
        input$rktSeason,
        '<br>',
        'Corrected for Autocorrelation? ',
        input$rktAuto,
        '<br>',
        Statement,
        ' (p=',
        round(p, 4),
        ')',
        '<br>',
        StartYear,
        ' to ',
        EndYear
      ),
      layerId = ~SITE_CODE,
      label = ~SITE_NAME
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
# trend_summary_map(
#   trend_summary = trend_summary_func(streams_wq_dat,
#                                      input=list(trend_summary_years=c(2000,2020),
#                                                 trend_summary_parm='Total Phosphorus',
#                                                 rktAuto=F,
#                                                 rktSeason='All',
#                                                 main_site3='05b')),
#   stream_sites
# )

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
# trend_summary_plot(
#   trend_summary = trend_summary_func(streams_wq_dat,
#                                      input=list(trend_summary_years=c(2000,2020),
#                                                 trend_summary_parm='Total Phosphorus',
#                                                 rktAuto=F,
#                                                 rktSeason='All',
#                                                 main_site3='05b'))
# )

trend_summary_trend_plot <- function(streams_wq_dat, input) {
  temp_trend_data <- streams_wq_dat %>%
    filter(
      #WaterYear>=input$trend_summary_years[1]&WaterYear<=input$trend_summary_years[2]&
      parameter == input$trend_summary_parm &
        SITE_CODE == input$trend_summary_site
    )
  if (input$rktSeason == 'winter') {
    temp_trend_data <- temp_trend_data %>% filter(Month >= 1 & Month <= 3)
  }
  if (input$rktSeason == 'spring') {
    temp_trend_data <- temp_trend_data %>% filter(Month >= 4 & Month <= 6)
  }
  if (input$rktSeason == 'summer') {
    temp_trend_data <- temp_trend_data %>% filter(Month >= 7 & Month <= 9)
  }
  if (input$rktSeason == 'fall') {
    temp_trend_data <- temp_trend_data %>% filter(Month >= 10 & Month <= 12)
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
