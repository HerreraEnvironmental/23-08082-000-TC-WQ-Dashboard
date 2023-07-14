library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(shiny)
library(leaflet)
library(bslib)
library(DT)
library(shinyWidgets)
library(purrr)
library(tidyr)
library(rkt)


log10_minor_break = function (...){
  function(x) {
    minx         = floor(min(log10(x), na.rm=T))-1;
    maxx         = ceiling(max(log10(x), na.rm=T))+1;
    n_major      = maxx-minx+1;
    major_breaks = seq(minx, maxx, by=1)
    minor_breaks = 
      rep(log10(seq(1, 9, by=1)), times = n_major)+
      rep(major_breaks, each = 9)
    return(10^(minor_breaks))
  }
}
# Data Load ---------------------------------------------------------------
source('functions/wqc_functions.R',local=T)
source('functions/site_map.R',local=T)
source('functions/trend_summary_and_plot.R',local=T)
source('functions/wqc_map.R',local=T)
source('functions/wqc_site.R',local=T)
source('functions/trend_shiny_functions.R',local=T)
source('functions/withinYear_plot.R',local=T)
source('functions/wqi_site_plot.R',local=T)
source('functions/wqi_map.R',local=T)

streams_wq_dat<-readRDS('outputs/streams_wq_dat.RDS')
streams_sites<-readRDS('outputs/streams_sites.RDS')
annual_wqi<-readRDS('outputs/annual_wqi.RDS') %>%
  mutate(Rating=ifelse(WQI>=80,'Good',
                       ifelse(WQI>=40,'Moderate','Poor')))


monthly_wqi_by_parameter<-readRDS('outputs/monthly_wqi_by_parameter.RDS')
monthly_wqi<-readRDS('outputs/monthly_wqi.RDS')

sites_list<-readRDS('outputs/sites_list.RDS')
parm_list<-readRDS('outputs/parm_list.RDS')
years_list<-readRDS('outputs/years_list.RDS')

sites_list_df <- streams_sites[,c(2,3)]
streams_wq_dat <- merge(streams_wq_dat, sites_list_df, by="SITE_CODE")

#For recent stream data (sample time and WQI score)

recent_streams_data<-streams_wq_dat %>%
  group_by(SITE_CODE) %>%
  slice(which.max(as.Date(ymd_hms(DateTime)))) %>%
  select(SITE_CODE,DateTime) %>%
  left_join(.,
            annual_wqi%>%
              slice(which.max(WaterYear)),
            by=c('SITE_CODE'='site')
  ) %>%
  left_join(streams_sites) %>%
  arrange(desc(DateTime))



server<-function(input,output,session){
  #OPENER TAB
  output$map<-renderLeaflet({
    site_map(recent_streams_data,input)
  })
  #WQI MAP TAB
  output$wqi_map<-renderLeaflet({
    wqi_map(streams_sites,annual_wqi,input)
  })
  output$wqi_summary_plot<-renderPlotly({
    wqi_summary_plot(annual_wqi,input)
  })
  
  
  #TRENDS TAB
  trend_summary<-reactive({
    trend_summary_func(streams_wq_dat,input)
  })
  output$trend_summary_map<-renderLeaflet({
    trend_summary_map(trend_summary(),streams_sites,input)
  })
  output$trend_summary_trend_plot<-renderPlotly({
    trend_summary_trend_plot(streams_wq_dat,input)
  })
  output$trend_summary_plot<-renderPlotly({
    trend_summary_plot(trend_summary(),input)
  })
  
  observeEvent(input$trend_summary_map_marker_click, {
    p <- input$trend_summary_map_marker_click
    if(!is.null(p$id)){
    updateSelectInput(session, "trend_summary_site", 
                      selected =p$id)
    }
  })
  
  output$trend_summary_table<-renderText({
    paste('')
    
  })
  
  output$trends_download <- downloadHandler(
    filename = function() {
      paste('trends-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(trend_summary() %>% select(SITE_CODE,parameter,StartYear,EndYear,Season,CorrectedForAutocorrelation,
                                           `p-value`=p,`Slope (units/years)`=Slope),
                con,row.names = F)
    }
  )
  
  #Water Quality Criteria MAP
  
  wqc_map_out<-reactive({
    wqc_comparison(streams_wq_dat,input)
    
  })
  
  output$wqc_map<-renderLeaflet({
    wqc_map(wqc_map_out(),stream_sites,input)
  })
  
  output$wqc_summary<-renderTable({
    wqc_table(wqc_map_out(),input)
  })
  
  #Individual  Site Water Quality 
  output$wqc_site<-renderTable({
    wqc_site(streams_wq_dat,input)
  })
  
  dataSubset<-reactive({
    streams_wq_dat %>%
      filter(SITE_CODE==input$main_site&
               parameter==input$trend_parm)%>%
      mutate(AquaticLifeUse='Core Summer Salmonid Habitat') ### need to pull from lookup table
  })
  
  output$data_plot<-renderPlotly({
    withinYear_plot(dataSubset(),input)
  })
  
  output$trend_plot<-renderPlotly({
    trend_plot(dataSubset(),input)
  })
  
  output$trend_text<-renderUI({
    trend_text(dataSubset(),input)
  })
  

  
  #Individual WQI Plot

  output$wqi_annual<-renderPlotly({
    ggplotly(wqi_annual_plot(annual_wqi,input),
             source='wqi_year_select') %>% event_register("plotly_click")
  })
  
  output$wqi_monthly<-renderPlotly({
    monthly_wqi_plot(monthly_wqi_by_parameter,input)
  })
  
  
  observeEvent(event_data("plotly_click",source='wqi_year_select'), {
    s <- event_data("plotly_click", source = "wqi_year_select")
    updateSelectInput(session, "wqi_sum_year", 
                      selected =s)
    updateSelectInput(session, "wqi_year", 
                      selected =s)
  })
  
  #DAta Download TAb
  dataout_data<-reactive({
    streams_wq_dat %>%
      filter(SITE_CODE %in% input$main_site4&
               parameter==input$params_out&
               WaterYear>=input$years_out[1]&WaterYear<=input$years_out[2])%>% 
      select(SITE_CODE,SITE_NAME,DateTime,parameter,value,unit,mdl)
  })
  
  output$data_view_table <- renderDT({
    if(is.null(input$params_out)){
    } else{
      datatable(head(dataout_data(),20), 
                escape = FALSE,
                options = list(
                  scrollX = TRUE,
                  dom = 't',
                  autoWidth = TRUE
                ),
                rownames= FALSE)
    }
  })
  
  
  output$downloadData <- downloadHandler(
    
    filename = function() { 
      paste("ThurstonCoWQData_", as.character(Sys.Date()), ".csv", sep="")
    },
    content = function(file) {
      write.csv(dataout_data() ,
                file, row.names = F)
    }
  )  
  
  
  
  ##updates for tabs
  observe({
    updateSelectInput(session,
                      'main_site',
                      selected = input$main_site
    )
  })
  
  observe({
    updateSelectInput(session,
                      'main_site2',
                      selected = input$main_site
    )
  })
  
  observe({
    updateSliderInput(session,
                      'trend_years',
                      min=min(dataSubset()$Year),
                      max=max(dataSubset()$Year),
                      value=c(min(dataSubset()$Year),
                              max(dataSubset()$Year))
    )
  })
  observe({
    updateSelectInput(session,
                      'trend_parm',
                      choices=parm_list[
                        parm_list %in% (streams_wq_dat %>% filter(SITE_CODE==input$main_site) %>% pull(parameter) %>% unique())]
    )
  })
  observe({
    updateSelectInput(session,
                      'data_year2',
                      choices=streams_wq_dat %>% filter(SITE_CODE==input$main_site) %>% pull(WaterYear) %>% unique()
    )
  })
  
  observe({
    updateSelectInput(session,
                      'data_year',
                      choices = sort(unique(dataSubset()$WaterYear),T)
    )
  })
  
  
  observe({
    updateSelectInput(session,
                      'data_parm',
                      choices=parm_list[
                        parm_list %in% (streams_wq_dat %>% filter(SITE_CODE==input$main_site) %>% pull(parameter) %>% unique())]
    )
  })
  # MAP 1 updates all variables
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    updateSelectInput(session, "main_site", 
                      selected = click$id)
    updateSelectInput(session, "main_site2", 
                      selected = click$id)
    updateSelectInput(session, "main_site3", 
                      selected = click$id)
    updateSelectInput(session, "main_site4", 
                      selected = click$id)
    updateSelectInput(session, "main_site5", 
                      selected = click$id)
  })
  # MAP 2 updates all variables
  observeEvent(input$wqi_map_marker_click, {
    click <- input$wqi_map_marker_click
    updateSelectInput(session, "main_site", 
                      selected = click$id)
    updateSelectInput(session, "main_site2", 
                      selected = click$id)
    updateSelectInput(session, "main_site3", 
                      selected = click$id)
    updateSelectInput(session, "main_site4", 
                      selected = click$id)
    updateSelectInput(session, "main_site5", 
                      selected = click$id)
  })
  # Dropdown 1 updates all variables
  observeEvent(input$main_site, {
    updateSelectInput(session, "main_site2",
                      selected = input$main_site)
    updateSelectInput(session, "main_site3",
                      selected = input$main_site)
    updateSelectInput(session, "main_site4",
                      selected = input$main_site)
    updateSelectInput(session, "main_site5",
                      selected = input$main_site)
  })
  # Dropdown 2 updates all variables
  observeEvent(input$main_site2, {
    updateSelectInput(session, "main_site",
                      selected = input$main_site2)
    updateSelectInput(session, "main_site3",
                      selected = input$main_site2)
    updateSelectInput(session, "main_site4",
                      selected = input$main_site2)
    updateSelectInput(session, "main_site5",
                      selected = input$main_site2)
  })
  
}