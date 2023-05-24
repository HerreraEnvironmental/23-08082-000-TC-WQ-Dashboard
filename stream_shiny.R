library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(shiny)
library(leaflet)
library(bslib)
#Develop shiny tools for stream data viz
#basic ui
#Tab 1 - MAP - Display all sites, select site (nice if would update select on other tabs,popup feeds you to other tabs)
#TAB 2 - Trends - Select by Site, parameter, change start and end date for trends (show all data, just grey out not select)
###produce automated text for "There is a X% likliehood that Y is decreasing
#Tab 3 - Data Deep Dive (water year plots) - select site, water year, parameter

streams_wq_dat<-readRDS('outputs/streams_wq_dat.RDS')
streams_sites<-readRDS('outputs/streams_sites.RDS')
annual_wqi<-readRDS('outputs/annual_wqi.RDS')
monthly_wqi_by_parameter<-readRDS('outputs/monthly_wqi_by_parameter.RDS')
monthly_wqi<-readRDS('outputs/monthly_wqi.RDS')

sites_list<-setNames(streams_sites$SITE_CODE,streams_sites$SITE_NAME)
parm_list<-unique(streams_wq_dat$parameter)

sites_list_df <- streams_sites[,c(2,3)]
streams_wq_dat <- merge(streams_wq_dat, sites_list_df, by="SITE_CODE")


ui<-
  tagList(
  tags$head(tags$link(includeScript("func.js"))),
  tags$head(tags$style("a{cursor:pointer;}")),
 
#UI
  navbarPage(
    theme = bs_theme(version = 4, bootswatch = "yeti"),
  paste0('Thurston County Streams Water Quality Data Dashboard - BETA (', Sys.Date(),')'),
  tabPanel('Map',value='map',
           fluidRow(column(12,h1("Water Quality Station Map"))),
           fluidRow(column(12, hr())),
           fluidRow(column(8,h4("Below are all stations, active and inactive, used to monitor water quality in Thurston County. To learn more about a station, click on the icon and follow the prompts to various data tabs."))),
           fluidRow(column(12,leafletOutput('map',height="800",width="1200"))),
           #column(1,
          #        sidebarLayout(
          #   sidebarPanel(width=1,
          #        selectInput('trend_site','Select Site',sites_list),
          #        selectInput('trend_parm','Select Parameter',parm_list)
          #        ),
          #  mainPanel(width=1
            #      selectInput('data_year','Select Year to Highlight',2000),
            #      sliderInput('trend_years','Select Year Range for Trend',value=c(2000,2020),
            #                               min=2000,max=2020,
            #                                 step=1,sep='')
                  #))
                  #fluidRow(plotlyOutput('trend_plot')),
                  
                 #fluidRow(plotlyOutput('data_plot'))
                  ),
  tabPanel('Water Quality Index',value='wqi',
           column(12,h1("Water Quality Index")),
           column(12, hr()),
           fluidRow(column(6,leafletOutput('wqi_map',height=800,width=800)),
                    column(6,sidebarLayout(
                     sidebarPanel(width=6,
                                  selectInput('main_site','Select Site',sites_list)
                     )
                     ,
                     mainPanel(width=6,
                               selectInput('wqi_year','Select Year to Highlight',sort(unique(annual_wqi$WaterYear),T)),
                               sliderInput('wqi_trend_years','Select Year Range for Trend',value=c(min(annual_wqi$WaterYear),max(annual_wqi$WaterYear)),
                                           min=min(annual_wqi$WaterYear),max=max(annual_wqi$WaterYear),
                                           step=1,sep='')
                     )),
                     fluidRow(plotlyOutput('wqi_annual')),
                     fluidRow(plotlyOutput('wqi_monthly'))
                     ))
           ),
  
  tabPanel('Water Quality Trends',value='trends',
           column(12,h1("Water Quality Trends")),
           column(12, hr()),
            sidebarLayout(
              sidebarPanel(width = 3,
                selectInput('main_site2','Select Site',sites_list),
                selectInput('trend_parm','Select Parameter',parm_list),
                sliderInput('trend_years','Select Year Range for Trend',value=c(2000,2020),
                           min=2000,max=2020,
                           step=1)
            ),
            mainPanel(width = 9,
                plotlyOutput('trend_plot'),
                textOutput('trend_text')
            ))
  ), 
  tabPanel('Detailed Data',value='data_tab',
           column(12,h1("Detailed Water Quality Data")),
           column(12, hr()),
            sidebarLayout(
              sidebarPanel(width = 3,
               selectInput('main_site3','Select Site',sites_list),
                selectInput('data_parm','Select Parameter',parm_list),
                selectInput('data_year','Select Year to Highlight',2000)
              ),
              mainPanel(width = 9,
                plotlyOutput('data_plot')
              ))
  ),
  tabPanel('All Data', value = 'all_data',
           column(12,h1("All Data Viewer")),
           column(12, hr())),
  tabPanel('Data Download', value = 'data_download',
           column(12,h1("Data Download")),
           column(12, hr()),
           sidebarLayout(
             sidebarPanel(width = 3,
                          selectInput('main_site4','Select Site to Download',sites_list),
                          selectInput('data_type', "Select Data Type for Download", c("Annual WQI",
                                                                                      "Monthly WQI",
                                                                                      "All Water Quality Data"))
             ),
             mainPanel(width = 9,
                      h4("Data Preview"),
                      tableOutput('data_view_table')
             ))
  )
  
 ,id='navbarpanel')
)

server<-function(input,output,session){
  
  output$map<-renderLeaflet({
    leaflet(streams_sites) %>%
      addMarkers(popup=~paste0("<h5>", "<b>", SITE_NAME,'<br>', "</b>","</h5>",
                               "<h6>", "<i>", SITE_CODE,'<br>', "</i>","</h6>",
                               "<br>",
                               "Last Sample Collected: ", "NA", "<br>",
                               "Recent WQI Score:      ", "NA", "<br>",
                               "<br>",
                               "<a onclick=","customHref('wqi')>",'View recent Water Quality Data.','</a>', "<br>",
                               "<a onclick=","customHref('trends')>",'View Water Quality trends.','</a>', "<br>",
                               "To view all data from this station, ", "<a onclick=","customHref('all_data')>",'click here.','</a>', "<br>",
                               "To download data from this station, ", "<a onclick=","customHref('data_download')>",'click here.','</a>', "<br>"
                                        ),
                 layerId= ~SITE_CODE,
                 label = ~SITE_CODE) %>%
      addProviderTiles('Esri.NatGeoWorldMap')
  })
  
  output$wqi_map<-renderLeaflet({
    
    pal<-colorFactor(c('green','yellow','red','grey'),levels=c('Good',"Moderate",'Poor',NA))
    
    streams_sites %>%
      left_join(annual_wqi %>%filter(WaterYear==input$wqi_year),by=c('SITE_CODE'='site')) %>%
      mutate(Category=ifelse(WQI>=80,'Good',ifelse(WQI>=40,"Moderate",'Poor'))) %>%
    leaflet() %>%
      addCircleMarkers(color=~pal(Category),fillOpacity = 0.9,weight=1,
      layerId= ~SITE_CODE,
      label = ~SITE_CODE) %>%
      addProviderTiles('Esri.NatGeoWorldMap') 
  })
  
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
                      choices=streams_wq_dat %>% filter(SITE_CODE==input$main_site) %>% pull(parameter) %>% unique()
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
  })
  # Dropdown 1 updates all variables
  observeEvent(input$main_site, {
    updateSelectInput(session, "main_site2",
                      selected = input$main_site)
    updateSelectInput(session, "main_site3",
                      selected = input$main_site)
  })
  # Dropdown 2 updates all variables
  observeEvent(input$main_site2, {
    updateSelectInput(session, "main_site",
                      selected = input$main_site2)
    updateSelectInput(session, "main_site3",
                      selected = input$main_site2)
  })
  # Dropdown 3 updates all variables
  observeEvent(input$main_site3, {
    updateSelectInput(session, "main_site",
                      selected = input$main_site3)
    updateSelectInput(session, "main_site2",
                      selected = input$main_site3)
  })

  
  dataSubset<-reactive({
    streams_wq_dat %>%
      filter(SITE_CODE==input$main_site&
               parameter==input$trend_parm)
  })
  
  
  output$trend_plot<-renderPlotly({
    trendplot<-dataSubset() %>%
      ggplot(aes(x=DateTime,y=value))+
      geom_point(data=~filter(.x,WaterYear==input$data_year),col='red',size=4)+
      geom_point()+
      geom_smooth(data=~filter(.x,WaterYear>=input$trend_years[1]&WaterYear<=input$trend_years[2]))+
      theme_bw()
    ggplotly(trendplot)
  })
  output$trend_text<-renderText({
    
    pos_lik=.65
    neg_lik=.35
    
    
    paste('Between water years',input$trend_years[1],'and',input$trend_years[2],
          'there is a ',pos_lik*100,'% likelihood that',input$trend_parm,'is increasing at',input$main_site,"\n",
          'Between water years',input$trend_years[1],'and',input$trend_years[2],
          'there is a ',neg_lik*100,'% likelihood that',input$trend_parm,'is decreasing at',input$main_site)
  })
  
  ##data plots
  dataSubset_data<-reactive({
     streams_wq_dat %>%
       filter(SITE_CODE==input$main_site&
                parameter==input$`data_parm`)
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
                       choices=streams_wq_dat %>% filter(SITE_CODE==input$main_site) %>% pull(parameter) %>% unique()
     )
  })
  
  output$data_plot<-renderPlotly({
    dataplot<-dataSubset() %>%
      ggplot(aes(x=WY_FakeDate,y=value,group=WaterYear))+
      geom_point(alpha=.2)+
      geom_line(data=~.x %>% filter(WaterYear==input$data_year))+
      geom_point(data=~.x %>% filter(WaterYear==input$data_year))+
      theme_bw()+
      scale_x_date('',date_breaks = '2 months',date_labels = '%b',limits=as.Date(c('1999-9-25','2000-10-05')),
                   date_minor_breaks = '1 month')+
      scale_y_continuous(input$trend_parm)
    ggplotly(dataplot)
  })
  
  output$wqi_annual<-renderPlotly({
    wqi_annual_plot<-annual_wqi %>%
      filter(site==input$main_site) %>%
      ggplot(aes(x=WaterYear,y=WQI))+
      geom_point(data=~filter(.x,WaterYear==input$wqi_year),col='red',size=4)+
      geom_point()+
      geom_smooth(data=~filter(.x,WaterYear>=input$wqi_trend_years[1]&WaterYear<=input$wqi_trend_years[2]))+
      theme_bw()+
      xlab('Water year')+
      scale_y_continuous('Water Quality Index',limits = c(0,100))
    ggplotly(wqi_annual_plot,source='wqi_year_select') %>% event_register("plotly_click")
  })
  
  observeEvent(event_data("plotly_click",source='wqi_year_select'), {
    s <- event_data("plotly_click", source = "wqi_year_select")
    updateSelectInput(session, "wqi_year", 
                      selected =s)
  })
  
  output$wqi_monthly<-renderPlotly({
    monthly_wqi_plot<-monthly_wqi_by_parameter %>%
      filter(site==input$main_site&WaterYear==input$wqi_year) %>%
      tidyr::pivot_longer(cols=c(FC,Oxygen,pH,Temp,Sediment,Nutrient),names_to = 'shortParmName',values_to = 'WQI') %>%
      select(site,WaterYear,Month,shortParmName,WQI) %>%
      mutate(Month=factor(Month,c(10:12,1:9),labels=month.abb[c(10:12,1:9)])) %>%
      #ggplot()+
      ggplot(aes(x=Month,y=WQI,col=shortParmName))+
       geom_line(alpha=.5,aes(group=shortParmName))+
       geom_point(alpha=.5)+
      geom_line(data=filter(monthly_wqi,site==input$main_site&WaterYear==input$wqi_year)%>%
                  mutate(Month=factor(Month,c(10:12,1:9),labels=month.abb[c(10:12,1:9)])) ,
                aes(x=Month,y=WQI),group=1,
                col='black')+
      geom_point(data=filter(monthly_wqi,site==input$main_site&WaterYear==input$wqi_year)%>%
                   mutate(Month=factor(Month,c(10:12,1:9),labels=month.abb[c(10:12,1:9)])) ,
                 aes(x=Month,y=WQI),
                 col='black')+
      theme_bw()+
      xlab('Month')+
      scale_y_continuous('Water Quality Index',limits = c(0,100))
    ggplotly(monthly_wqi_plot)
  })
  
  output$data_view_table <- renderTable({
    if(input$data_type == "Annual WQI"){
      my_output <- head(annual_wqi[annual_wqi$site == input$main_site,])
    }
    if(input$data_type == "Monthly WQI"){
      my_output <- head(monthly_wqi[monthly_wqi$site == input$main_site,])
    }
    if(input$data_type == "All Water Quality Data"){
      my_output <- head(streams_wq_dat %>%
                      filter(SITE_CODE == input$main_site))
    }
    my_output
  })
}

  
  


shinyApp(ui,server)




















