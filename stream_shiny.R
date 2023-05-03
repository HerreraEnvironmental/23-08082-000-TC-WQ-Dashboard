library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(shiny)
library(leaflet)
#Develop shiny tools for stream data viz
#basic ui
#Tab 1 - MAP - Display all sites, select site (nice if would update select on other tabs,popup feeds you to other tabs)
#TAB 2 - Trends - Select by Site, parameter, change start and end date for trends (show all data, just grey out not select)
###produce automated text for "There is a X% likliehood that Y is decreasing
#Tab 3 - Data Deep Dive (water year plots) - select site, water year, parameter

streams_wq_dat<-readRDS('outputs/streams_wq_dat.RDS')
streams_sites<-readRDS('outputs/streams_sites.RDS')


sites_list<-setNames(streams_sites$SITE_CODE,streams_sites$SITE_NAME)
parm_list<-unique(streams_wq_dat$parameter)

ui<-tagList(
  # tags$head(tags$script(HTML('
  #       var fakeClick = function(tabName) {
  #         var dropdownList = document.getElementsByTagName("a");
  #         for (var i = 0; i < dropdownList.length; i++) {
  #           var link = dropdownList[i];
  #           if(link.getAttribute("data-value") == tabName) {
  #             link.click();
  #           };
  #         }
  #       };
  #     '))),
   # tags$head(tags$link(includeScript("func.js"))),
   # tags$head(tags$style("a{cursor:pointer;}")),
#UI
  navbarPage(
  'Thurston County Streams Water Quality Data Dashboard',
  tabPanel('Map',value='map',
           column(6,leafletOutput('map',height=800,width=800)),
           column(6,sidebarLayout(
             sidebarPanel(width=6,
                  selectInput('trend_site','Select Site',sites_list),
                  selectInput('trend_parm','Select Parameter',parm_list)
                  ),
             mainPanel(width=6,
                  selectInput('data_year','Select Year to Highlight',2000),
                  sliderInput('trend_years','Select Year Range for Trend',value=c(2000,2020),
                                           min=2000,max=2020,
                                             step=1,sep='')
                  )),
                  fluidRow(plotlyOutput('trend_plot')),
                  
                  fluidRow(plotlyOutput('data_plot'))
                  ))
  #,
#   tabPanel('WQ Trends',value='trends',
#            sidebarLayout(
#              sidebarPanel(
#                selectInput('trend_site','Select Site',sites_list),
#                selectInput('trend_parm','Select Parameter',parm_list),
#                sliderInput('trend_years','Select Year Range for Trend',value=c(2000,2020),
#                           min=2000,max=2020,
#                           step=1)
#            ),
#            mainPanel(
#                plotlyOutput('trend_plot'),
#                textOutput('trend_text')
#            ))
#   ),
#   tabPanel('Detailed Data',value='data_tab',
#            sidebarLayout(
#              sidebarPanel(
#                selectInput('data_site','Select Site',sites_list),
#                selectInput('data_parm','Select Parameter',parm_list),
#                selectInput('data_year','Select Year to Highlight',2000)
#              ),
#              mainPanel(
#                plotlyOutput('data_plot')
#              ))
#            )
 ,id='navbarpanel')
)

server<-function(input,output,session){
  
  output$map<-renderLeaflet({
    leaflet(streams_sites) %>%
      addMarkers(popup=~paste0(SITE_CODE,'<br>',SITE_NAME,'<br>'#,
                               #"<a onclick=","customHref('trends')>",'Trends','</a>'
                                        ),
                 layerId= ~SITE_CODE,
                 label = ~SITE_CODE) %>%
      addProviderTiles('Esri.NatGeoWorldMap')
  })
  
  dataSubset<-reactive({
    streams_wq_dat %>%
      filter(SITE_CODE==input$trend_site&
               parameter==input$trend_parm)
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
                      choices=streams_wq_dat %>% filter(SITE_CODE==input$trend_site) %>% pull(parameter) %>% unique()
    )
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    updateSelectInput(session, "trend_site", 
                      selected = click$id)
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
          'there is a ',pos_lik*100,'% likelihood that',input$trend_parm,'is increasing at',input$trend_site,"\n",
          'Between water years',input$trend_years[1],'and',input$trend_years[2],
          'there is a ',neg_lik*100,'% likelihood that',input$trend_parm,'is decreasing at',input$trend_site)
  })
  
  
  ##data plots
  # dataSubset_data<-reactive({
  #   streams_wq_dat %>%
  #     filter(SITE_CODE==input$data_site&
  #              parameter==input$`data_parm`)
  # })
  
  observe({
    updateSelectInput(session,
                      'data_year',
                      choices = sort(unique(dataSubset()$WaterYear),T)
    )
  })
  
  # observe({
  #   updateSelectInput(session,
  #                     'data_parm',
  #                     choices=streams_wq_dat %>% filter(SITE_CODE==input$trend_site) %>% pull(parameter) %>% unique()
  #   )
  # })
  
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
}

shinyApp(ui,server)
