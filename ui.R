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

# User Interface ----------------------------------------------------------
sites_list<-readRDS('outputs/sites_list.RDS')
parm_list<-readRDS('outputs/parm_list.RDS')
years_list<-readRDS('outputs/years_list.RDS')

ui<-tagList(
    tags$head(tags$link(includeScript("func.js"))),
    tags$head(tags$style("a{cursor:pointer;}")),
    
    #UI
    navbarPage(
      theme = bs_theme(version = 4, bootswatch = "yeti"),
      paste0('Thurston County Streams Water Quality Data Dashboard - BETA (', Sys.Date(),')'),
      tabPanel('Map',value='map',
               fluidRow(column(12,h1("Water Quality Station Map"))),
               fluidRow(column(12, hr())),
               fluidRow(column(8,h5("Below are all stations, active and inactive, used to monitor water quality in Thurston County. To learn more about a station, click on the icon and follow the prompts to various data tabs."))),
               fluidRow(column(12, br())),
               fluidRow(column(12,leafletOutput('map',height=700,width=1600))),
               fluidRow(column(12, br())),
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
      tabPanel('Summary of Water Quality Index',value='sum_wqi',
               column(12,h1("Summary of Water Quality Index")),
               column(12, hr()),
               fluidRow(column(8,leafletOutput('wqi_map',height=800,width=1200)),
                        column(4, 
                               selectInput('wqi_sum_year','Select Year to Highlight',years_list),
                               plotlyOutput('wqi_summary_plot')
                        )),
               fluidRow(column(12, br()))
      ),
      tabPanel('Summary of Water Quality Criteria',value='sum_wqc',
               column(12,h1("Summary of Water Quality Criteria")),
               column(12, hr()),
               fluidRow(column(8,leafletOutput('wqc_map',height=800,width=1200)),
                        column(4,
                               selectInput('wqc_sum_year','Select Year to Highlight',years_list),
                               # plotlyOutput('wqc_summary_plot'),
                               tableOutput('wqc_summary')
                        )),
               fluidRow(column(12, br()))
      ),
      
      tabPanel('Summary of Water Quality Trends',value='trends',
               column(12,h1("Water Quality Trends")),
               column(12, hr()),
               sidebarLayout(
                 sidebarPanel(width = 3,
                              
                              pickerInput('main_site3','Select Site',sites_list, multiple = T,
                                          selected=sites_list[1:3],
                                          options = pickerOptions(
                                            actionsBox = TRUE, 
                                            size = 10,
                                            selectedTextFormat = "count > 3"
                                          )),
                              selectInput('trend_summary_parm','Select Parameter for Table and Plot',
                                          parm_list),
                              sliderInput('trend_summary_years','Select Year Range for Trend',
                                          value=c(min(years_list),max(years_list)),
                                          min=min(years_list),max=max(years_list),
                                          step=1,sep=''),
                              materialSwitch(inputId = "trend_summary_log_scale", label = "Log-scale?", status = "default",value=F),
                              downloadButton('trends_download',label='Download Trend Statistics')
                 ),
                 mainPanel(width = 9,
                           mainPanel(plotlyOutput('trend_summary_plot'),
                                     #     tableOutput('trend_summary_table'),
                                     plotlyOutput('trend_summary_parm_plot'))
                 )),
               fluidRow(column(12, br()))
      ), 
      tabPanel('WQI',value='wqi',
               column(12,h1("Water Quality Index")),
               column(12, hr()),
               fluidRow(column(12, "The Water Quality Index (WQI) is a quantitative means
                               of assessing the relative health of a water body. 
                               It provides a single number that expresses overall water quality
                               at a given location and time. Parameters include but are not 
                               limited to temperature, turbidity, pH, dissolved oxygen, nutrient levels, 
                               and bioindicators. Each parameter is given a weight based on its 
                               importance for water quality. The final WQI is calculated by 
                               aggregating the individual index scores, and the resulting score 
                               usually falls on a scale from 0 to 100, with higher values indicating 
                               better water quality.")),
               column(12, hr()),
               fluidRow(column(12,sidebarLayout(
                 sidebarPanel(width=3,
                              selectInput('main_site','Select Site',sites_list),
                              selectInput('wqi_year','Select Year to Highlight',years_list),
                              sliderInput('wqi_trend_years','Select Year Range for Trend',value=c(min(years_list),max(years_list)),
                                          min=min(years_list),max=max(years_list),
                                          step=1,sep='')
                 )
                 ,
                 mainPanel(width=9,
                           fluidRow(plotlyOutput('wqi_monthly')),
                           fluidRow(plotlyOutput('wqi_annual'))
                           
                 ))
                 
               )),
               fluidRow(column(12, br()))
      ),
      
      tabPanel('Data Visualization and Trends',value='all_data',
               column(12,h1("All Data Viewer")),
               column(12, hr()),
               sidebarLayout(
                 sidebarPanel(width = 3,
                              pickerInput('main_site2','Select Site',sites_list, multiple = F),
                              selectInput('data_year','Select Year to Highlight',years_list),
                              selectInput('trend_parm','Select Parameter',parm_list),
                              sliderInput('trend_years','Select Year Range for Trend',value=c(2000,2020),
                                          min=2000,max=2020,
                                          step=1,sep=''),
                              materialSwitch(inputId = "data_log_scale", label = "Log-scale?", status = "default",value=F),
                              hr(),
                              h2('Water Quality Criteria Comparison for Selected Year'),
                              tableOutput('wqc_site')
                   
                 ),
                 mainPanel(width = 9,
                           plotlyOutput('data_plot'),
                           textOutput('trend_text'),
                           plotlyOutput('trend_plot')
                 )),
               fluidRow(column(12, br()))
      ),
      
      tabPanel('Data Download', value = 'data_download',
               column(12,h1("Data Download")),
               column(12, hr()),
               sidebarLayout(
                 sidebarPanel(width = 3,
                              pickerInput('main_site4','Select Site to Download',sites_list, multiple = T,
                                          options = pickerOptions(
                                            actionsBox = TRUE, 
                                            size = 10,
                                            selectedTextFormat = "count > 3"
                                          )),
                              pickerInput('params_out', "Select Parameter(s)", parm_list, multiple = TRUE,
                                          options = pickerOptions(
                                            actionsBox = TRUE, 
                                            size = 10,
                                            selectedTextFormat = "count > 3"
                                          )),
                              sliderInput('years_out','Select Year Range for Download', 
                                          value=c(min(years_list),max(years_list)),
                                          min=min(years_list),max=max(years_list),
                                          step=1,sep=''),
                              downloadButton('downloadData', "Download Data")
                 ),
                 mainPanel(width = 9,
                           h4("Data Preview"),
                           br(),
                           DTOutput('data_view_table')
                 )),
               fluidRow(column(12, br()))
      ),
      tabPanel('Disclaimer', value='disclaimer',
               column(12,h1("Disclaimer")),
               column(12, hr()),
               fluidRow(column(4, "This is sample text"
                        )),
               fluidRow(column(12, br()))
      ),
      
      id='navbarpanel')
  )
