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
library(shinydashboard)
library(fresh)
library(shiny.router)
library(shinyjs)
library(shinyBS)
library(rintrojs)


# User Interface ----------------------------------------------------------
sites_list<-readRDS('outputs/sites_list.RDS')
parm_list<-readRDS('outputs/parm_list.RDS')
years_list<-readRDS('outputs/years_list.RDS')

tagList(
   # tags$head(tags$link(includeScript("func.js"))),
    tags$head(tags$style("a{cursor:pointer;}")))


ui<-
  dashboardPage(  
    #title = "Thurston County",
    title = "Thurston County Water Quality",
    dashboardHeader(
      title = img(src = "TC2.png", height = 120),
      #itle = "Thurston County Water Quality",
      tags$li(class = "dropdown",
              tags$style(".main-header {max-height: 135px}"),
              tags$style(".main-header .logo {height: 135px}")
      ),
      tags$li(
          a(
            strong("ABOUT"),
            height = 50,
            href = "https://github.com/HerreraEnvironmental/23-08082-000-TC-WQ-Dashboard",
            title = "",
            target = "_blank"
          ),
          class = "dropdown"
      )
    ),
    dashboardSidebar(
      tags$style(".left-side, .main-sidebar {padding-top: 135px}"),
      tags$style(HTML(".sidebar-menu li a { font-size: 16px; }")),
      width = 300,
      sidebarMenu(
        menuItem(
                "Map",
                 tabName = "map",
                 icon = icon("map-marker", lib = "glyphicon")),
        menuItem(text = 'Summary of Water Quality Index',
                 tabName = 'sum_wqi',
                 icon = icon("tint", lib = "glyphicon")),
        menuItem(text = 'Summary of Water Quality Criteria',
                 tabName = "sum_wqc",
                 icon = icon("tint", lib = "glyphicon")),
        menuItem(text = 'Summary of Water Quality Trends',
                 tabName = "trends",
                 icon = icon("tint", lib = "glyphicon")),
        menuItem(text = "WQI",
                 tabName = "wqi",
                 icon = icon("tint", lib = "glyphicon")),
        menuItem(text = 'Data Visualization and Trends',
                 tabName = "all_data",
                 icon = icon("signal", lib = "glyphicon")),
        menuItem(text = 'Data Download',
                 tabName = "data_download",
                 icon = icon("download-alt", lib = "glyphicon")),
        menuItem(text = 'Disclaimer',
                 tabName = "disclaimer",
                 icon = icon("info-sign", lib = "glyphicon"))
        
      )
    ),
    dashboardBody(
      tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
     tags$head(
        tags$link(
          rel = "stylesheet", 
          type = "text/css", 
          href = "radar_style.css")
      ),
     includeCSS("www/radar_style.css"), # Link to style sheet
      tags$head(tags$style(HTML(
        '.myClass { 
        font-size: 32px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
      tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Thurston County Water Quality Dashboard - BETA </span>\');
      })
     ')),
      #strong(h1(paste0('THURSTON COUNTY WATER QUALITY DASHBOARD [BETA ', Sys.Date(), "]", sep=""))),
      #hr(),
      tabItems(
        tabItem(tabName ='map',
                 fluidRow(column(12,h2("Water Quality Station Map"))),
                 fluidRow(column(8,h5("Below are all stations, active and inactive, used to monitor water quality in Thurston County. To learn more about a station, click on the icon and follow the prompts to various data tabs."))),
                 fluidRow(column(12, br())),
                 fluidRow(column(12,leafletOutput('map',height=700,width="100%"))),
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
        tabItem(tabName ='sum_wqi',
                 column(12,h2("Summary of Water Quality Index")),
                 fluidRow(column(8,leafletOutput('wqi_map',height=800,width="100%")),
                          column(4,
                                 p("The Water Quality Index (WQI) is a quantitative means
                               of assessing the relative health of a water body. 
                               It provides a single number that expresses overall water quality
                               at a given location and time. Parameters include but are not 
                               limited to temperature, turbidity, pH, dissolved oxygen, nutrient levels, 
                               and bioindicators. Each parameter is given a weight based on its 
                               importance for water quality. The final WQI is calculated by 
                               aggregating the individual index scores, and the resulting score 
                               usually falls on a scale from 0 to 100, with higher values indicating 
                               better water quality."),
                                 selectInput('wqi_sum_year','Select Year to Highlight', rev(sort(unique(annual_wqi$WaterYear)))),
                                 plotlyOutput('wqi_summary_plot')
                          )),
                 fluidRow(column(12, br()))
        ),
        tabItem(tabName ='sum_wqc',
                 column(12,h2("Summary of Water Quality Criteria")),
                 fluidRow(column(8,leafletOutput('wqc_map',height=800,width="100%")),
                          column(4,
                                 p('The map to the left displays if a water quality montitoring site had an ',
                                          'exceedance for any water quality criteria during the highlighted year.',br(),
                                          'You may also select an individual parameter for comparison below'),
                                 selectInput('wqc_sum_year','Select Year to Highlight', years_list),
                                 selectInput('wqc_sum_parm','Select All or Individual Parameters for Mapping',
                                             c('All','Water Temperature (°C)','Dissolved Oxygen','pH','E. coli','Fecal Coliform')),
                                 p(paste0('The table below summarizes the number of sites with a violation for each of',
                                          'the monitoring parameters relative to the total number of sites.')),
                                 tableOutput('wqc_summary')
                          )),
                 fluidRow(column(12, br()))
        ),
        
        tabItem(tabName ='trends',
                 column(12,h2("Water Quality Trends")),
                 sidebarLayout(
                   sidebarPanel(width = 3,
                                p('Explore trends across the landscape. Click on a site on the map to view the long-term',
                                  'dataset for that site. You may select individual water quality parameters and set the period',
                                  'of analysis. You may also correct for serial autocorrelation in the Mann-Kendall Trend test and',
                                  ' select individual seasons for analysis.'),
                                selectInput('trend_summary_parm','Select Parameter for Table and Plot',
                                            parm_list),
                                sliderInput('trend_summary_years','Select Year Range for Trend',
                                            value=c(min(years_list),max(years_list)),
                                            min=min(years_list),max=max(years_list),
                                            step=1,sep=''),
                                checkboxInput('rktAuto','Correct for Autocorrelation? (requires 10+ years data)?'),
                                selectInput('rktSeason','Select Seasons for Mann-Kendall Test',
                                            c('All','Winter (Jan-Mar)'='winter','Spring (Apr-Jun)'='spring',
                                              'Summer (Jul-Sep)'='summer','Fall (Oct-Dec)'='fall')),
                                materialSwitch(inputId = "trend_summary_log_scale", label = "Log-scale?", status = "default",value=F),
                                downloadButton('trends_download',label='Download Trend Statistics')
                   ),
                   mainPanel(width = 9,
                             mainPanel(
                               #fluidRow(
                              # column(6,
                                leafletOutput('trend_summary_map',width='100%'),
                               # column(6,
                               #  plotlyOutput('trend_summary_plot'))
                               # ),
                               fluidRow(h3('Trend for Selected Site'),
                                        column(2),
                                        pickerInput('trend_summary_site','Select Site',sites_list)),
                               plotlyOutput('trend_summary_trend_plot')
                                       #     tableOutput('trend_summary_table'),
                                      # plotlyOutput('trend_summary_parm_plot')
                                      )
                   )),
                 fluidRow(column(12, br()))
        ), 
        tabItem(tabName ='wqi',
                 column(12,h2("Water Quality Index"),
                         p("The Water Quality Index (WQI) is a quantitative means
                               of assessing the relative health of a water body. 
                               It provides a single number that expresses overall water quality
                               at a given location and time. Parameters include but are not 
                               limited to temperature, turbidity, pH, dissolved oxygen, nutrient levels, 
                               and bioindicators. Each parameter is given a weight based on its 
                               importance for water quality. The final WQI is calculated by 
                               aggregating the individual index scores, and the resulting score 
                               usually falls on a scale from 0 to 100, with higher values indicating 
                               better water quality."),
                        a("Return to Map",href="#",onclick='openTab("map")')),
                 fluidRow(column(12,sidebarLayout(
                   sidebarPanel(width=3,
                               
                                pickerInput('main_site','Select Site',sites_list),
                                selectInput('wqi_year','Select Year to Highlight',years_list),
                                sliderInput('wqi_trend_years','Select Year Range for Trend',value=c(min(years_list),max(years_list)),
                                            min=min(years_list),max=max(years_list),
                                            step=1,sep='')
                   )
                   ,
                   mainPanel(width=9,
                             uiOutput("data_missing_message"),
                             fluidRow(plotlyOutput('wqi_monthly')),
                             fluidRow(plotlyOutput('wqi_annual')),
                             htmlOutput('wqi_trend_text')
                             
                   ))
                   
                 )),
                 fluidRow(column(12, br()))
        ),
        
        tabItem(tabName ='all_data',
                 column(12,h2("All Data Viewer"),
                        a("Return to Map",href="#",onclick='openTab("map")')),
                 sidebarLayout(
                   sidebarPanel(width = 3,
                                pickerInput('main_site2','Select Site',sites_list, multiple = F),
                                selectInput('data_year','Select Year to Highlight',years_list),
                                selectInput('trend_parm','Select Parameter',parm_list),
                                sliderInput('trend_years','Select Year Range for Trend',value=c(2000,2020),
                                            min=2000,max=2020,
                                            step=1,sep=''),
                                checkboxInput('rktAuto_oneSite','Correct for Autocorrelation? (requires 10+ years data)?'),
                                selectInput('rktSeason_oneSite','Select Seasons for Mann-Kendall Test',
                                            c('All','Winter (Jan-Mar)'='winter','Spring (Apr-Jun)'='spring',
                                              'Summer (Jul-Sep)'='summer','Fall (Oct-Dec)'='fall')),
                                materialSwitch(inputId = "data_log_scale", label = "Log-scale?", status = "default",value=F),
                                hr(),
                                h3('Water Quality Criteria Comparison for Selected Year'),
                                tableOutput('wqc_site')
                     
                   ),
                   mainPanel(width = 9,
                             tabsetPanel(
                               tabPanel("Data Visualization",
                                        plotlyOutput('data_plot')),
                               tabPanel("Data Trends",
                                          plotlyOutput('trend_plot'),    
                                          htmlOutput('trend_text'))
                   ))),
                 fluidRow(column(12, br()))
        ),
        
        tabItem(tabName = 'data_download',
                 column(12,h2("Data Download"),
                        a("Return to Map",href="#",onclick='openTab("map")')),
                 sidebarLayout(
                   sidebarPanel(width = 3,
                                virtualSelectInput('main_site4','Select Site to Download',sites_list, 
                                                   multiple = T,search=T
                                            # options = list(
                                            #   actionsBox = TRUE, 
                                            #   size = 10,
                                            #   selectedTextFormat = "count > 3"
                                            #)
                                ),
                                virtualSelectInput('params_out', "Select Parameter(s)", parm_list,selected=parm_list, 
                                                   multiple = TRUE,search=T
                                            # options = list(
                                            #   actionsBox = TRUE, 
                                            #   size = 10,
                                            #   selectedTextFormat = "count > 3"
                                            # )
                                            ),
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
        tabItem(tabName = 'disclaimer',
                h2("Disclaimer"),
                p(readLines('disclaimer.txt'))
        )
      ),
      tags$head(tags$style(HTML('* {font-family: "Helvetica Neue",Helvetica,Arial,sans-serif};')))
    )
  )

