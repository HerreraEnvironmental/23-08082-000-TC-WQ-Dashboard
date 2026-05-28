library(arrow)
library(tidyverse)
library(plotly)
library(shiny)
library(leaflet)
library(leaflet.esri)
library(bslib)
library(DT)
library(shinyWidgets)
library(shinydashboard)
library(shiny.router)
library(shinyjs)
library(shinyBS)
library(purrr)
library(tidyr)
library(rkt)
library(vegabrite)
library(purrr)


source("ui.R")
if (!file.exists("server.R")) {
  server <- function(input, output, session) { }
} else {
  source("server.R") # defines server
}
shinyApp(ui = ui, server = server)
