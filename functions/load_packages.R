## Short script to check for and install required packages 

## Print a warning if the user has conda installed;
## 
if(is.null(system("conda --version"))) {
  print("Your system has conda installed.")
}

installPackages <- c("bslib",
                     "dplyr",
                     "DT",
                     "fresh",
                     "leaflet",
                     "lubridate",
                     "plotly",
                     "purrr",
                     "rintrojs",
                     "rkt",
                     "shiny",
                     "shiny.router",
                     "shinyBS",
                     "shinyjs",
                     "shinydashboard",
                     "shinyWidgets")


for(Package in installPackages) {
  if(!require(Package,character.only = TRUE)) { 
    install.packages(Package, dependencies=TRUE)
  }
  suppressWarnings(library(Package, character.only = TRUE))
}



