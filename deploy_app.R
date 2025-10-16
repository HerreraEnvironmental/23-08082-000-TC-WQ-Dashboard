# deploy app

source("install_deps.R")
source("sourcing_scripts/data_prep.R")

branch <- system("git rev-parse --abbrev-ref HEAD", intern = TRUE)

if (branch == "main") {
  rsconnect::deployApp(
    appFiles = c(
      "ui.R",
      "server.R",
      "disclaimer.txt",
      "sourcing_scripts/",
      "helper_functions/",
      "outputs/",
      #' wqi_function.R',
      "www/"
    ),
    appId = 9060286,
    account = "herrerainc",
    server = "shinyapps.io",
    appName = "ThurstonWQDashboard_Streams_Dev",
    appTitle = "ThurstonWQDashboard_Streams_Dev"
  )
} else {
  stop("Deployments are only allowed from main branch!")
}
