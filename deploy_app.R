#deploy app

source('package_check_script.R')
source('data_prep.R')

rsconnect::deployApp(appFiles = c('ui.R',
                                  'server.R',
                                  'functions/',
                                  'outputs/',
                                  'wqi_function.R',
                                  'www/'
                                  ),
                     appId=9060286,
                     account= 'herrerainc',
                     server= 'shinyapps.io',
                     appName= 'ThurstonWQDashboard_Streams_Dev',
                     appTitle= 'ThurstonWQDashboard_Streams_Dev')
