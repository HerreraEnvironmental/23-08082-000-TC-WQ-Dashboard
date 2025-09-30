#deploy app

source('package_check_script.R') #TODO might replace with install_deps.R
source('external_data/data_prep.R')

rsconnect::deployApp(appFiles = c('ui.R',
                                  'server.R',
                                  'disclaimer.txt',
                                  'external_data/',
                                  'helper_functions/',
                                  'outputs/',
                                  #'wqi_function.R',
                                  'www/'
                                  ),
                     appId=9060286,
                     account= 'herrerainc',
                     server= 'shinyapps.io',
                     appName= 'ThurstonWQDashboard_Streams_Dev',
                     appTitle= 'ThurstonWQDashboard_Streams_Dev')

