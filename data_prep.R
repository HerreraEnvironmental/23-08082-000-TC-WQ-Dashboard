library(dplyr)
library(lubridate)

library(ggplot2)

#streams data prep
##develop a lookup table for sites
##check for flags

streams_sites<-read.csv('inputs/Herrera All Stream Data Dump 4 12 2023.csv') %>%
  select(gid,SITE_CODE,SITE_NAME,Metro_ID,LAT,LON)%>%
  distinct() %>%
  arrange(SITE_NAME)

saveRDS(streams_sites,'outputs/streams_sites.RDS')


streams_wq_dat<-read.csv('inputs/Herrera All Stream Data Dump 4 12 2023.csv') %>%
  tibble()%>%
  mutate(DateTime=with_tz(as_datetime(date_time,format='%m/%d/%Y %H:%M',tz='UTC')+hours(sample_utc_offset),
                          tz='America/Los_Angeles')) %>%
  select(SITE_CODE,DateTime,parameter,value,unit,depth_m,dup,mdl,pql,qualifier) %>%
  mutate(unit=trimws(unit),
         qualifier=trimws(qualifier),
         nonDetectFlag=grepl('U',qualifier),
         newResultValue=ifelse(nonDetectFlag,pql,value),
         newResultValue=ifelse(parameter=='Turbidity'&newResultValue<=0,0.01,newResultValue),
         Year=year(DateTime),
         Month=month(DateTime),
         WaterYear=ifelse(Month>=10,Year+1,Year),
         FakeDate=as.Date(paste(2000,Month,day(DateTime),sep='-')),
         WY_FakeDate=as.Date(if_else(Month>=10,FakeDate-years(1),FakeDate)))

streams_wq_dat["parameter"][streams_wq_dat["parameter"] == "Temperature, water"] <- "Water Temperature (°C)"

saveRDS(streams_wq_dat,'outputs/streams_wq_dat.RDS')

sites_list<-setNames(streams_sites$SITE_CODE,paste0(streams_sites$SITE_NAME,' (',streams_sites$SITE_CODE,')'))
parm_list<-unique(streams_wq_dat$parameter)
years_list<-sort(unique(streams_wq_dat$WaterYear),T)

saveRDS(sites_list,'outputs/sites_list.RDS')
saveRDS(parm_list,'outputs/parm_list.RDS')
saveRDS(years_list,'outputs/years_list.RDS')

unique(streams_wq_dat$depth_m) #all 0 or NA
unique(streams_wq_dat$dup) #there are dups
unique(streams_wq_dat$qualifier)
#[1] "   " "FE " "JL " "FH " ""    "U  " "J  " "K  " "FA " "EST" "EQP" "FS " "FD " "JG "
#EST = estimated J
#JG = estimated high
#JG = estimated low
#FE = ????
#FH = ????
#U = nonDetect
#K = ????
#FD = ?????
#EQP = ????
#FA = ????

##need to do:
#Stream WQI

source('wqi_function.R')

parm_table<-data.frame(rbind(c('FC','Fecal Coliform'),
                             c('FC','E. coli'),
                             c('FC','Fecal Bacteria'),
                             c('Oxygen','Dissolved Oxygen'),
                             c('pH','pH'),
                             c('TP_P','Total Phosphorus'),
                             c('SUSSOL','Total Suspended Solids'),
                             c('Temp','Water Temperature (°C)'),
                             #c('TPN','Total Nitrogen'),
                             #for the sake of this example let's use nitate
                             c('TPN','Nitrate-Nitrite as N'),
                             c('Turb','Turbidity')))
colnames(parm_table)<-c('shortParmName','parameter')


annual_wqi<-streams_wq_dat %>%
  left_join(parm_table) %>%
  filter(!is.na(shortParmName)) %>%
  with(.,
       wqi_calc(site=SITE_CODE,
                value=newResultValue,
                shortParmName = shortParmName,
                date=as.Date(DateTime),
                TemperatureCode = 8, #assume core for no2
                OxygenCode = 26, #assume core for now,
                small_PS_stream = T #assume all puget sound small streams
       ))
saveRDS(annual_wqi,'outputs/annual_wqi.RDS')

streams_wq_dat %>%
  left_join(parm_table) %>%
  filter(!is.na(shortParmName)) %>%
  with(.,
       wqi_calc(site=SITE_CODE,
                value=newResultValue,
                shortParmName = shortParmName,
                date=as.Date(DateTime),
                TemperatureCode = 8, #assume core for no2
                OxygenCode = 26, #assume core for now,
                small_PS_stream = T, #assume all puget sound small streams
                summary_by = 'ByParameter'
       )) %>%
  saveRDS('outputs/annual_wqi_by_parameter.RDS')

streams_wq_dat %>%
  left_join(parm_table) %>%
  filter(!is.na(shortParmName)) %>%
  with(.,
       wqi_calc(site=SITE_CODE,
                value=newResultValue,
                shortParmName = shortParmName,
                date=as.Date(DateTime),
                TemperatureCode = 8, #assume core for no2
                OxygenCode = 26, #assume core for now,
                small_PS_stream = T, #assume all puget sound small streams
                summary_by = 'ByParameter',
                period='Monthly'
       )) %>%
  saveRDS('outputs/monthly_wqi_by_parameter.RDS')


streams_wq_dat %>%
  left_join(parm_table) %>%
  filter(!is.na(shortParmName)) %>%
  with(.,
       wqi_calc(site=SITE_CODE,
                value=newResultValue,
                shortParmName = shortParmName,
                date=as.Date(DateTime),
                TemperatureCode = 8, #assume core for no2
                OxygenCode = 26, #assume core for now,
                small_PS_stream = T, #assume all puget sound small streams
                period='Monthly'
       )) %>%
  saveRDS('outputs/monthly_wqi.RDS')

