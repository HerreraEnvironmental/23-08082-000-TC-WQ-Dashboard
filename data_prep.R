library(dplyr)
library(lubridate)

library(ggplot2)

#streams data prep
##develop a lookup table for sites
##check for flags

streams_sites<-read.csv('inputs/Herrera All Stream Data Dump 4 12 2023.csv') %>%
  select(gid,SITE_CODE,SITE_NAME,Metro_ID,LAT,LON)%>%
  distinct()

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
         Year=year(DateTime),
         Month=month(DateTime),
         WaterYear=ifelse(Month>=10,Year+1,Year),
         FakeDate=as.Date(paste(2000,Month,day(DateTime),sep='-')),
         WY_FakeDate=as.Date(if_else(Month>=10,FakeDate-years(1),FakeDate)))

saveRDS(streams_wq_dat,'outputs/streams_wq_dat.RDS')

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

