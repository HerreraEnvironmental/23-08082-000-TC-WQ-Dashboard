library(dplyr)
library(lubridate)
library(ggplot2)


source('WQP_r_script.R')

wqp_data <- read.csv("wqp_data.csv") %>%
  filter(parameter != "Depth to water from rim of well casing")

stream_use_designations<-readxl::read_xlsx('inputs/Stream Use Designations Herrera.xlsx') %>%
  transmute(SITE_CODE=`Site Code`,
            AquaticLifeUse=case_when(grepl('Spawn',`ALU (Temp. °C)`) ~'Salmonid Spawning, Rearing, and Migration',
                                     grepl('Core',`ALU (Temp. °C)`) ~'Core Summer Salmonid Habitat',
                                     grepl('13',`ALU (Temp. °C)`) ~'Marine',
                                     T ~ 'ERROR')) %>%
  filter(!is.na(SITE_CODE))


## Using wqp data
streams_sites<-wqp_data %>%
  select(gid,SITE_CODE,SITE_NAME,Metro_ID,LAT,LON)%>%
  distinct() %>%
  arrange(SITE_NAME) %>%
  left_join(stream_use_designations) %>%
  mutate(AquaticLifeUse=ifelse(is.na(AquaticLifeUse),"Core Summer Salmonid Habitat",AquaticLifeUse))

saveRDS(streams_sites,'outputs/streams_sites.RDS')
write.csv(streams_sites, 'outputs/streams_sites.csv', row.names = F)

## WQP data
streams_wq_dat <- wqp_data %>%
  tibble() %>%
  mutate(sample_utc_offset = ifelse(dst(date_time), 7, 8)) %>%
  mutate(DateTime = with_tz(as_datetime(date_time, tz = "UTC") + hours(sample_utc_offset),
         tz = "America/Los_Angeles")) %>%
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
         WY_FakeDate=as.Date(if_else(Month>=10,FakeDate-years(1),FakeDate)),
#rename to internally consistent parameter names
  parameter=case_when(  parameter %in% c("Dissolved oxygen (DO)" ) ~ "Dissolved Oxygen",
             parameter %in% c("Temperature") ~ "Temperature, water",
             parameter %in% c("Nitrate + Nitrite as N") ~ 'Nitrite + Nitrate',
             parameter %in% c("Total Phosphorus, mixed forms",'Phosphorus','Total Phosphorus') ~ 'Total Phosphorus',
             parameter %in% c("Escherichia coli") ~ 'E. coli',
             parameter %in% c("Specific conductance",'Conductivity') ~'Conductivity' ,
             parameter %in% c("Total suspended solids") ~ 'Total Suspended Solids',
             .default = parameter)
)

#TODO The following snippet will be removed in a future update and replaced with a config file. 
mini_params <- data.frame(
  parameter = c("Dissolved oxygen (DO)", "Temperature", "Nitrate + Nitrite as N",
                "Total Phosphorus, mixed forms", "Phosphorus", "Total Phosphorus",
                "Escherichia coli", "Specific conductance", "Conductivity",
                "Total suspended solids"),
  mini_param = c("Dissolved Oxygen", "Temperature, water", "Nitrite + Nitrate",
                 "Total Phosphorus", "Total Phosphorus", "Total Phosphorus",
                 "E. coli", "Conductivity", "Conductivity", "Total Suspended Solids")
)
write_csv(mini_params, "public_dashboard_outputs_streams/parameter_merges.csv")

#treams_wq_dat["parameter"][streams_wq_dat["parameter"] == "Temperature, water"] <- "Water Temperature (°C)"

saveRDS(streams_wq_dat,'outputs/streams_wq_dat.RDS')

sites_list<-setNames(streams_sites$SITE_CODE,paste0(streams_sites$SITE_NAME,' (',streams_sites$SITE_CODE,')'))
parm_list<-unique(streams_wq_dat$parameter) %>%
  factor(.,levels=c('Temperature, water',
                    'Dissolved Oxygen',
                    'pH',
                    'Conductivity',
                    'Turbidity',
                    'Total Phosphorus',
                    'Nitrate + Nitrite',
                    'Ammonia-nitrogen',
                    'Total Suspended Solids',
                    'Fecal Coliform',
                    'E. coli',
                    "Enterococcus",
                    'Flow',
                    'Copper',
                    'Lead',
                    'Zinc',
                    'Hardness',
                    "Petroleum hydrocarbons, total extractable")) %>%
  levels()
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
                             c('Temp','Temperature, water'),
                             c('TPN','Total Nitrogen'),
                             #for the sake of this example let's use nitate
                             c('TPN',"Nitrate + Nitrite"),
                             c('Turb','Turbidity')))
colnames(parm_table)<-c('shortParmName','parameter')


annual_wqi<-streams_wq_dat %>%
  left_join(stream_use_designations) %>%
  left_join(parm_table) %>%
  filter(!is.na(shortParmName)) %>%
  with(.,
       wqi_calc(site=SITE_CODE,
                value=newResultValue,
                shortParmName = shortParmName,
                date=as.Date(DateTime),
                TemperatureCode = ifelse(AquaticLifeUse=='Core Summer Salmonid Habitat',8,
                                         ifelse(AquaticLifeUse=='Salmonid Spawning, Rearing, and Migration',9,NA)), 
                OxygenCode =  ifelse(AquaticLifeUse=='Core Summer Salmonid Habitat',26,
                                     ifelse(AquaticLifeUse=='Salmonid Spawning, Rearing, and Migration',21,NA)), 
                small_PS_stream = T #assume all puget sound small streams
       ))
saveRDS(annual_wqi,'outputs/annual_wqi.RDS')

streams_wq_dat %>%
  left_join(streams_sites %>% select(SITE_CODE,AquaticLifeUse)) %>%
  left_join(parm_table) %>%
  filter(!is.na(shortParmName)) %>%
  with(.,
       wqi_calc(site=SITE_CODE,
                value=newResultValue,
                shortParmName = shortParmName,
                date=as.Date(DateTime),
                TemperatureCode = ifelse(AquaticLifeUse=='Core Summer Salmonid Habitat',8,
                                         ifelse(AquaticLifeUse=='Salmonid Spawning, Rearing, and Migration',9,NA)), 
                OxygenCode =  ifelse(AquaticLifeUse=='Core Summer Salmonid Habitat',26,
                                     ifelse(AquaticLifeUse=='Salmonid Spawning, Rearing, and Migration',21,NA)), 
                small_PS_stream = T, #assume all puget sound small streams
                summary_by = 'ByParameter'
       )) %>%
  saveRDS('outputs/annual_wqi_by_parameter.RDS')

streams_wq_dat %>%
  left_join(streams_sites %>% select(SITE_CODE,AquaticLifeUse)) %>%
  left_join(parm_table) %>%
  filter(!is.na(shortParmName)) %>%
  with(.,
       wqi_calc(site=SITE_CODE,
                value=newResultValue,
                shortParmName = shortParmName,
                date=as.Date(DateTime),
                TemperatureCode = ifelse(AquaticLifeUse=='Core Summer Salmonid Habitat',8,
                                         ifelse(AquaticLifeUse=='Salmonid Spawning, Rearing, and Migration',9,NA)), 
                OxygenCode =  ifelse(AquaticLifeUse=='Core Summer Salmonid Habitat',26,
                                     ifelse(AquaticLifeUse=='Salmonid Spawning, Rearing, and Migration',21,NA)), 
                small_PS_stream = T, #assume all puget sound small streams
                summary_by = 'ByParameter',
                period='Monthly'
       )) %>%
  saveRDS('outputs/monthly_wqi_by_parameter.RDS')


streams_wq_dat %>%
  left_join(streams_sites %>% select(SITE_CODE,AquaticLifeUse)) %>%
  left_join(parm_table) %>%
  filter(!is.na(shortParmName)) %>%
  with(.,
       wqi_calc(site=SITE_CODE,
                value=newResultValue,
                shortParmName = shortParmName,
                date=as.Date(DateTime),
                TemperatureCode = ifelse(AquaticLifeUse=='Core Summer Salmonid Habitat',8,
                                         ifelse(AquaticLifeUse=='Salmonid Spawning, Rearing, and Migration',9,NA)), 
                OxygenCode =  ifelse(AquaticLifeUse=='Core Summer Salmonid Habitat',26,
                                     ifelse(AquaticLifeUse=='Salmonid Spawning, Rearing, and Migration',21,NA)), 
                small_PS_stream = T, #assume all puget sound small streams
                period='Monthly'
       )) %>%
  saveRDS('outputs/monthly_wqi.RDS')

