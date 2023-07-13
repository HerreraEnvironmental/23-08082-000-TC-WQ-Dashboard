#based on Ecology Version 6 - (2014.06.11)
#note that this uses outdated designations for Fecal Coliform (Extraordinary, Primary, Secondary)
#and uses fecal coliform instead of e. coli
#for the purposes of this effort - E. coli and Fecal coliform are assumed to be one to one, and the the primary standard is used
library(readxl)
library(dplyr)
library(lubridate)


# ecology_curves<-read_excel('WQI_ecology_v6.xlsm',sheet='Curves',n_max=204)
# write.csv(ecology_curves,'wqi_ecology_curves_v6.csv',row.names = F)




#some sets of coefficients are dependt on month


## need to bring in a look up table that designates each of the sream uses
#aquatic life, supplement criteria dates fro temperature, oxygen code



wqi_calc<-function(period=c('Annual','Monthly'),summary_by=c('Index','ByParameter'),
                   site=NULL,date,value,shortParmName,TemperatureCode=NULL,OxygenCode=NULL,
                   small_PS_stream=T,
                   curveSource='wqi_ecology_curves_v6.csv'){
  #basic equation is WQI = a + b1 * [C] + b2 * [C]^2
  #where [C] is the constituent in question 
  #Fecal Coliform, Total Suspended Solids,Total Phosphorus, and Turbitity are (natural) log-transformed prior to this euqation
  period=period[1]
  summary_by=summary_by[1]
  parms_to_log<-c('FC','SUSSOL','TP_P','Turb')
  
  if(!file.exists(curveSource)) stop('Must provide lookup table')
  
  if(!(period %in% c('Annual','Monthly'))) stop('Must select Annual or Monthly for WQI summary')
  if(!(summary_by %in% c('Index','ByParameter'))) stop('Must select Index or ByParameter for output')
  if(any(shortParmName=='Temp'&missing(TemperatureCode))) {
    stop('Must provide temperature code (depeneds on WQ Standards) to calculate WQI for temperature')}
  if(any(shortParmName=='Oxygen'&missing(OxygenCode))) {
    stop('Must provide oxygen code (depened on WQ Standards) to calculate WQI for oxygen')}
  if(missing(site)){
    site='A'
    warning('No site name provided. Assuming data are all from same site ("A").')
  }
 # if(missing(value)|missing(date)|missing(shortParmName)) simpleError('Must input date, value, and shortened parameter name')
  if(!(any(shortParmName %in% c('FC','Oxygen','pH','TP_P','SUSSOL','Temp','TPN','Turb')))){
    stop('Must provide parameter name in shorterned form:\n
                FC - Fecal coliform\n
                Oxygen - Dissolved Oxygen (mg/L)]\n
                pH - pH\n
                TP_P - Total Phosphorus (mg/L)\n
                SUSSOL - Total suspended solid (mg/L)\n
                Temp - Temperature (deg C)\n
                TPN - Total (persulfate) nitrogen (mg/L)\n
                Turb - turbidity (NTU)')
  }
  
  ecology_curves<-read.csv(curveSource)
  
  wqi_prep<-tibble(site,date,shortParmName,value,TemperatureCode,OxygenCode)%>%
    mutate(Year=year(date),
           Month=month(date),
           WaterYear=ifelse(Month>=10,Year+1,Year)) %>%
    mutate(MonthFraction=Month+day(date)/days_in_month(Month)) %>%
    group_by(site,shortParmName,WaterYear,Month,MonthFraction) %>%
    summarise(value=mean(value)) %>%
    mutate(WQI_Value=ifelse(shortParmName %in% parms_to_log,log(value),value)) %>%
    mutate(rownumber=ifelse(shortParmName=='Temp',TemperatureCode,
                            ifelse(shortParmName=='TP_P',ifelse(small_PS_stream,72,272),
                                   ifelse(shortParmName=='Oxygen',OxygenCode,
                                          ifelse(shortParmName=='TPN',ifelse(small_PS_stream,62,262),
                                                 ifelse(shortParmName=='Turb',92,
                                                        ifelse(shortParmName=='SUSSOL',82,
                                                               ifelse(shortParmName=='FC',51,
                                                                      ifelse(shortParmName=='pH',41,-9999)))))))))  %>%
    left_join(ecology_curves,by=c('rownumber'='ParamClassID')) %>%
    filter(value>=LowerResult&value<=UpperResult&
             MonthFraction>=Start.Month&MonthFraction<End.Month) %>%
    mutate(WQI= a + b*WQI_Value + b2*WQI_Value^2) %>%
    mutate(WQI=ifelse(WQI>=100,100,WQI)) %>%
    mutate(WQI=ifelse(WQI<1,1,WQI)) %>%
    select(site,shortParmName,WaterYear,Month,value,WQI_Value,WQI) %>%
    summarise(value=mean(value),
              WQI_Value=mean(WQI_Value),
              WQI=mean(WQI)) %>%
    mutate(Month=factor(Month,levels=c(10:12,1:9))) %>%
    arrange(site,shortParmName,WaterYear,Month)
  
  #Monthly Score = FC + Oxygen + ph+ Temp + NUTRIENT SCORE+ Sediment 2/(1/TSS + 1/Turb)
  
  #nutrient score is based on npratio, if ratio <10, use TN, if >20, use TP, else use min
  if(any(shortParmName %in% c('SUSSOL','Turb'))){
  sediment_score<-wqi_prep %>%
    filter(shortParmName %in% c('SUSSOL','Turb')) %>%
    group_by(site,WaterYear,Month) %>%
    summarise(shortParmName='Sediment',
              WQI=sum(WQI^-1)^-1*length(WQI))
  } else sediment_score<-NULL
  
  if(any(shortParmName %in% c('TPN','TP_P'))){
  np_ratios<-wqi_prep %>%
    select(-WQI_Value,-WQI) %>%
    filter(shortParmName %in% c('TPN','TP_P')) %>%
    group_by(site,WaterYear,Month) %>%
    tidyr::pivot_wider(names_from=shortParmName,values_from=value,values_fn=mean) %>%
    mutate(NPRatio=TPN/TP_P) %>%
    select(site,WaterYear,Month,NPRatio)
  
  nutrient_score<-wqi_prep %>%
    filter(shortParmName %in% c('TPN','TP_P')) %>%
    left_join(np_ratios) %>%
    group_by(site,WaterYear,Month) %>%
    summarise(WQI=ifelse(is.na(NPRatio),min(WQI,na.rm=T),
                         ifelse(NPRatio<=10,WQI[shortParmName=='TPN'],
                                ifelse(NPRatio>20,WQI[shortParmName=='TP_P'],
                                       min(WQI,na.rm=T))))) %>%
    mutate(shortParmName='Nutrient') %>%
    group_by(site,WaterYear,Month,shortParmName) %>%
    summarise(WQI=mean(WQI))
  } else nutrient_score<-NULL
  
  if(summary_by=='Index'){
  monthly_wqi<-wqi_prep %>%
    filter(!(shortParmName %in% c('TP_P','TPN','Turb','SUSSOL'))) %>%
    select(-WQI_Value,-value) %>%
    bind_rows(sediment_score,nutrient_score) %>%
    group_by(site,WaterYear,Month) %>%
    mutate(Penalty=ifelse(WQI<80,(85-WQI)/2,0)) %>%
    mutate(Penalty=ifelse(Penalty<0,0,Penalty)) %>%
    mutate(Penalty=ifelse(shortParmName %in% c('Sediment','Nutrient')&Penalty>20,20,Penalty)) %>%
    summarise(WQI=mean(WQI)-sum(Penalty)) %>%
    mutate(WQI=ifelse(WQI<1,1,WQI))
  }
  
  if(period=='Monthly'&summary_by=='Index'){
    return(monthly_wqi)
  } 
  
  if(period=='Monthly'&summary_by=='ByParameter'){
    monthly_wqi_by_parameter<-wqi_prep %>%
      select(-WQI_Value,-value) %>%
      bind_rows(sediment_score,nutrient_score) %>%
      group_by(site,WaterYear,Month) %>%
      mutate(Penalty=ifelse(WQI<80&!(shortParmName %in% c('TP_P','TPN','Turb','SUSSOL')),(85-WQI)/2,0)) %>%
      mutate(Penalty=ifelse(Penalty<0,0,Penalty)) %>%
      mutate(Penalty=ifelse(shortParmName %in% c('Sediment','Nutrient')&Penalty>20,20,Penalty)) %>%
      mutate(Penalty=sum(Penalty)) %>%
      tidyr::pivot_wider(values_from=WQI,names_from=shortParmName,values_fn=mean)
    return(monthly_wqi_by_parameter)
  } 
  if(period=='Annual'&summary_by=='ByParameter'){
    annual_wqi_by_parameter<-wqi_prep %>%
      select(-WQI_Value,-value) %>%
      bind_rows(sediment_score,nutrient_score) %>%
      group_by(site,shortParmName,WaterYear) %>%
      summarise(WQI=ifelse(grepl('Oxygen|Temp|pH',shortParmName),sort(WQI)[1],mean(sort(WQI)[1:3],na.rm=T))) %>%
      summarise(WQI=mean(WQI,na.rm=T)) %>%
      tidyr::pivot_wider(values_from=WQI,names_from=shortParmName,values_fn=mean) 
    return(annual_wqi_by_parameter)
  } 
  if(period=='Annual'&summary_by=='Index'){
    annual_wqi<-monthly_wqi %>%
      group_by(site,WaterYear) %>%
      summarise(WQI=mean(sort(WQI)[1:3],na.rm=T))
    return(annual_wqi)
  }
}

##run example
streams_wq_dat<-readRDS('outputs/streams_wq_dat.RDS')
streams_sites<-readRDS('outputs/streams_sites.RDS')

#let's make a lookup list for parameter names
parm_table<-data.frame(rbind(c('FC','Fecal Coliform'),
                             c('FC','E. coli'),
                             c('FC','Fecal Bacteria'),
                             c('Oxygen','Dissolved Oxygen'),
                             c('pH','pH'),
                             c('TP_P','Total Phosphorus'),
                             c('SUSSOL','Total Suspended Solids'),
                             c('Temp',"Water Temperature (°C)"),
                             #c('TPN','Total Nitrogen'),
                             #for the sake of this example let's use nitate
                             c('TPN','Nitrate-Nitrite as N'),
                             c('Turb','Turbidity')))
colnames(parm_table)<-c('shortParmName','parameter')


tc_annual_wqi<-streams_wq_dat %>%
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

