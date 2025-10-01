library(tidyverse)
library(readxl)
library(lubridate)

## Additional Filtered Export of Most Recent WQI score

# Supplementary imports ----------------------------------------------------------
stream_use_designations <- read_xlsx("inputs/Stream Use Designations Herrera.xlsx") |>
  transmute(
    SITE_CODE=`Site Code`,
    AquaticLifeUse=case_when(
        grepl("Spawn",`ALU (Temp. °C)`) ~ "Salmonid Spawning, Rearing, and Migration",
        grepl('Core',`ALU (Temp. °C)`) ~ "Core Summer Salmonid Habitat",
        grepl("13",`ALU (Temp. °C)`) ~ "Marine",
        TRUE ~ "ERROR"
        )
    ) |>
  filter(!is.na(SITE_CODE))

parm_table <- tibble::tibble(
  shortParmName = c(
    "FC", "FC", "FC",
    "Oxygen",
    "pH",
    "TP_P",
    "SUSSOL",
    "Temp",
    "TPN", "TPN",
    "Turb"
  ),
  parameter = c(
    "Fecal Coliform",
    "E. coli",
    "Fecal Bacteria",
    "Dissolved Oxygen",
    "pH",
    "Total Phosphorus",
    "Total Suspended Solids",
    "Temperature, water",
    "Total Nitrogen",
    "Nitrate + Nitrite",
    "Turbidity"
  )
)



# WQI calc ----------------------------------------------------------------
wqi_calc<-function(period=c('Annual','Monthly'),summary_by=c('Index','ByParameter'),
                   site=NULL,date,value,shortParmName,TemperatureCode=NULL,OxygenCode=NULL,
                   small_PS_stream=T,
                   curveSource='wqi_ecology_curves_v6.csv'){
  period=period[1]
  summary_by=summary_by[1]
  parms_to_log<-c('FC','SUSSOL','TP_P','Turb')
  
  if(!file.exists(curveSource)) stop('Must provide lookup table')
  
  if(!(period %in% c('Annual','Monthly'))) stop('Must select Annual or Monthly for WQI summary')
  if(!(summary_by %in% c('Index','ByParameter'))) stop('Must select Index or ByParameter for output')
  if(any(shortParmName=='Temp'&missing(TemperatureCode))) {
    stop('Must provide temperature code (depends on WQ Standards) to calculate WQI for temperature')}
  if(any(shortParmName=='Oxygen'&missing(OxygenCode))) {
    stop('Must provide oxygen code (depened on WQ Standards) to calculate WQI for oxygen')}
  if(missing(site)){
    site='A'
    warning('No site name provided. Assuming data are all from same site ("A").')
  }
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
  
  wqi_prep<-tibble(site,date,shortParmName,value,TemperatureCode,OxygenCode) %>%
    mutate(Year=year(date),
           Month=month(date),
           WaterYear=ifelse(Month>=10,Year+1,Year)) %>%
    mutate(MonthFraction=Month+day(date)/days_in_month(Month)) %>%
    group_by(site,shortParmName,WaterYear,Month,MonthFraction) %>%
    mutate(value=mean(value)) %>% #TODO Was summarise
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
    arrange(site,shortParmName,WaterYear,Month) %>%
    group_by(site, WaterYear) %>%
    mutate(nSamples = n())
    

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
      mutate(shortParmName=factor(shortParmName,
                                  levels=c('Temp','Oxygen','pH','FC',
                                           'TPN','TP_P','Nutrient',
                                           'Turb','SUSSOL','Sediment'))) %>%
      tidyr::pivot_wider(values_from=WQI,names_from=shortParmName,values_fn=mean,names_expand = T)
    return(monthly_wqi_by_parameter)
  }
  if(period=='Annual'&summary_by=='ByParameter'){
    annual_wqi_by_parameter<-wqi_prep %>%
      select(-WQI_Value,-value) %>%
      bind_rows(sediment_score,nutrient_score) %>%
      group_by(site,shortParmName,WaterYear) %>%
      # TODO: summarize changed to mutate for specific troubleshooting.
      mutate(WQI=ifelse(grepl('Oxygen|Temp|pH',shortParmName),sort(WQI)[1],mean(sort(WQI)[1:3],na.rm=T))) %>%
      mutate(WQI=mean(WQI,na.rm=T)) %>%
      mutate(shortParmName=factor(shortParmName,
                                  levels=c('Temp','Oxygen','pH','FC',
                                           'TPN','TP_P','Nutrient',
                                           'Turb','SUSSOL','Sediment'))) %>%
      tidyr::pivot_wider(values_from=WQI,names_from=shortParmName,values_fn=mean,names_expand = T)
    return(annual_wqi_by_parameter)
  }
  if(period=='Annual'&summary_by=='Index'){
    annual_wqi<-monthly_wqi %>%
      group_by(site,WaterYear) %>%
      summarise(WQI=mean(sort(WQI)[1:3],na.rm=T))
    return(annual_wqi)
  }
}



# Application of calcs ----------------------------------------
streams_wq_dat <- readRDS("outputs/streams_wq_dat.RDS")

annual_wqi_public<-streams_wq_dat %>%
  left_join(stream_use_designations) %>%
  left_join(parm_table) %>%
  filter(!is.na(shortParmName)) %>%
  with(.,
       wqi_calc(period = "Annual", summary_by = "ByParameter", site=SITE_CODE,
                value=newResultValue,
                shortParmName = shortParmName,
                date=as.Date(DateTime),
                # TODO Temperature and Oxygen are getting lost with this aquatic life use filtering
                # TemperatureCode = 8,
                # OxygenCode = 26,
                TemperatureCode = ifelse(AquaticLifeUse=='Core Summer Salmonid Habitat',8,
                                         ifelse(AquaticLifeUse=='Salmonid Spawning, Rearing, and Migration',9,NA)), 
                OxygenCode =  ifelse(AquaticLifeUse=='Core Summer Salmonid Habitat',26,
                                     ifelse(AquaticLifeUse=='Salmonid Spawning, Rearing, and Migration',21,NA)), 
                small_PS_stream = T
       )) %>%
  ungroup() %>%
  filter(n_distinct(Month, na.rm = TRUE) >= 8, .by = c(site, WaterYear)) %>%
  select(site, WaterYear, nSamples:Sediment) %>%
  unique()

write_csv(annual_wqi_public, "public_dashboard_outputs_streams/recent_WQI.csv")

