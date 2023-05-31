# water quality criteria function
# goal: for a given water year, count the number of exceedance/violations for water quality criteria
#limitations: focusing on pH, FC/EC, temperature, dissolved oxygen

#based on WA 173-201A-200 -https://app.leg.wa.gov/WAC/default.aspx?cite=173-201A-200 - as of May 31, 2023

library(dplyr)


#assume instantaneous samples rather than continuous
wqc_function<-function(AquaticLifeUse=c("Char Spawning and Rearing", "Core Summer Salmonid Habitat", 
                          "Salmonid Spawning, Rearing, and Migration", "Salmonid Rearing and Migration Only", 
                          "Nonanadromous Interior Redband Trout", "Indigenous Warm Water Species"),
         Month,Parameter,Result){
  AquaticLifeUse<-AquaticLifeUse[1]
  #define look up table for designated aquatic life uses
  #note that temperature and dissolved oxygen for lakes are not absolute thresholds are based on relative change from natty conditions
  aquatic_life_uses<-tibble(
    AquaticLifeUse=c('Char Spawning and Rearing',
                     'Core Summer Salmonid Habitat',
                     'Salmonid Spawning, Rearing, and Migration',
                     'Salmonid Rearing and Migration Only',
                     'Nonanadromous Interior Redband Trout',
                     'Indigenous Warm Water Species'),
    Temp_7DADMax_C=c(12,
                     16,
                     17.5,
                     17.5,
                     18,
                     20),
    DO_DailyMin_mgL=c(10,
                      10,
                      10,
                      6.5,
                      10,
                      6.5),
    DO_Saturation=c(90,
                    95,
                    90,
                    NA,
                    90,
                    NA),
    Turbidity=NA,
    TotDissolvedGas=110,
    pH_min=6.5,
    pH_max=8.5
  )
  
  bacteria_criteria<-tibble(
    Parameter=c(
      'E. coli',
      'Fecal Coliform'
    ),
    GeoMean_Criteria=100,
    #Ambient water quality samples: When averaging bacteria sample values for comparison to the geometric mean criteria, 
    #it is preferable to average by season. The averaging period of bacteria sample data shall be 90 days or less.
    STV_Criteria=c(320,
                   200)
    #STV no more than 10 percent of samples in averaging period (or maximum if less than 10 samples)
  )
  
  if(!all(AquaticLifeUse %in% aquatic_life_uses$AquaticLifeUse)) stop('Must select Aquatic Life Use')
  
  wqc_out<-NA
  
  if(Parameter %in% c('Temperature','Temperature, water')){
    wqc_out<- tibble(nViolation=length(which(Result>aquatic_life_uses$Temp_7DADMax_C[aquatic_life_uses$AquaticLifeUse==AquaticLifeUse]))) %>%
      mutate(Notes=paste0(nViolation,' violations in ',length(Result), ' samples'))
    
  }
  
  if(Parameter %in% c('Dissolved Oxygen','Dissolved Oxygen, Field')){
    wqc_out<-tibble(nViolation=length(which(Result<aquatic_life_uses$DO_DailyMin_mgL[aquatic_life_uses$AquaticLifeUse==AquaticLifeUse]))) %>%
      mutate(Notes=paste0(nViolation,' violations in ',length(Result), ' samples'))
    
  }
  
  if(Parameter %in% c('Dissolved Oxygen Saturation')){
    wqc_out<- tibble(nViolation=length(which(Result<aquatic_life_uses$DO_Saturation[aquatic_life_uses$AquaticLifeUse==AquaticLifeUse]))) %>%
      mutate(Notes=paste0(nViolation,' violations in ',length(Result), ' samples'))
    
  }
  
  if(Parameter %in% c('pH')){
    wqc_out<-tibble(nViolation=length(which(Result>aquatic_life_uses$pH_max[aquatic_life_uses$AquaticLifeUse==AquaticLifeUse]&
                                  Result<aquatic_life_uses$pH_min[aquatic_life_uses$AquaticLifeUse==AquaticLifeUse]))) %>%
      mutate(Notes=paste0(nViolation,' violations in ',length(Result), ' samples'))
    
  }
  
  if(Parameter %in% c('Total Dissolved Gas')){
    wqc_out<-tibble(nViolation=length(which(Result>aquatic_life_uses$TotDissolvedGas[aquatic_life_uses$AquaticLifeUse==AquaticLifeUse]))) %>%
      mutate(Notes=paste0(nViolation,' violations in ',length(Result), ' samples'))
  }
  
  if(Parameter %in% c('E. coli','Fecal Coliform')){
    wqc_out<-tibble(Month=Month,
                    Result=Result,
                    Parameter=Parameter) |>
      mutate(Season=factor(
        ifelse(Month<=3,'Winter',ifelse(Month<=6,'Spring',ifelse(Month<=9,'Summer','Fall'))),
        levels=c('Fall','Winter','Spring','Summer')),
             Result=ifelse(Result==0|is.na(Result),1,Result)) |> #assume 1 CFU/100 mL for non-detect for this summary
      group_by(Parameter,Season) |>
      summarise(GM=exp(mean(log(Result))),
                p90=ifelse(n()<=10,max(Result),quantile(Result,p=.9)),.groups='drop') |>
      left_join(bacteria_criteria,by='Parameter')|>
      mutate(Season,
                GM_Violation=GM>GeoMean_Criteria,
                STV_Violation=p90>STV_Criteria) %>%
      summarise(nViolation=length(which(GM_Violation|STV_Violation)),
                Notes=ifelse(length(which(GM_Violation))==0&length(which(STV_Violation))==0,'No Violations',
                             paste(ifelse(length(which(GM_Violation))==0,'No Geometric Mean Violations and',
                                    paste('Geometric Mean Violations in',Season[GM_Violation],'and')),
                                   ifelse(length(which(STV_Violation))==0,'No STV Violations',
                                          paste('STV Violations in',Season[STV_Violation]))
                             )
      ))
  }

return(wqc_out)  
}

###test
library(purrr)
library(tidyr)
streams_wq_dat %>%
  filter(SITE_CODE=='EH-TOTKE0000') %>%
  mutate(AquaticLifeUse='Core Summer Salmonid Habitat') %>%
  filter(parameter %in% c('Temperature, water','Dissolved Oxygen','pH','E. coli','Fecal Coliform')) %>%
  group_by(SITE_CODE,WaterYear,AquaticLifeUse,parameter) %>%
  nest() %>%
  mutate(WQC_Output=pmap(list(.x=data,parameter=parameter,AquaticLifeUse=AquaticLifeUse),.f=~{
    wqc_function(AquaticLifeUse=AquaticLifeUse,
                 Month=.x$Month,
                 Parameter=parameter,
                 Result=.x$value
                 )
  })) %>%
  select(-data) %>%
  unnest(WQC_Output)
