# WQ Criteria Functions ---------------------------------------------------



wqc_finder<-function(AquaticLifeUse=c('Char Spawning and Rearing',
                                      'Core Summer Salmonid Habitat',
                                      'Salmonid Spawning, Rearing, and Migration',
                                      'Salmonid Rearing and Migration Only',
                                      'Nonanadromous Interior Redband Trout',
                                      'Indigenous Warm Water Species',
                                      'Marine'),Parameter){
  AquaticLifeUse<-AquaticLifeUse[1]
  aquatic_life_uses<-tibble(
    AquaticLifeUse=c('Char Spawning and Rearing',
                     'Core Summer Salmonid Habitat',
                     'Salmonid Spawning, Rearing, and Migration',
                     'Salmonid Rearing and Migration Only',
                     'Nonanadromous Interior Redband Trout',
                     'Indigenous Warm Water Species',
                     'Marine'),
    Temp_7DADMax_C=c(12,
                     16,
                     17.5,
                     17.5,
                     18,
                     20,
                     13),
    DO_DailyMin_mgL=c(10,
                      10,
                      10,
                      6.5,
                      10,
                      6.5,
                      7),
    DO_Saturation=c(90,
                    95,
                    90,
                    NA,
                    90,
                    NA,
                    NA),
    Turbidity=NA,
    TotDissolvedGas=110,
    pH_min=6.5,
    pH_max=8.5
  )
  
  if(Parameter %in% c('Temperature','Temperature, water','Water Temperature (°C)')){
    return(aquatic_life_uses$Temp_7DADMax_C[AquaticLifeUse==aquatic_life_uses$AquaticLifeUse])
  }
  if(Parameter %in% c('Dissolved Oxygen','Dissolved Oxygen, Field')){
    return(aquatic_life_uses$DO_DailyMin_mgL[AquaticLifeUse==aquatic_life_uses$AquaticLifeUse])
  }
  if(Parameter %in% c('Dissolved Oxygen Saturation')){
    return(aquatic_life_uses$DO_Saturation[AquaticLifeUse==aquatic_life_uses$AquaticLifeUse])
  }
  if(Parameter %in% c('Total Dissolved Gas')){
    return(aquatic_life_uses$TotDissolvedGas[AquaticLifeUse==aquatic_life_uses$AquaticLifeUse])
  }
  if(Parameter %in% c('pH')){
    return(c(6.5,8.5))
  }
  #Ambient water quality samples: When averaging bacteria sample values for comparison to the geometric mean criteria, 
  #it is preferable to average by season. The averaging period of bacteria sample data shall be 90 days or less.
  #STV no more than 10 percent of samples in averaging period (or maximum if less than 10 samples)
  if(Parameter %in% c('E. coli')){
    return(c(100,320))
  }
  if(Parameter %in% c('Fecal Coliform')){
    return(c(100,200))
  }
}


wqc_function<-function(AquaticLifeUse=c("Char Spawning and Rearing", "Core Summer Salmonid Habitat", 
                                        "Salmonid Spawning, Rearing, and Migration", "Salmonid Rearing and Migration Only", 
                                        "Nonanadromous Interior Redband Trout", "Indigenous Warm Water Species","Marine"),
                       Month,Parameter,Result){
  AquaticLifeUse<-AquaticLifeUse[1]
  #define look up table for designated aquatic life uses
  #note that temperature and dissolved oxygen for lakes are not absolute thresholds are based on relative change from natty conditions
  
  
  if(!all(AquaticLifeUse %in% c("Char Spawning and Rearing", "Core Summer Salmonid Habitat", 
                                "Salmonid Spawning, Rearing, and Migration", "Salmonid Rearing and Migration Only", 
                                "Nonanadromous Interior Redband Trout", "Indigenous Warm Water Species","Marine"))) stop('Must select Aquatic Life Use')
  
  wqc_out<-NA
  
  if(Parameter %in% c('Temperature','Temperature, water','Water Temperature (°C)')){
    wqc_out<- tibble(nViolation=length(which(Result>wqc_finder(AquaticLifeUse,Parameter)))) %>%
      mutate(Notes=paste0(nViolation,' violations in ',length(Result), ' samples'))
    
  }
  
  if(Parameter %in% c('Dissolved Oxygen','Dissolved Oxygen, Field')){
    wqc_out<-tibble(nViolation=length(which(Result<wqc_finder(AquaticLifeUse,Parameter)))) %>%
      mutate(Notes=paste0(nViolation,' violations in ',length(Result), ' samples'))
    
  }
  
  if(Parameter %in% c('Dissolved Oxygen Saturation')){
    wqc_out<- tibble(nViolation=length(which(Result<wqc_finder(AquaticLifeUse,Parameter)))) %>%
      mutate(Notes=paste0(nViolation,' violations in ',length(Result), ' samples'))
    
  }
  
  if(Parameter %in% c('pH')){
    wqc_out<-tibble(nViolation=length(which(Result>wqc_finder(AquaticLifeUse,Parameter)[2]&
                                              Result<wqc_finder(AquaticLifeUse,Parameter)[1]))) %>%
      mutate(Notes=paste0(nViolation,' violations in ',length(Result), ' samples'))
    
  }
  
  if(Parameter %in% c('Total Dissolved Gas')){
    wqc_out<-tibble(nViolation=length(which(Result>wqc_finder(AquaticLifeUse,Parameter)))) %>%
      mutate(Notes=paste0(nViolation,' violations in ',length(Result), ' samples'))
  }
  
  if(Parameter %in% c('E. coli','Fecal Coliform')){
    wqc_out<-tibble(Month=Month,
                    Result=Result) |>
      mutate(Season=factor(
        ifelse(Month<=3,'Winter',ifelse(Month<=6,'Spring',ifelse(Month<=9,'Summer','Fall'))),
        levels=c('Fall','Winter','Spring','Summer')),
        Result=ifelse(Result==0|is.na(Result),1,Result)) |> #assume 1 CFU/100 mL for non-detect for this summary
      group_by(Season) |>
      summarise(GM=exp(mean(log(Result))),
                p90=ifelse(n()<=10,max(Result),quantile(Result,p=.9)),.groups='drop') |>
      mutate(Season,
             GM_Violation=GM>wqc_finder(AquaticLifeUse,Parameter)[1],
             STV_Violation=p90>wqc_finder(AquaticLifeUse,Parameter)[2]) %>%
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
