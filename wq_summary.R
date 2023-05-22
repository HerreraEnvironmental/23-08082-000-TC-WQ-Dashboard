##summary statistics
library(NADA2)
library(dplyr)
streams_wq_dat<-readRDS('outputs/streams_wq_dat.RDS')

watershed_wq_summary<-streams_wq_dat %>%
  filter(!is.na(newResultValue)&
           WaterYear==2022) %>%
  group_by(SITE_CODE,parameter) %>%
  summarise(n=n(),
            Detects=length(which(!nonDetectFlag)),
            FOD=paste0(Detects,'/',n),
            KM_Mean=ifelse(Detects/n<0.4,NA,
                           cfit(newResultValue,nonDetectFlag,Cdf=F,printstat = F)['KMmean'] %>% unlist()),
            KM_SD=ifelse(Detects/n<0.4,NA,
                         cfit(newResultValue,nonDetectFlag,Cdf=F,printstat = F)['KMsd'] %>% unlist()),
            KM_Median=ifelse(Detects/n<0.4,NA,
                             cfit(newResultValue,nonDetectFlag,Cdf=F,printstat = F)['KMmedian'] %>% unlist %>% as.numeric),
            MDL=paste(unique(mdl[nonDetectFlag]),collapse=','),
            Min=min(newResultValue),
            Max=max(newResultValue)
  )


