#TDA FUNCTION

#TDA using seasonal mann kendall
#calculate S value and var(s) for each month, then sum to calculate likelihood of increasing/decreasing slope
#following methods from McBride 2019

library(dplyr)
library(tidyr)
library(purrr)

source('tda_helper_functions.R')

streams_wq_dat<-readRDS('outputs/streams_wq_dat.RDS')

#running ats slopes and the entire monthly seasonall mannkendall (to work towards probability of trend)
#can take a long while
site_parm_medians<-streams_wq_dat %>%
  filter(!is.na(newResultValue)) %>%
  select(SITE_CODE,WaterYear,Parameter=parameter,Conc=newResultValue,Month=Month,nonDetectFlag) %>%
  arrange(SITE_CODE,WaterYear,Parameter) %>%
  group_by(SITE_CODE,Parameter) %>%
  mutate(YearsMonitored=length(unique(WaterYear)),
         YearsRange=toString(gsub(":", "-",
                                  as.character(split(as.integer(WaterYear), 
                                                     cumsum(c(TRUE, diff(WaterYear) != 1))))))
  )  %>%
  filter(YearsMonitored>=5) %>%
  group_by(SITE_CODE,Parameter) %>%
  summarise(nDetects=length(which(!nonDetectFlag)),
            n=n(),
            MedianValue=ifelse(Parameter=='Temperature, water'|nDetects<3,median(Conc),
                               ifelse(is.character(
                                 NADA2::cfit(ifelse(Conc==0,0.1,Conc),nonDetectFlag,Cdf=F,printstat = F)$KMmedian),
                                 median(Conc), #take median detection limit
                                 NADA2::cfit(ifelse(Conc==0,0.1,Conc),nonDetectFlag,Cdf=F,printstat = F)$KMmedian)
  )[1]
  )


mk_out_monthly_overall<-streams_wq_dat %>%
  filter(!is.na(newResultValue)) %>%
  #filter(SITE_CODE=='10a') %>%
  select(SITE_CODE,WaterYear,Parameter=parameter,Conc=newResultValue,Month=Month,nonDetectFlag) %>%
  arrange(SITE_CODE,WaterYear,Parameter) %>%
  group_by(SITE_CODE,Parameter) %>%
  mutate(YearsMonitored=length(unique(WaterYear)),
         YearsRange=toString(gsub(":", "-",
                                  as.character(split(as.integer(unique(WaterYear)), 
                                                     cumsum(c(TRUE, diff(unique(WaterYear)) != 1))))))
  ) %>%
  filter(YearsMonitored>=5) %>%
  group_by(SITE_CODE,Parameter,Month,YearsMonitored,YearsRange) %>%
  nest() %>%
  mutate(MK_Out=map2(.x=data,.y=Parameter,.f=~{
    if(nrow(.x)>=4){
      tempData=.x 
      
      imp_function_out<-with(tempData,imp_function(x=WaterYear,y=Conc,ycen=nonDetectFlag,iter=100))
      slopes=imp_function_out$ave_slopes
      rank_slope_zero<-imp_function_out$rank_slope_zero
      z1a_zero<-imp_function_out$z1a_zero
      
      VarS=imp_function_out$VarS
      
      data.frame(
        Slopes=slopes,VarS=VarS,rank_slope_zero=rank_slope_zero,z1a_zero=z1a_zero
      )
    } else{data.frame(Slopes=NA,VarS=0,rank_slope_zero=NA,z1a_zero=NA
    )}
  })) %>%
  select(-data) %>%
  unnest(MK_Out) %>%
  summarise(Slopes1=list(Slopes),
            VarS=unique(VarS),
            n_slopes=length(which(!is.na(Slopes))),
            rank_slope_zero=unique(rank_slope_zero),
            z1a_zero=unique(z1a_zero),
            neg.prob=round(1-pnorm(z1a_zero),4),
            pos.prob=round(pnorm(z1a_zero),4),
  ) %>%
  group_by(SITE_CODE,Parameter,YearsMonitored,YearsRange) %>%
  summarise(Slope=median(unlist(c(Slopes1)),na.rm=T),
            z1a_zero_all=(sum(rank_slope_zero,na.rm=T)*2-sum(n_slopes))/-sqrt(sum(VarS,na.rm=T)),
            Neg_Like=round(1-pnorm(z1a_zero_all),4),
            Pos_Like=round(pnorm(z1a_zero_all),4), 
  ) %>%
  left_join(site_parm_medians) %>%
  mutate(RelSlope=100*Slope/MedianValue) %>%
  mutate(Statement=ifelse(Pos_Like>.95,'Increasing - Highly Likely',
                          ifelse(Pos_Like>.9,'Increasing - Very Likely',
                                 ifelse(Pos_Like>2/3,'Increasing - Likely',
                                        ifelse(Pos_Like>=1/3,'Trend as Likely as Not',
                                               ifelse(Pos_Like>=.1,'Decreasing - Likely',
                                                      ifelse(Pos_Like>=.05,'Decreasing - Very Likely',
                                                             'Decreasing - Highly Likely')))))))

library(mgcv)

#use log-transformed data for nutrients, tss, turbidity, fecal bacteria
norm_parms<-c('Temperature, water','Dissolved Oxygen','Specific Conductivity (at 25 deg C)','pH')

water_year_summary<-streams_wq_dat %>%
  select(SITE_CODE,WaterYear,Parameter=parameter,Conc=newResultValue,Month=Month,nonDetectFlag) %>%
  arrange(SITE_CODE,WaterYear,Parameter) %>%
  group_by(SITE_CODE,Parameter) %>%
  mutate(YearsMonitored=length(unique(WaterYear)),
         YearsRange=toString(gsub(":", "-",
                                  as.character(split(as.integer(WaterYear), cumsum(c(TRUE, diff(WaterYear) != 1))))))
  )%>%
  filter(YearsMonitored>=5) %>%

  group_by(SITE_CODE,Parameter,WaterYear) %>%
  mutate(SamplesInYear=n()) %>%
  filter(SamplesInYear>=6) %>% # at least 6 samples in eyar
  summarise(
    MeanValue=ifelse(Parameter=='Temperature',mean(Conc),
                     ifelse(length(which(!nonDetectFlag))<=2,
                            mean(Conc), #take mean detection limit
                            NADA2::cfit(ifelse(Conc==0,0.1,Conc),nonDetectFlag,Cdf=F,printstat = F)$KMmean)
    )[1],
    nonDetectFlag=length(which(nonDetectFlag)),
    SDValue=ifelse(Parameter=='Temperature',sd(Conc),
                   ifelse(length(which(!nonDetectFlag))<=2,
                          sd(Conc), #take sd detection limit
                          NADA2::cfit(ifelse(Conc==0,0.1,Conc),nonDetectFlag,Cdf=F,printstat = F)$KMsd)
    )[1]
  ) %>%
  mutate(YearsMonitored=length(unique(WaterYear)),
         YearsRange=toString(gsub(":", "-",
                                  as.character(split(as.integer(WaterYear), cumsum(c(TRUE, diff(WaterYear) != 1))))))
         #YearsRange=paste(range(WaterYear),collapse='-')
  ) %>%
  filter(YearsMonitored>=5)

#use general additive model to smooth data between years,
#use smoothed, detreneded data to calculate standard deviation
detrended_data<-water_year_summary %>%
  group_by(SITE_CODE,Parameter) %>%
  ###detrend annual means
  nest() %>%
  mutate(detrend.out=map2(data,Parameter,~{
    n_year=unique(.x$YearsMonitored)
    if(.y %in% norm_parms){
      gam(MeanValue~s(WaterYear,k=round(n_year/2))
          ,data=.x,method='REML')
    } else{
      gam(log(MeanValue)~s(WaterYear,k=round(n_year/2))
          ,data=.x,method='REML')
    }}),
    detrended.values=map2(detrend.out,Parameter,~{
      if(.y %in% norm_parms){
        # detrended.values=mean(.x$data$MeanValue)+resid(.x)
        detrended.values=mean(.x$model[,1])+resid(.x)
        smoothed.values<-fitted(.x)
      } else{
        # detrended.values=exp(mean(log(.x$data$MeanValue))+resid(.x))
        detrended.values=exp(mean(.x$model[,1])+resid(.x))
        smoothed.values<-exp(fitted(.x))
      }
      data.frame(Smoothed.Value=smoothed.values,
                 Detrended.Value=detrended.values)
    }
    )) %>%
  unnest(c(data,detrended.values))


#calculate long-term mean and SD thresholds, calculate mean of the last 5 years monitored (this can have gaps!)
#nutrients, tss, turbidity, and fecal coliform means & SDs are based on a log-transformation (geometric)
overall_ave<-detrended_data%>%
  group_by(SITE_CODE,Parameter,YearsRange) %>%
  summarise(
    ArithMean=mean(MeanValue),
    SD=sd(Detrended.Value),
    MeanLog=mean(log(Detrended.Value)),
    SDLog=sd(log(Detrended.Value)),
    Mean=ifelse(unique(Parameter) %in% norm_parms,
                ArithMean,
                exp(MeanLog)),
    Last5Mean=ifelse(unique(Parameter) %in% norm_parms,
                     mean(MeanValue[(length(MeanValue)-5):length(MeanValue)]),
                     exp(mean(log(MeanValue[(length(MeanValue)-5):length(MeanValue)]))))) %>%
  mutate(UpperThreshold_1SD=ifelse(Parameter %in% norm_parms,
                                   ArithMean+SD,exp(MeanLog+SDLog)),
         LowerThreshold_1SD=ifelse(Parameter %in% norm_parms,
                                   ArithMean-SD,exp(MeanLog-SDLog)),
         noise=ifelse(Parameter %in% norm_parms,
                      SD,sqrt((exp(SDLog^2)-1)*exp(2*MeanLog+SDLog^2)))) %>%
  mutate(Change=ifelse(Last5Mean>=UpperThreshold_1SD,'Meaningful Increase',
                       ifelse(Last5Mean<=LowerThreshold_1SD,'Meaningful Decrease','Maintaining'))) %>%
#  mutate(Parameter=factor(Parameter,levels=parm_order)) %>%
  filter(!is.na(Parameter))



#combine "meaingful" change results with Mann Kendall likelihood results
combined_trend_outputs<-overall_ave %>%
  left_join(mk_out_monthly_overall %>% select(-YearsRange)) %>%
  # select(SITE_CODE,Parameter,YearsMonitored,Change,Statement)%>%
  mutate(FinalStatement=ifelse(Change=='Maintaining','Maintaining',
                               ifelse(grepl('Meaningful',Change)&grepl(' - ',Statement),
                                      paste(Change,'- Trend',gsub('Increasing - |Decreasing - ','',Statement)),
                                      'Too Few Data'))) %>%
  mutate(FinalStatement=factor(FinalStatement,levels=
                                 c('Meaningful Increase - Trend Highly Likely',
                                   'Meaningful Increase - Trend Very Likely',
                                   'Meaningful Increase - Trend Likely',
                                   'Trend as Likely as Not',
                                   'Maintaining',
                                   'Meaningful Decrease - Trend Highly Likely',
                                   'Meaningful Decrease - Trend Very Likely',
                                   'Meaningful Decrease - Trend Likely',
                                   'Too Few Data'))) %>%
  mutate(SummaryStatement=factor(ifelse(grepl('Meaningful',FinalStatement),gsub('Highly |Very ','',FinalStatement),
                                        ifelse(Change=='Meaningful Increase','Meaningful Increase - Trend Not Likely',
                                               ifelse(Change=='Meaningful Decrease','Meaningful Decrease - Trend Not Likely',
                                                      ifelse(grepl('Not',Statement),'No Trend Likely',
                                                             ifelse(Slope>0,'Increasing Trend Likely but No Meaningful Change',
                                                                    'Decreasing Trend Likely but No Meaningful Change'))))),
                                 levels=c('Meaningful Increase - Trend Likely','Meaningful Increase - Trend Not Likely',
                                          'Increasing Trend Likely but No Meaningful Change','No Trend Likely',
                                          'Decreasing Trend Likely but No Meaningful Change',
                                          'Meaningful Decrease - Trend Likely','Meaningful Decrease - Trend Not Likely')))


library(ggplot2)
combined_trend_outputs %>%
  mutate(Parameter=factor(Parameter)) %>%
  group_by(Parameter,FinalStatement) %>%
  summarise(Count=n()) %>%
  mutate(newCount=ifelse(grepl('Increase',FinalStatement),Count,-1*Count)) %>%
  filter(FinalStatement!='Too Few Data'&FinalStatement!= 'Maintaining') %>%
  ggplot()+
  geom_bar(aes(y=Parameter,fill=FinalStatement,x=newCount),col='black',stat='identity',
           position=position_stack(reverse = T))+
  xlab('Decreasing Trends <-----> Increasing Trends')+
  scale_x_continuous(limits=c(-20,20),breaks=seq(-100,100,5),labels=abs(seq(-100,100,5)))+
  scale_fill_manual('',
                    values=c('darkblue','lightblue4','cyan','grey','grey','darkgreen','forestgreen','lightgreen',
                                       'black'),
                                       guide=guide_legend(nrow=2,byrow=T),drop=F)+
  theme_bw()+
  scale_y_discrete(limits = rev,drop=F)+
  theme(legend.position='bottom')

combined_trend_outputs %>%
  mutate(Parameter=factor(Parameter)) %>%
  group_by(Parameter,FinalStatement) %>%
  summarise(Count=n()) %>%
  mutate(newCount=ifelse(grepl('Increase',FinalStatement),Count,-1*Count)) %>%
 # filter(FinalStatement!='Too Few Data'&FinalStatement!= 'Maintaining') %>%
  ggplot()+
  geom_bar(aes(x=Parameter,fill=FinalStatement,y=Count),col='black',stat='identity',
           position=position_stack(reverse = T))+
#  xlab('Decreasing Trends <-----> Increasing Trends')+
 # scale_x_continuous(limits=c(-20,20),breaks=seq(-100,100,5),labels=abs(seq(-100,100,5)))+
  scale_fill_manual('',
                    values=c('darkblue','lightblue4','cyan','white','darkgrey','darkgreen','forestgreen','lightgreen',
                                       'black'),
                                       guide=guide_legend(nrow=2,byrow=T),drop=F)+
  theme_bw()+
#  scale_x_discrete(limits = rev,drop=F)+
  theme(legend.position='bottom',
        axis.text.x = element_text(angle=45,hjust=1))

