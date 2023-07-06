withinYear_plot<-function(dataSubset,input){
  dataplot<-dataSubset %>%
    ggplot(aes(x=WY_FakeDate,y=value,group=WaterYear))+
    geom_point(alpha=.2)+
    geom_path(data=~.x %>% filter(WaterYear==input$data_year))+
    geom_point(data=~.x %>% filter(WaterYear==input$data_year))+
    theme_bw()+
    scale_x_date('',date_breaks = '2 months',date_labels = '%b',
                 #limits=as.Date(c('1999-9-25','2000-10-05')),
                 date_minor_breaks = '1 month')+
    scale_y_continuous(input$trend_parm)
  
  if(input$trend_parm=='Temperature, water'){
    temp_criteria<-wqc_finder(unique(dataSubset$AquaticLifeUse),input$trend_parm)
    dataplot<-dataplot+
      geom_hline(yintercept = temp_criteria)
  }
  
  if(input$trend_parm=='Dissolved Oxygen'){
    do_criteria<-wqc_finder(unique(dataSubset$AquaticLifeUse),input$trend_parm)
    dataplot<-dataplot+
      geom_hline(yintercept = do_criteria)
  }
  
  if(input$trend_parm=='pH'){
    dataplot<-dataplot+
      geom_hline(yintercept = c(6.5,8.5))
  }
  
  if(input$trend_parm=='Fecal Coliform'){
    dataplot<-dataplot+
      geom_hline(yintercept = c(100,200))
  }
  if(input$trend_parm=='E. coli'){
    dataplot<-dataplot+
      geom_hline(yintercept = c(100,320))
  }
  
  if(input$data_log_scale){
    dataplot<-dataplot+
      scale_y_log10(input$trend_parm,breaks=10^(-4:4),minor_breaks=log10_minor_break())
  }
  
  ggplotly(dataplot)
}

withinYear_plot(dataSubset=
             streams_wq_dat %>%
             filter(SITE_CODE=='05b'&
                      parameter=='Total Phosphorus')%>%
             mutate(AquaticLifeUse='Core Summer Salmonid Habitat'),
           input=list(data_log_scale=F,trend_parm='Total Phosphorus',data_year=2022,
                      trend_years=c(2000,2022)))
