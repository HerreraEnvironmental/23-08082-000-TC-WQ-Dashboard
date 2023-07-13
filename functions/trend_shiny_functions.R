#Trend Plot and text summary for water quality data


trend_plot<-function(dataSubset,input){
  
  trendplot<-dataSubset %>%
      ggplot(aes(x=DateTime,y=value))+
      geom_point(data=~filter(.x,WaterYear==input$data_year),col='red',size=4)+
      geom_point()+
      geom_smooth(data=~filter(.x,WaterYear>=input$trend_years[1]&WaterYear<=input$trend_years[2]),se=F)+
      theme_bw()+
      scale_y_continuous(input$trend_parm)

if(input$trend_parm=='Temperature, water'){
  temp_criteria<-wqc_finder(unique(dataSubset()$AquaticLifeUse),input$trend_parm)
  trendplot<-trendplot+
    geom_hline(yintercept = temp_criteria)
}

if(input$trend_parm=='Dissolved Oxygen'){
  do_criteria<-wqc_finder(unique(dataSubset()$AquaticLifeUse),input$trend_parm)
  trendplot<-trendplot+
    geom_hline(yintercept = do_criteria)
}

if(input$trend_parm=='pH'){
  trendplot<-trendplot+
    geom_hline(yintercept = c(6.5,8.5))
}

if(input$trend_parm=='Fecal Coliform'){
  trendplot<-trendplot+
    geom_hline(yintercept = c(100,200))
}
if(input$trend_parm=='E. coli'){
  trendplot<-trendplot+
    geom_hline(yintercept = c(100,320))
}

if(input$data_log_scale){
  trendplot<-trendplot+
    scale_y_log10(input$trend_parm,breaks=10^(-4:4),minor_breaks=log10_minor_break())
}

ggplotly(trendplot)
}


# trend_plot(dataSubset=
# streams_wq_dat %>%
#   filter(SITE_CODE=='05b'&
#            parameter=='Total Phosphorus')%>%
#   mutate(AquaticLifeUse='Core Summer Salmonid Habitat'),
# input=list(data_log_scale=F,trend_parm='Total Phosphorus',data_year=2022,
#            trend_years=c(2000,2022)))

trend_text<-function(dataSubset,input){
  trend_out<-dataSubset %>%
    filter(WaterYear>=input$trend_years[1]&WaterYear<=input$trend_years[2]) %>%
    with(.,rkt::rkt(WaterYear,newResultValue,Month,rep='a'))
  trend_unit<-unique(dataSubset$unit)[1]
  sigStatement<-ifelse(trend_out$sl<=0.05,'a  significant trend','insufficient evidence of a trend')
  slopeStatement<-ifelse(trend_out$sl<=0.05,paste('The trend slope is',trend_out$B,trend_unit,'per year'),'')
  HTML(paste0('<u>Mann-Kendall Trend Test:</u>','<br/>',
        'Between water years <b>',input$trend_years[1],'</b> and <b>',input$trend_years[2],'</b>',
        ' at ',input$main_site2,', there is ','<b>',sigStatement,"</b>",' in <b>',input$trend_parm,'</b><br/>',
        slopeStatement))
}
# 
# trend_text(dataSubset=
#              streams_wq_dat %>%
#              filter(SITE_CODE=='05b'&
#                       parameter=='Total Phosphorus')%>%
#              mutate(AquaticLifeUse='Core Summer Salmonid Habitat'),
#            input=list(data_log_scale=F,trend_parm='Total Phosphorus',data_year=2022,
#                       trend_years=c(2000,2022)))
