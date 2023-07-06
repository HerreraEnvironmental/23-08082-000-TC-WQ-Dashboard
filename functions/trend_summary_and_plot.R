#set a limit for number of samples for selected time period
#let's go for 4
#if((input$trend_summary_years[2]-input$trend_summary_years[1])<4) "Please select at least 4 years" else{

trend_summary_func<-function(streams_wq_dat,input){

temp_trend_data<-streams_wq_dat %>%
  filter(WaterYear>=input$trend_summary_years[1]&WaterYear<=input$trend_summary_years[2]&
           parameter==input$trend_summary_parm&
           SITE_CODE %in% input$main_site3) 
temp_trend_data %>%
  group_by(SITE_CODE,parameter) %>%
  nest() %>%
  mutate(MK_Out=map(.x=data,.f=~{
    mk_out<-with(.x,rkt::rkt(WaterYear,newResultValue,Month,rep='a'))
    tibble(p=mk_out$sl,
           Slope=mk_out$B) %>%
      mutate(Statement=ifelse(is.na(Slope),'Test Not Run - insufficient data',
                              ifelse(p>0.05|Slope==0,'No Significant Trend',ifelse(Slope>0,'Increasing Trend','Decreasing Trend'))))
  })) %>%
  select(-data) %>%
  unnest(MK_Out)%>%
  mutate(Statement=factor(Statement,levels=rev(c('Decreasing Trend','Test Not Run - insufficient data',
                                                 'No Significant Trend','Increasing Trend'))),
         StartYear=max(input$trend_summary_years[1],min(temp_trend_data$WaterYear[SITE_CODE==temp_trend_data$SITE_CODE])),
         EndYear=min(input$trend_summary_years[2],max(temp_trend_data$WaterYear[SITE_CODE==temp_trend_data$SITE_CODE]))) 
}

# trend_summary_func(streams_wq_dat,
#                    input=list(trend_summary_years=c(2000,2020),
#                               trend_summary_parm='Total Phosphorus',
#                               main_site3='05b'))

trend_summary_plot<-function(trend_summary,input){
plot<-trend_summary %>%
  ggplot(aes(x=parameter,fill=Statement))+
  geom_bar(position='stack')+
  scale_fill_manual(values=c('Decreasing Trend'='blue','Test Not Run - insufficient data'='darkgrey',
                             'No Significant Trend'='lightgrey','Increasing Trend'='red'))+
  theme_bw()+
  theme(#axis.text.x = element_text(angle=45,hjust=1),
    legend.position = 'bottom')+
  coord_flip()+xlab('')

ggplotly(plot)
}
trend_summary_plot(
  trend_summary = trend_summary_func(streams_wq_dat,
                                     input=list(trend_summary_years=c(2000,2020),
                                                trend_summary_parm='Total Phosphorus',
                                                main_site3='05b'))
)
