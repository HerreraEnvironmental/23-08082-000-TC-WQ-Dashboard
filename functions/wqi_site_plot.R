#wqi site plot

wqi_annual_plot<-function(annual_wqi,input){
annual_wqi %>%
  filter(site==input$main_site) %>%
  ggplot(aes(x=WaterYear,y=WQI))+
  geom_point(data=~filter(.x,WaterYear==input$wqi_year),col='red',size=4)+
  geom_point()+
  geom_smooth(data=~filter(.x,WaterYear>=input$wqi_trend_years[1]&WaterYear<=input$wqi_trend_years[2]),se=F)+
  theme_bw()+
  xlab('Water year')+
  scale_y_continuous('Water Quality Index',limits = c(0,100))
}

monthly_wqi_plot<-function(monthly_wqi_by_parameter,input){
  monthly_wqi_ggplot<-monthly_wqi_by_parameter %>%
  filter(site==input$main_site&WaterYear==input$wqi_year) %>%
  tidyr::pivot_longer(cols=c(FC,Oxygen,pH,Temp,Sediment,Nutrient),names_to = 'shortParmName',values_to = 'WQI') %>%
  select(site,WaterYear,Month,shortParmName,WQI) %>%
  mutate(Month=factor(Month,c(10:12,1:9),labels=month.abb[c(10:12,1:9)])) %>%
  #ggplot()+
  ggplot(aes(x=Month,y=WQI,col=shortParmName))+
  geom_line(alpha=.5,aes(group=shortParmName))+
  geom_point(alpha=.5)+
  geom_line(data=filter(monthly_wqi,site==input$main_site&WaterYear==input$wqi_year)%>%
              mutate(Month=factor(Month,c(10:12,1:9),labels=month.abb[c(10:12,1:9)])) ,
            aes(x=Month,y=WQI),group=1,
            col='black')+
  geom_point(data=filter(monthly_wqi,site==input$main_site&WaterYear==input$wqi_year)%>%
               mutate(Month=factor(Month,c(10:12,1:9),labels=month.abb[c(10:12,1:9)])) ,
             aes(x=Month,y=WQI),
             col='black')+
  theme_bw()+
  xlab('Month')+
  scale_y_continuous('Water Quality Index',limits = c(0,100))
ggplotly(monthly_wqi_ggplot) %>%
  layout(legend=list(title=list(text='')))
}