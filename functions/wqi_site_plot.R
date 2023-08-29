#Water Quality Index plot for individual site. 
#Annual plot of summary WQI over time
#Monthly Plot for a selected year broken out by parameter


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

wqi_trend_text<-function(annual_wqi,input){
  wqi_trend_out<-annual_wqi %>%
    filter(site==input$main_site&
             WaterYear>=input$wqi_trend_years[1]&WaterYear<=input$wqi_trend_years[2]) %>%
    nest() %>%
    mutate(MK_Out=map(.x=data,.f=~{
      mk_out<-with(.x,rkt::rkt(WaterYear,WQI,rep='a'))
      tibble(p=mk_out$sl,
             Slope=mk_out$B) %>%
        mutate(Statement=ifelse(is.na(Slope),'Test Not Run - insufficient data',
                                ifelse(p>0.05|Slope==0,'No Significant Trend',ifelse(Slope>0,'Increasing Trend','Decreasing Trend'))))
    })) %>%
    select(-data) %>%
    unnest(MK_Out)%>%
    mutate(Statement=factor(Statement,levels=rev(c('Decreasing Trend','Test Not Run - insufficient data',
                                                   'No Significant Trend','Increasing Trend'))),
           StartYear=input$wqi_trend_years[1],
           EndYear=input$wqi_trend_years[2],
           sigStatement=paste0(ifelse(p<=0.05,'a  significant trend','insufficient evidence of a trend'),
                               ' (p',ifelse(p<0.001,'<0.001)',paste0('=',round(p,3),')'))),
           slopeStatement=ifelse(p<=0.05,paste('The trend slope is',round(Slope,4),'units per year'),'')
    )
  HTML(paste0('<u>Mann-Kendall Trend Test:</u>','<br/>',
              'Between water years <b>',wqi_trend_out$StartYear,'</b> and <b>',wqi_trend_out$EndYear,'</b>',
              ', there is ',ifelse(is.na(wqi_trend_out$p),'inadequate data to evalute a trend',
                                            paste0('<b>',wqi_trend_out$sigStatement,"</b>",' in <b>','WQI','</b><br/>',
              wqi_trend_out$slopeStatement))))
  }

monthly_wqi_plot<-function(monthly_wqi_by_parameter,monthly_wqi,input){
  monthly_wqi_ggplot<-monthly_wqi_by_parameter %>%
    filter(site==input$main_site&WaterYear==input$wqi_year) %>%
    left_join(monthly_wqi %>% rename(`Summary Score`=WQI)) %>%
  select(site, WaterYear, Month,`Summary Score`, Bacteria=FC, Oxygen, pH, Temperature=Temp, Sediment, Nutrient) %>%
  tidyr::pivot_longer(cols=-c(site:Month),names_to = 'Parameter',values_to = 'WQI') %>%
  select(site,WaterYear,Month,Parameter,WQI) %>%
  mutate(Parameter=factor(Parameter,levels=c('Summary Score','Temperature','Oxygen','pH','Bacteria',
                                             'Sediment','Nutrient')),
         Month=factor(Month,c(10:12,1:9),labels=month.abb[c(10:12,1:9)])) %>%
  #ggplot()+
  ggplot(aes(x=Month,y=WQI,col=Parameter))+
  geom_line(alpha=.75,aes(group=Parameter))+
  geom_point(alpha=.75)+
  theme_bw()+
  xlab('Month')+
  scale_y_continuous('Water Quality Index',limits = c(0,100))+
  scale_color_manual(values=c('Summary Score'='black','Temperature'='red','Oxygen'='blue','pH'='yellow',
                              'Bacteria'='brown','Sediment','rosybrown','Nutrient'='green'))
ggplotly(monthly_wqi_ggplot) %>%
  layout(legend=list(title=list(text='Parameter')))
}