#map and summary plot of all WQI data

wqi_map<-function(streams_sites,annual_wqi,input){

  pal<-colorFactor(c('darkgreen','gold','darkred','grey'),levels=c('Good',"Moderate",'Poor',NA))
  
  streams_sites %>%
    left_join(annual_wqi %>%filter(WaterYear==input$wqi_sum_year),by=c('SITE_CODE'='site')) %>%
    mutate(Category=ifelse(WQI>=80,'Good',ifelse(WQI>=40,"Moderate",'Poor'))) %>%
    leaflet() %>%
    addCircleMarkers(color=~pal(Category),fillOpacity = 0.9,weight=1,
                     popup=~paste0("<h5>", "<b>", SITE_NAME,'<br>', "</b>","</h5>",
                                   "<hr>",
                                   "For WY",WaterYear,  ", the WQI score was ", "<b>", 
                                   round(WQI,0),"</b>"," and is considered ", "<b>", Rating, "</b>",".",  
                                   "<br>"
                                   
                     ),
                     layerId= ~SITE_CODE,
                     label = ~SITE_CODE) %>%
    addProviderTiles('Esri.NatGeoWorldMap') 
}

#wqi_map(streams_sites,annual_wqi,input=list(wqi_sum_year=2017))
wqi_summary_plot<-function(annual_wqi,input){
  cols <- c("Good" = "darkgreen", "Moderate" = "gold", "Poor" = "darkred")
  wqi_summary_plot<-annual_wqi %>%
    filter(WaterYear==input$wqi_sum_year) %>%
    ggplot(aes(x=Rating))+
    geom_bar(stat="count", aes(fill = Rating))+
    ylab("Number of stations")+
    scale_fill_manual(values = cols)+
    theme_bw()
  ggplotly(wqi_summary_plot)
}

#wqi_summary_plot(annual_wqi,input=list(wqi_sum_year=2017))
