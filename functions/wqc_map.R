
#functions to compare to water quality criteria and map results
#also a summary table

wqc_comparison<-function(streams_wq_dat,streams_sites,input){
  streams_wq_dat %>%
    filter(WaterYear==input$wqc_sum_year&
             parameter %in% c('Water Temperature (°C)','Dissolved Oxygen',
                              'pH','E. coli','Fecal Coliform'))%>%
    left_join(streams_sites %>% select(SITE_CODE,AquaticLifeUse)) %>%
    group_by(SITE_CODE,AquaticLifeUse,parameter) %>%
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
}

#wqc_comparison(streams_wq_dat,input=list(wqc_sum_year=2022))


wqc_map<-function(wqc_map_out,stream_sites,input){
  pal_WQC<-colorFactor(c('darkred','darkgreen','grey'),levels=c('Violation(s)',"No Violation",'Not Measured'))
  
  wqc_parm<-input$wqc_sum_parm
  if(input$wqc_sum_parm=='All')  wqc_parm<-c('Water Temperature (°C)','Dissolved Oxygen','pH','E. coli','Fecal Coliform')
  
  wqc_map_out %>%
    group_by(SITE_CODE) %>%
    mutate(nViolation=ifelse(length(nViolation[parameter %in% wqc_parm])==0,NA,sum(nViolation[parameter %in% wqc_parm])),
           parameter=factor(parameter,levels=c('Water Temperature (°C)','Dissolved Oxygen','pH','E. coli','Fecal Coliform'))) %>%
    pivot_wider(names_from=parameter,values_from = Notes,names_expand = T,
                values_fn=~ ifelse(is.na(.x),'',.x),
                values_fill='Not Measured') %>%
    summarise(Violation=ifelse(is.na(nViolation),'Not Measured',ifelse(nViolation>0,'Violation(s)',"No Violation")),
              Text=paste0(unique(AquaticLifeUse),'<br>',
                          'Temperature: ',`Water Temperature (°C)`,'<br>',
                          'Dissolved Oxygen: ', `Dissolved Oxygen`,'<br>',
                          'pH: ', `pH`,'<br>',
                          'Fecal Coliform: ', `Fecal Coliform`,'<br>',
                          'E. coli: ', `E. coli`)) %>%
    left_join(streams_sites) %>%
    leaflet() %>%
    addCircleMarkers(fillColor=~pal_WQC(Violation),fillOpacity = 0.9,weight=1,
                     color='black',
                     popup=~paste0("<h6>", "<b>", SITE_NAME,'<br>', "</b>","</h6>",'<br>',
                                   SITE_CODE,'<br>',
                                   Text),
                     layerId= ~SITE_CODE,
                     label = ~SITE_NAME) %>%
    addProviderTiles('Esri.NatGeoWorldMap') %>%
    addLegend(pal=pal_WQC,
              values=factor(c('Violation(s)',"No Violation",'Not Measured'),levels=c('Violation(s)',"No Violation",'Not Measured')),
              title='Legend')
}

# wqc_map(wqc_map_out = wqc_comparison(streams_wq_dat,stream_sites,input=list(wqc_sum_year=2022)),
#         stream_sites)

wqc_table<-function(wqc_map_out,input){
  wqc_map_out %>%
    mutate(parameter=factor(parameter,levels=c('Water Temperature (°C)','Dissolved Oxygen','pH','E. coli','Fecal Coliform'))) %>%
    group_by(parameter) %>%
    summarise(`Number of Sites with Violations`=length(which(nViolation>0)),
              `Number of Site Monitored`=length(nViolation)) %>%
    complete(parameter,fill=list(`Number of Sites with Violations`=NA,`Number of Site Monitored`=0))
}

#wqc_table(wqc_map_out = wqc_comparison(streams_wq_dat,input=list(wqc_sum_year=2022)))
