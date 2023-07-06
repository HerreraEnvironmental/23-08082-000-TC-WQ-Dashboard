
wqc_comparison<-function(streams_wq_dat,input){
  streams_wq_dat %>%
    filter(WaterYear==input$wqc_sum_year&
             parameter %in% c('Temperature, water','Dissolved Oxygen','pH','E. coli','Fecal Coliform'))%>%
    mutate(AquaticLifeUse='Core Summer Salmonid Habitat') %>% ### NEED TO UPDATE WITH LOOKUP TABLE
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
pal_WQC<-colorFactor(c('darkred','darkgreen','grey'),levels=c('TRUE',"FALSE",NA))

wqc_map_out %>%
  group_by(SITE_CODE) %>%
  mutate(nViolation=sum(nViolation),
         parameter=factor(parameter,levels=c('Temperature, water','Dissolved Oxygen','pH','E. coli','Fecal Coliform'))) %>%
  pivot_wider(names_from=parameter,values_from = Notes,names_expand = T,
              values_fn=~ ifelse(is.na(.x),'',.x),
              values_fill='Not Measured') %>%
  summarise(Violation=nViolation>0,
            Text=paste0(unique(AquaticLifeUse),'<br>',
                        'Temperature: ',`Temperature, water`,'<br>',
                        'Dissolved Oxygen: ', `Dissolved Oxygen`,'<br>',
                        'pH: ', `pH`,'<br>',
                        'Fecal Coliform: ', `Fecal Coliform`,'<br>',
                        'E. coli: ', `E. coli`)) %>%
  left_join(streams_sites) %>%
  leaflet() %>%
  addCircleMarkers(color=~pal_WQC(Violation),fillOpacity = 0.9,weight=1,
                   popup=~paste0("<h5>", "<b>", SITE_NAME,'<br>', "</b>","</h5>",'<br>',
                                 SITE_CODE,'<br>',
                                 Text),
                   layerId= ~SITE_CODE,
                   label = ~SITE_CODE) %>%
  addProviderTiles('Esri.NatGeoWorldMap') 
}

# wqc_map(wqc_map_out = wqc_comparison(streams_wq_dat,input=list(wqc_sum_year=2022)),
#         stream_sites)

wqc_table<-function(wqc_map_out,input){
  wqc_map_out %>%
  mutate(parameter=factor(parameter,levels=c('Temperature, water','Dissolved Oxygen','pH','E. coli','Fecal Coliform'))) %>%
  group_by(parameter) %>%
  summarise(`Number of Sites with Violations`=length(which(nViolation>0)),
            `Number of Site Monitored`=length(nViolation)) %>%
  complete(parameter,fill=list(`Number of Sites with Violations`=NA,`Number of Site Monitored`=0))
}

#wqc_table(wqc_map_out = wqc_comparison(streams_wq_dat,input=list(wqc_sum_year=2022)))
