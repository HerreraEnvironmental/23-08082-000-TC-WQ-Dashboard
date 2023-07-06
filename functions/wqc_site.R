#compare to water quality criteria for selected site

wqc_site<-function(streams_wq_data,input){
streams_wq_dat %>%
  filter(SITE_CODE==input$main_site&
           WaterYear==input$data_year) %>%
  mutate(AquaticLifeUse='Core Summer Salmonid Habitat') %>% ### NEED TO UPDATE WITH LOOKUP TABLE
  filter(parameter %in% c('Temperature, water','Dissolved Oxygen','pH','E. coli','Fecal Coliform'))%>%
  group_by(AquaticLifeUse,parameter) %>%
  nest() %>%
  mutate(WQC_Output=pmap(list(.x=data,parameter=parameter,AquaticLifeUse=AquaticLifeUse),.f=~{
    wqc_function(AquaticLifeUse=AquaticLifeUse,
                 Month=.x$Month,
                 Parameter=parameter,
                 Result=.x$value
    )
  })) %>%
  ungroup() %>%
  select(-data,-AquaticLifeUse) %>%
  unnest(WQC_Output)%>%
  mutate(parameter=factor(parameter,levels=c('Temperature, water','Dissolved Oxygen','pH','E. coli','Fecal Coliform'))) %>%
  complete(parameter,fill=list(nViolation=NA,Notes='No Data'))
}

#wqc_site(streams_wq_data,input=list(main_site='05b',data_year=2022))
