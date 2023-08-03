#Map of all monitoring sites in leaflet

site_map<-function(recent_streams_data,input){
leaflet(recent_streams_data) %>%
  addMarkers(popup=~paste0("<h5>", "<b>", SITE_NAME,'<br>', "</b>","</h5>",
                           "<h6>", "<i>", "Last Sampled on ", as.Date(DateTime, "%Y-%M-%d"), "</i>","<br>",
                           "<br>",
                           #can insert photo
                           #   '<img src="https://thumbs.dreamstime.com/z/deep-forest-stream-crystal-clear-water-sunshine-plitvice-lakes-croatia-41645592.jpg" width="250px" height="250px"/>',
                           "<hr>",
                           "Most Recent WQI Score of ", "<b>", as.character(round(WQI,0)),' in ',WaterYear,' (',Rating,')', "</b>", "</h6>",
                         #  "<hr>"),
                           "<h6>", "<b>", "Click to do the following:", "</b>","<ul>","<br>",
                           "<li>", "<a href='#' onclick=","openTab('wqi')>",'View Recent Water Quality Index','</a>', "<br>","</li>",
                           "<li>", "<a href='#' onclick=","openTab('all_data')>",'View Water Quality trends and all data for this station','</a>', "<br>","</li>",
                           "<li>", "<a href='#' onclick=","openTab('data_download')>",'Download all data for this station', '</a>', "<br>","</li>",
                           "</ul>","</h6>"),
             layerId= ~SITE_CODE,
             label = ~SITE_CODE) %>%
  #addPolygons(data = ThurstonCo_WA,
  #            fillColor = "")%>%
  addProviderTiles('Esri.NatGeoWorldMap')
}

#site_map(recent_streams_data)
