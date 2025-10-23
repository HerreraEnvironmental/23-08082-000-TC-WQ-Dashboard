# Map of all monitoring sites in leaflet
# Requires: tidyverse, leaflet

site_map <- function(recent_streams_data, input) {
  leaflet(recent_streams_data) %>%
    addMarkers(
      popup = ~ paste0(
        "<h5><b>", SITE_NAME, "</b></h5>",
        "<h6><i>Last Sampled on ", as.Date(DateTime, "%Y-%m-%d"), "</i><br><br>",
        # Optional image:
        # '<img src="https://thumbs.dreamstime.com/z/deep-forest-stream-crystal-clear-water-sunshine-plitvice-lakes-croatia-41645592.jpg" width="250" height="250"/>',
        "<hr>",
        "Most Recent WQI Score of <b>", round(WQI, 0), "</b> in ", WaterYear, " (", Rating, ")<br><br>",
        "<b>Click to do the following:</b><ul>",
        "<li><a href='#' onclick=openTab('wqi')>View Recent Water Quality Index</a></li>",
        "<li><a href='#' onclick=openTab('all_data')>View Water Quality Trends and All Data for This Station</a></li>",
        "<li><a href='#' onclick=openTab('data_download')>Download All Data for This Station</a></li>",
        "</ul></h6>"
      ),
      layerId = ~SITE_CODE,
      label = ~SITE_NAME
    ) %>%
    # addPolygons(data = ThurstonCo_WA,
    #            fillColor = "")%>%
    leaflet.esri::addEsriFeatureLayer(
      "https://map.co.thurston.wa.us/arcgis/rest/services/Thurston/Thurston_Watersheds/FeatureServer/0",
      useServiceSymbology = TRUE,
      fillColor = "lightblue",
      stroke = 1,
      color = "black",
      popupProperty = JS(paste0(
        "function(feature) {",
        " return L.Util.template(",
        " \"<b>Watershed: {Watershed}</b>",
        " <p>This watershed drains to {Drainage}</p>",
        " \",",
        " feature.properties",
        " );",
        "}"
      ))
    ) %>%
    addProviderTiles("Esri.NatGeoWorldMap")
}
