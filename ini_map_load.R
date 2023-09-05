
m<-leaflet() %>%
  addProviderTiles(layerId = 'm_tiles',
                   providers$CartoDB.Positron,
                   options = providerTileOptions(opacity = .85)) %>%
  addPolygons(data = county_selected,
              layerId = ~paste(GEOID),
              color = cty_border_color,
              weight = .5,
              smoothFactor = 0.3,
              opacity = 0.4,
              fillOpacity = 0.1,
              label = cty_labels,
              labelOptions = labelOptions(
                style = list("front-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              highlightOptions = highlightOptions(
                weight = 2,
                color = "red",
                fillOpacity = 0.7,
                bringToFront = TRUE))

m_cs<-leaflet() %>%
  addProviderTiles(layerId = 'm_tiles_cs',
                   providers$CartoDB.Positron,
                   options = providerTileOptions(opacity = .85)) %>%
  setView(-99.0909, 39.8355, zoom = 4)%>%
  addPolygons(data = state_base, 
              color = st_border_color,
              weight = .5,
              smoothFactor = 0.3,
              opacity=0.4,
              fillOpacity = .1,
              label = state_base_labels,
              labelOptions = labelOptions(
                style = list("front-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = county_selected,
              layerId = 'base_county',
              color = cty_border_color,
              weight = .5,
              smoothFactor = 0.3,
              opacity = .4,
              fillOpacity = 0,
              label = cty_labels,
              labelOptions = labelOptions(
                style = list("front-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              highlightOptions = highlightOptions(
                weight = 2,
                color = "red",
                fillOpacity = 0,
                bringToFront = TRUE))

m_in<-leaflet() %>%
  addProviderTiles(layerId = 'm_tiles_in',
                   providers$CartoDB.Positron,
                   options = providerTileOptions(opacity = .85)) %>%
  fitBounds(-130, -40, 170, 70) %>% 
  addPolygons(data = international_base, 
              color = st_border_color,
              weight = .5,
              smoothFactor = 0.3,
              opacity=0.4,
              fillOpacity = .1,
              label = ~NAME,
              labelOptions = labelOptions(
                style = list("front-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = state_base,
              layerId = 'base_state',
              color = cty_border_color,
              weight = .5,
              smoothFactor = 0.3,
              opacity = .4,
              fillOpacity = 0,
              label = cty_labels,
              labelOptions = labelOptions(
                style = list("front-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              highlightOptions = highlightOptions(
                weight = 2,
                color = "red",
                fillOpacity = 0,
                bringToFront = TRUE))


map_icons <- iconList(
  waterport = makeIcon("www/ship_icon.png", iconWidth = 18, iconHeight = 18),
  airport = makeIcon("www/airport_icon.png", iconWidth = 18, iconHeight = 18),
  border_crossing = makeIcon("www/crossing_icon.png", iconWidth = 18, iconHeight = 18)
)

m_pin<-leaflet() %>%
  addProviderTiles(layerId = 'm_tiles_pin',
                   providers$CartoDB.Positron,
                   options = providerTileOptions(opacity = .85)) %>%
  fitBounds(-130, -40, 170, 70) %>%
  addMarkers(data = ports_base,
             layerId = ~GEOID,
             label = ~NAME,
             labelOptions = labelOptions(
               style = list("front-weight" = "normal", padding = "3px 8px"),
               textsize = "15px",
               direction = "auto"),
             icon = ~map_icons[type])
