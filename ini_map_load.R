# Create base map ----
m<-leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron,#providers$CartoDB.DarkMatter,
                   options = providerTileOptions(opacity = .85)) %>%
  addFullscreenControl() %>% 
  addPolygons(data = county_choices,
              layerId = ~(GEOID),
              color = cty_border_color,
              weight = .5,
              smoothFactor = 0.3,
              opacity = 0.4,
              fillOpacity = 0.1,
              label = ~county_lab,
              labelOptions = labelOptions(
                style = list("front-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              highlightOptions = highlightOptions(
                weight = 2,
                color = "red",
                fillOpacity = 0.7,
                bringToFront = TRUE))
#create county to sate base map----
m_cs<-leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron,#providers$CartoDB.DarkMatter,
                   options = providerTileOptions(opacity = .85)) %>%
  addFullscreenControl() %>% 
  setView(-99.0909, 39.8355, zoom = 4)%>%
  addPolygons(data = state_base, 
              color = st_border_color,
              weight = .5,
              smoothFactor = 0.3,
              opacity=0.4,
              fillOpacity = .1,
              label = ~state_lab,
              labelOptions = labelOptions(
                style = list("front-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                 direction = "auto")) #%>%
  # addPolygons(data = county_choices,
  #             layerId = 'base_county',
  #             color = cty_border_color,
  #             weight = .5,
  #             smoothFactor = 0.3,
  #             opacity = .4,
  #             fillOpacity = 0,
  #             label = cty_labels,
  #             labelOptions = labelOptions(
  #               style = list("front-weight" = "normal", padding = "3px 8px"),
  #               textsize = "15px",
  #               direction = "auto"),
  #             highlightOptions = highlightOptions(
  #               weight = 2,
  #               color = "red",
  #               fillOpacity = 0,
  #               bringToFront = TRUE))
#international_map
m_in<-leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron,#providers$CartoDB.DarkMatter,
                   options = providerTileOptions(opacity = .85)) %>%
  addFullscreenControl() %>% 
  #setView(-99.0909, 39.8355, zoom = 4)%>%
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
  waterport = makeIcon("WWW/ship_icon.png", iconWidth = 18, iconHeight = 18),
  airport = makeIcon("WWW/airport_icon.png", iconWidth = 18, iconHeight = 18),
  border_crossing = makeIcon("WWW/crossing_icon.png", iconWidth = 18, iconHeight = 18)
)

m_pin<-leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron,#providers$CartoDB.DarkMatter,
                   options = providerTileOptions(opacity = .85)) %>%
  addFullscreenControl() %>%
  #setView(-99.0909, 39.8355, zoom = 4)%>%
  fitBounds(-130, -40, 170, 70) %>%
  addMarkers(data = ports_base, 
             #radius = 18000,
             layerId = ~GEOID,
             #color = "purple",
             #weight = .5,
             #opacity = ~transp,
             #fillOpacity = .3,
             label = ~NAME,
             labelOptions = labelOptions(
               style = list("front-weight" = "normal", padding = "3px 8px"),
               textsize = "15px",
               direction = "auto"),
             icon = ~map_icons[type])
