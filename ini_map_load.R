state_base <- st_read("data/cb_2018_us_county_500k/sum_to_state_lvl.shp") %>%
  mutate(GEOID = STATEFP)  %>%
  inner_join(y=tigris::fips_codes %>%
              mutate(GEOID = state_code)%>%
              select(GEOID, state_name) %>%
              group_by(GEOID, state_name) %>%
               summarise() %>%
               ungroup() , by = "GEOID") %>%
  rename(state_lab = state_name)  %>%
  mutate(NAME = state_lab) 

international_base <- st_read("data/cb_2018_us_county_500k/international_regions_base.shp") %>% st_transform(crs = st_crs(state_base)) %>% mutate(GEOID = as.character(GEOID))

ports_base <- st_read("data/cb_2018_us_county_500k/ports_base.shp") %>% 
  mutate(type=case_when(str_detect(NAME,'Air')~'airport',
                        str_detect(NAME,'Water')~'waterport',
                        str_detect(NAME, 'Border')~'border_crossing',
                        TRUE~NA_character_)) %>% 
  mutate(mode_nm = case_when(str_detect(NAME,'Air')~'4',
                             str_detect(NAME,'Water')~'3',
                             str_detect(NAME, 'Border')~'99',
                             TRUE~NA_character_))%>%
  st_transform(crs = st_crs(state_base)) %>% 
  filter(!is.na(GEOID)) 

county_base <- st_read("data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")  %>%
  filter(STATEFP %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))

county_selected <- county_base %>%
  left_join(y=tigris::fips_codes %>% 
              mutate(GEOID=paste0(state_code,county_code)) %>% 
              select(GEOID,state_abb=state)) %>%
  mutate(county_lab=paste0(NAME,', ',state_abb)) %>% 
  select(GEOID, NAME, county_lab, STATEFP) 

ITTS_states_choices <- data.frame(statename=c("Arkansas","Florida","Georgia","Kentucky",
                                              "Louisiana","Mississippi","Missouri","South Carolina",
                                              "Texas","Virginia",
                                              "Alabama","Tennessee","North Carolina"),
                                  STATEFP = c("05","12","13",
                                              "21","22","28",
                                              "29","45","48",
                                              "51",
                                              "01","47","37"))

county_choices <- county_selected %>%
  left_join(ITTS_states_choices) %>% 
  select(-STATEFP) %>%
  arrange(statename)

state_choices <- state_base %>%
  filter(GEOID %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))

# all_selected <- rbind(select(mutate(county_choices, NAME = county_lab),-c(county_lab,statename)), select(state_choices, -c(state_lab, STATEFP))) %>%
#   bind_rows(ports_base)

all_counties_centr <- county_base %>% 
  st_centroid() %>%
  mutate(long = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()
all_states_centr<- state_choices %>%
  st_centroid() %>%
  mutate(long = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()


#county_selected$has_data = if_else(county_selected$GEOID %in% (dat$dms_orig %>% unique),'#66CD00',"#666666")
#county_selected$has_data = if_else(county_selected$GEOID %in% (dat$dms_orig %>% unique),cty_border_color,"#666666")

 
#cty_labels <- str_to_title(county_selected$county_lab) %>% lapply(htmltools::HTML)
cty_labels <- county_selected$county_lab %>% lapply(htmltools::HTML)
state_base_labels <- state_base$state_lab %>% lapply(htmltools::HTML)
state_choice_labels <- state_choices$NAME %>% lapply(htmltools::HTML)

# Create base map ----
m<-leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron,#providers$CartoDB.DarkMatter,
                   options = providerTileOptions(opacity = .85)) %>%
  addFullscreenControl() %>% 
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
              label = state_base_labels,
              labelOptions = labelOptions(
                style = list("front-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                 direction = "auto")) #%>%
  # addPolygons(data = county_selected,
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


