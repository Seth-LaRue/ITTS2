# Load Libraries ----
library(tidyverse)
library(sf)
library(data.table)
library(readxl)
library(dplyr)
library(stringi)
library(fuzzyjoin)
library(leaflet)
# Process ITTS Features Data ----
## Load data -----
# base_path = "C:/Users/qsi/OneDrive - Cambridge Systematics/Commodity Flow/"
# base_path = "C:/Users/qsi/Cambridge Systematics/PROJ 190103.002 ITTS LATTS - Commodity Flow/"
# base_path = "C:/Users/slarue/Cambridge Systematics/PROJ 190103.002 ITTS LATTS - Documents/003 ITTS SETTS/Task 5 Scenario Tool/Removed ITTS Files/"
# base_path = "C:/Users/slarue/Cambridge Systematics/PROJ 190103.002 ITTS LATTS - Documents/3. Regional Profile/3.4 Commodity Flow and Performance/Commodity Flow/"
### load the aggregated commodity groups 
commodity_group <- read_excel("SETTS_sandbox/GroupedCommodities.xlsx")

state_code <- read_excel("SETTS_sandbox/ZoneLookup.xlsx")

# ### load trade type dictionary
# trade_code<- c(1,2,3)
# trade_name <- c("Domestic Only","Import","Export")
# trade_type <- data.frame(trade_code,trade_name) %>%
#   mutate(trade_name = paste(trade_code,trade_name,sep = " - "))
# 
# ### load mode dictionary
# mode_code <- c(1,2,3,4,5,6,7,8)
# mode_name <- c("Truck", "Rail","Water","Air(Includes truck-air)","Multiple Modes and Mail",
#                "Pipeline","Other and Unknown","No Domestic Mode")
# Mode <- data.frame(mode_code,mode_name) %>%
#   mutate(mode_name = paste(mode_code,mode_name,sep = " - "))

## load and process mother faf data ----



moth <- fread(paste0(base_path, 'disaggregatedITTS-20172050-withHeaders',sep =""),
                       colClasses = c(dms_orig = "character",dms_dest ="character",
                                      fr_dest = "character", fr_orig = "character")
              ) %>%
  left_join(.,commodity_group, by = c("sctg2" = "STCGCode")) %>%
  rename(Grouped_sctg2 = Grouped) %>%
  mutate(Foreign = ifelse(!is.na(fr_orig)|!is.na(fr_dest),1,0)) %>%
  select(-c("tons_2022","tons_2023","tons_2025","tons_2030","tons_2035","tons_2040","tons_2045",
            "value_2022","value_2023","value_2025","value_2030","value_2035","value_2040","value_2045",
            "STCGDescription")) 
gc()
rm(commodity_group)

#check moth ----
moth$dms_orig %>% stringr::str_sub(1,2) %>% unique() %>% length()
moth$dms_dest %>% stringr::str_sub(1,2) %>% unique() %>% length()
moth$dms_dest[str_sub(moth$dms_dest,1,2) == "23"] %>% nchar() %>% unique()
moth$dms_orig[str_sub(moth$dms_orig,1,2) == "23"] %>% nchar() %>% unique()

## Process Port Data ----
airport_zonename_lookup <- state_code %>%
  filter(len == 3) %>%
  rename("ZoneName_Airport" = "ZoneName") %>%
  select(c("ZoneNum","ZoneName_Airport"))

FAF_ports <- state_code %>%
  filter(nchar(ZoneNum) == 4) %>%
  mutate(Type = case_when(substr(ZoneNum,1,1) == 1 ~ "Border Crossing",
                          substr(ZoneNum,1,1) == 2 ~ "Water Port",
                          substr(ZoneNum,1,1) == 4 ~ "Airport",
                          substr(ZoneNum,1,1) == 5 ~ "Multiple Modes",
                          substr(ZoneNum,1,1) == 6 ~ "Pipeline",
                          substr(ZoneNum,1,1) == 7 ~ "Unknown"))
  # create a list of ports used in the FAF disaggregated data:
PortsInData_import<- moth %>% filter(nchar(dms_orig)==4)%>%
  distinct(dms_orig) %>%
  as.list()
PortsInData_export<- moth %>% filter(nchar(dms_dest)==4)%>%
  distinct(dms_dest)%>%
  as.list()
PortsInData<- setdiff(PortsInData_import,PortsInData_export)

  # filter FAF_ports used in the data. split into water port and border crossing
FAF_waterport_used <- FAF_ports %>% subset(ZoneNum %in% unlist(PortsInData)) %>%
  filter(Type == "Water Port")

FAF_borderCrossing_used <- FAF_ports %>% subset(ZoneNum %in% unlist(PortsInData)) %>%
  mutate(keyword = substr(ZoneName,1,regexpr(",",ZoneName)-1)) %>%
  filter(Type == "Border Crossing")   # based on the output, our dataset only has border crossing and water port.

  # loading border crossing and water port data shared by Lizze, adjust columns: 
BorderCrossing_L <- st_read("./Data/Border_Crossing.shp") %>%
  select(c("portname","state","cp_name","geometry")) %>%
  rename("Fac_Name" = "portname",
         "City" = "cp_name") %>%
  add_column(ITTS = NA,
             ITTS_NET = NA,
             County = NA) %>%
  mutate(Type = "Border Crossing")

Riverport_L <- st_read("./Data/River_Port.shp") %>%
  select(c("port_name","ITTS_Net","ITTS","geometry")) %>%
  rename("Fac_Name" = "port_name",
         "ITTS_NET" = "ITTS_Net") %>%
  add_column(state = NA,
             County = NA,
             City = NA) %>%
  mutate(Type = "Water Port")

Waterport_L <- st_read("./Data/Water_Port.shp") %>%
  select(c("port_name","ITTS_Net","ITTS","geometry")) %>%
  rename("Fac_Name" = "port_name",
         "ITTS_NET" = "ITTS_Net") %>%
  add_column(state = NA,
             County = NA,
             City = NA) %>%
  mutate(Type = "Water Port")

all_waterport_L <- rbind(Riverport_L,Waterport_L) %>%
  mutate(keyword = substr(Fac_Name,1,regexpr(",",Fac_Name)-1))

  # apply fuzzy join on water ports and border crossings. 
bordercrossing_GIS <- regex_left_join(FAF_borderCrossing_used,BorderCrossing_L, by = c("keyword" = "Fac_Name"),
                                      ignore_case = T) %>%
  select(c(-"len",-"Comments",-"Type.y",-"Fac_Name",-"County",-"City",-"keyword"))
waterport_GIS <- regex_left_join(FAF_waterport_used,all_waterport_L, by = c("ZoneName" = "keyword"),
                                 ignore_case = T) %>%
  select(c(-"len",-"Type.y",-"County",-"City",-"Fac_Name",-"Comments",-"keyword"))


AllPorts_L_GIS <- rbind(bordercrossing_GIS,waterport_GIS) %>%
  rename(Type = Type.x)  # missing geometry for five water ports.

  # check if the missing ports exist in another gis data. 
FAF_ports_gis <- st_read("./Data/Freight_Analysis_Framework_(FAF5)_Network_Nodes/Freight_Analysis_Framework_(FAF5)_Network_Nodes.shp")

FAF_ports_gis_selected <- FAF_ports_gis %>% 
  filter(Facility_T %in% c("Border Crossing","Airport-Terminal Location","Seaport",
                           "Airport-Cargo Location","Intermodal")) %>%
  select(-c("Centroid","CentroidID","Entry_or_E","Exit_Numbe","StateID","StateName","StateNameB")) %>%
  distinct(.,Facility_T,Facility_N,County, .keep_all = TRUE) #keep one point for each facility

 # check if missing ports exist in this dataset: 
Pensacola <- FAF_ports_gis_selected %>% filter(.,grepl("Pensacola",Facility_N,ignore.case = TRUE) & Facility_T == "Seaport")
Hopewell <- FAF_ports_gis_selected %>% filter(.,grepl("Hopewell",Facility_N,ignore.case = TRUE) & Facility_T == "Seaport")
Newport <- FAF_ports_gis_selected %>% filter(.,grepl("Newport",Facility_N,ignore.case = TRUE) & Facility_T == "Seaport")

missing_port <- rbind(Pensacola,Hopewell,Newport) %>%
  select(c("Facility_N","geometry")) %>%
  regex_right_join(AllPorts_L_GIS,., by = c("ZoneName" = "Facility_N"),
                   ignore_case = T) %>%
  select(c(-"geometry.x",-"Facility_N")) %>%
  rename(geometry = geometry.y)

AllPorts_L_GIS_new <- AllPorts_L_GIS %>%
  filter(!ZoneName %in% missing_port$ZoneName) %>%
  rbind(.,missing_port)

# st_write(AllPorts_L_GIS_new, "AllPorts_raw.shp") # two ports are still missing geometry, add geometry info in Arcgis Pro.

# load the edited shapefile. 
All_ports <- st_read("./Data/AllPorts.shp")
  


## Process ITTS Features cnty 2 cnty trading ----
#i had to split this apart for it to be able to calculate
cnty2cnty_feature_tons <- moth %>%
  filter(nchar(dms_orig)==5 & nchar(dms_dest)==5) %>% #this is essential to filter out states/ports?
  #group by new commodity
  select(-fr_orig,-fr_dest,-fr_outmode,-fr_inmode) %>%
  group_by(dms_orig,dms_dest,Grouped_sctg2,dms_mode) %>% #,trade_type) %>%
  # add description for Mode
  summarise(tons_2017 = sum(tons_2017),
            tons_2020 = sum(tons_2020),
            tons_2050 = sum(tons_2050)) %>% 
  ungroup()

cnty2cnty_feature_value <- moth %>%
  filter(nchar(dms_orig)==5&nchar(dms_dest)==5) %>% #this is essential to fitler out state/ports?
  select(-fr_orig,-fr_dest,-fr_outmode,-fr_inmode) %>%
  group_by(dms_orig,dms_dest,Grouped_sctg2,dms_mode) %>%#,trade_type) %>%
  # add description for Mode
  summarise(value_2017 = sum(value_2017),
            value_2020 = sum(value_2020),
            value_2050 = sum(value_2050)) %>%
  ungroup()# %>%
  # left_join(.,Mode, by = c("dms_mode"= "mode_code")) %>%
  # # add description for trade type
  # left_join(.,trade_type, by = c("trade_type" = "trade_code"))
cnty2cnty_feature <- left_join(cnty2cnty_feature_tons, cnty2cnty_feature_value) %>%
  #rename(origin = dms_orig) %>%
  #rename(destination = dms_dest) %>%
  mutate(lineid = paste0(pmin(origin, destination),pmax(origin, destination)))
gc()

#write.csv(cnty2cnty_feature, "cnty2cnty_feature.csv")
## Process ITTS Features cnty 2 state trading ----

### load state codes: 

#### destination state
cnty2State_ToState <- moth %>%
  filter(str_sub(dms_orig,1,2) == "23"|str_sub(dms_dest,1,2)=="23") %>% 
  left_join(.,state_code, by = c("dms_dest" = "ZoneNum")) %>% #should we even be using this?
  rename(dms_dest_state = StateCode) %>% 
  select(-c("ZoneName","StateAbbrev","fr_orig","fr_dest",
            "fr_inmode","fr_outmode")) %>%
  #filter(nchar(dms_dest_state)==2) %>%  # what does this filter here mean? all the states are two digits.
  filter(nchar(dms_orig)==5) %>%
  filter(str_sub(dms_orig, 1,2) %in% c("05","12","13","21","22","28","29","45","48","51")) %>%#these are an essential step to filter only itts counties and states 
  group_by(dms_dest_state,dms_orig,Grouped_sctg2,dms_mode,trade_type) %>%
  summarise(tons_2017 = sum(tons_2017),
            tons_2020 = sum(tons_2020),
            tons_2050 = sum(tons_2050),
            value_2017 = sum(value_2017),
            value_2020 = sum(value_2020),
            value_2050 = sum(value_2050)) %>% 
  ungroup() %>%
  rename(origin = dms_orig,
         destination = dms_dest_state)
gc()
#### origin state
cnty2State_FromState <- moth %>%
  left_join(.,state_code, by = c("dms_orig" = "ZoneNum")) %>%
  rename(dms_orig_state = StateCode) %>%
  select(-c("ZoneName","StateAbbrev","fr_orig","fr_dest",
            "fr_inmode","fr_outmode")) %>%
  #filter(nchar(dms_orig_state)==2) %>%  # what does this filter here mean? all the states are two digits. 
  filter(nchar(dms_dest)==5) %>%
  filter(str_sub(dms_dest, 1,2) %in% c("05","12","13","21","22","28","29","45","48","51")) %>%
  group_by(dms_orig_state,dms_dest,Grouped_sctg2,dms_mode,trade_type) %>%
  summarise(tons_2017 = sum(tons_2017),
            tons_2020 = sum(tons_2020),
            tons_2050 = sum(tons_2050),
            value_2017 = sum(value_2017),
            value_2020 = sum(value_2020),
            value_2050 = sum(value_2050)) %>% 
  ungroup() %>%
  rename(origin = dms_orig_state,
         destination = dms_dest)

gc()
  # cnty2state_feature <- rbind(cnty2State_ToState,cnty2State_FromState) %>%
  #   mutate(lineid = paste0(pmin(origin, destination), pmax(origin, destination)))
  #left_join(.,Mode, by = c("dms_mode"= "mode_code")) %>%
  #left_join(.,trade_type, by = c("trade_type" = "trade_code"))

##Process State to State datasetf
state2state_feature <- moth %>%
  left_join(state_code, by = c("dms_orig" = "ZoneNum")) %>%
  rename(dms_orig_state = StateCode) %>%
  select(-c("ZoneName","StateAbbrev","fr_orig","fr_dest",
            "fr_inmode","fr_outmode"))  %>%  
  left_join(state_code, by = c("dms_dest" = "ZoneNum")) %>%
  rename(dms_dest_state = StateCode)%>% 
  select(-c("ZoneName","StateAbbrev")) %>%
  filter(nchar(dms_orig_state)==2) %>%
  filter(nchar(dms_dest_state)==2) %>%
  group_by(dms_orig_state,dms_dest_state,Grouped_sctg2,dms_mode,trade_type) %>%
  summarise(tons_2017 = sum(tons_2017),
            tons_2020 = sum(tons_2020),
            tons_2050 = sum(tons_2050),
            value_2017 = sum(value_2017),
            value_2020 = sum(value_2020),
            value_2050 = sum(value_2050)) %>% 
  ungroup() %>%
  rename(origin = dms_orig_state,
         destination = dms_dest_state)
write.csv(state2state_feature, "state2state_feature.csv", row.names = F)

## Process ITTS Features International trading ----
### Option 1 ----
### feature for international flow, ignore the domestic segment of the flow.  
ports2Internatioal_Import <- moth %>%
  # only keep rows involved import international trading 
  filter(Foreign == 1 & trade_type == 2) %>%  # also filtered out data with 3-digits zonenumber. 
  left_join(.,state_code, by = c("dms_orig" = "ZoneNum")) %>%
  select(-c("ZoneName","StateAbbrev","Comments","len")) %>%
  filter(StateCode %in% c("05","12","13","21","22","28","29","45","48","51")) %>%
  rename(origin = fr_orig,
         destination = dms_orig,
         fr_mode = fr_inmode) %>%
  group_by(origin,destination,fr_mode,Grouped_sctg2,trade_type) %>%
  summarise(tons_2017 = sum(tons_2017),
            tons_2020 = sum(tons_2020),
            tons_2050 = sum(tons_2050),
            value_2017 = sum(value_2017),
            value_2020 = sum(value_2020),
            value_2050 = sum(value_2050)) %>% 
  ungroup()
  # export international trading
ports2Internatioal_Export <- moth %>%
  # only keep rows involved international trading
  filter(Foreign == 1 & trade_type == 3) %>%  # also filtered out data with 3-digits zonenumber. 
  left_join(.,state_code, by = c("dms_dest" = "ZoneNum")) %>%
  select(-c("ZoneName","StateAbbrev","Comments","len")) %>%
  filter(StateCode %in% c("05","12","13","21","22","28","29","45","48","51")) %>%
  rename(origin = dms_dest,
         destination = fr_dest,
         fr_mode = fr_outmode) %>%
  group_by(origin,destination,fr_mode,Grouped_sctg2,trade_type) %>%
  summarise(tons_2017 = sum(tons_2017),
            tons_2020 = sum(tons_2020),
            tons_2050 = sum(tons_2050),
            value_2017 = sum(value_2017),
            value_2020 = sum(value_2020),
            value_2050 = sum(value_2050)) %>% 
  ungroup()

ports2International_feature<- rbind(ports2Internatioal_Import,ports2Internatioal_Export) %>%
  mutate(lineid = paste0(pmin(origin, destination), pmax(origin, destination)))

### Option 2 ---- 
### if ignore the intermediate location, directly link the foreign country to the final destination. 
county2Internatioal_Import <- moth %>%
  filter(Foreign == 1 & trade_type == 2) %>%
  left_join(.,state_code, by = c("dms_dest" = "ZoneNum")) %>%
  select(-c("ZoneName","StateAbbrev","Comments","len")) %>%
  filter(StateCode %in% c("05","12","13","21","22","28","29","45","48","51")) %>%
  rename(origin = fr_orig,
         destination = dms_dest,
         fr_mode = fr_inmode) %>%
  group_by(origin,destination,fr_mode,Grouped_sctg2,trade_type) %>%
  summarise(tons_2017 = sum(tons_2017),
            tons_2020 = sum(tons_2020),
            tons_2050 = sum(tons_2050),
            value_2017 = sum(value_2017),
            value_2020 = sum(value_2020),
            value_2050 = sum(value_2050)) %>% 
  ungroup()

# export international trading
county2Internatioal_Export <- moth %>%
  # only keep rows involved international trading
  filter(Foreign == 1 & trade_type == 3) %>%  # also filtered out data with 3-digits zonenumber. 
  left_join(.,state_code, by = c("dms_orig" = "ZoneNum")) %>%
  select(-c("ZoneName","StateAbbrev","Comments","len")) %>%
  filter(StateCode %in% c("05","12","13","21","22","28","29","45","48","51")) %>%
  rename(origin = dms_orig,
         destination = fr_dest,
         fr_mode = fr_outmode) %>%
  group_by(origin,destination,fr_mode,Grouped_sctg2,trade_type) %>%
  summarise(tons_2017 = sum(tons_2017),
            tons_2020 = sum(tons_2020),
            tons_2050 = sum(tons_2050),
            value_2017 = sum(value_2017),
            value_2020 = sum(value_2020),
            value_2050 = sum(value_2050)) %>% 
  ungroup()

county2International_feature<- rbind(county2Internatioal_Import,county2Internatioal_Export) %>%
  mutate(lineid = paste0(pmin(origin, destination), pmax(origin, destination)))

### Option 3 ---- 
### Include both dms_orig and dms_dest
county2Internatioal_Import_v2 <- moth %>%
  filter(Foreign == 1 & trade_type == 2) %>%
  left_join(.,state_code, by = c("dms_dest" = "ZoneNum")) %>%
  select(-c("ZoneName","StateAbbrev","Comments","len")) %>%
  filter(StateCode %in% c("05","12","13","21","22","28","29","45","48","51")) %>%
  rename(origin = fr_orig,
         Intermediate = dms_orig,
         destination = dms_dest,
         fr_mode = fr_inmode) %>%
  group_by(origin,Intermediate,destination,fr_mode,Grouped_sctg2,trade_type) %>%
  summarise(tons_2017 = sum(tons_2017),
            tons_2020 = sum(tons_2020),
            tons_2050 = sum(tons_2050),
            value_2017 = sum(value_2017),
            value_2020 = sum(value_2020),
            value_2050 = sum(value_2050)) %>% 
  ungroup()

county2Internatioal_Export_v2 <- moth %>%
  # only keep rows involved international trading
  filter(Foreign == 1 & trade_type == 3) %>%  # also filtered out data with 3-digits zonenumber. 
  left_join(.,state_code, by = c("dms_orig" = "ZoneNum")) %>%
  select(-c("ZoneName","StateAbbrev","Comments","len")) %>%
  filter(StateCode %in% c("05","12","13","21","22","28","29","45","48","51")) %>%
  rename(origin = dms_orig,
         Intermediate = dms_dest,
         destination = fr_dest,
         fr_mode = fr_outmode) %>%
  group_by(origin,Intermediate,destination,fr_mode,Grouped_sctg2,trade_type) %>%
  summarise(tons_2017 = sum(tons_2017),
            tons_2020 = sum(tons_2020),
            tons_2050 = sum(tons_2050),
            value_2017 = sum(value_2017),
            value_2020 = sum(value_2020),
            value_2050 = sum(value_2050)) %>% 
  ungroup()

county2International_feature_v2 <- rbind(county2Internatioal_Import_v2,county2Internatioal_Export_v2) %>%
  mutate(lineid = paste0(pmin(origin, destination), pmax(origin, destination)))





# Process ITTS Lines ----

## Load data -----
all_counties <- st_read("SETTS_sandbox/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")%>% 
  filter(STATEFP %in% c("05","12","13","21","22","28","29","45","48","51")) %>%
  st_centroid() %>%
  mutate(long = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

## Process ITTS Lines cty 2 cty trading ----
cnty2cnty_lines <- cnty2cnty_feature %>%
  filter(!duplicated(paste0(pmin(origin, destination), pmax(origin, destination)))) %>%
  filter(!(origin == destination)) %>%
  filter(origin != "51515", destination != "51515") %>% #independent city of bedford doesn't exist in our county shapefile it's not ideal to drop at ~200 rows
  pivot_longer(cols = c(origin,destination), values_to = c("point"), names_to=NULL) %>% 
  left_join(select(all_counties,"GEOID","long","lat"), by = c("point"="GEOID")) %>%
  st_as_sf(coords = c("long","lat")) %>%
  group_by(lineid) %>%
  summarise() %>% st_make_valid() %>% 
  st_cast("LINESTRING")
gc()
#write.csv(cnty2cnty_feature, "cnty2cnty_feature.csv")
#st_write(cnty2cnty_lines, "cnty2cnty_lines.shp")
#gc()
## Process ITTS Lines cty 2 state trading ----
all_states <- st_read("SETTS_sandbox/cb_2018_us_county_500k/sum_to_state_lvl.shp")  %>%
  st_centroid() %>%
  rename(GEOID = STATEFP)%>%
  mutate(long = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
    select(GEOID, long, lat)
all_c_join <- all_counties %>% 
  select(GEOID, long, lat)
all_cents <- rbind(all_states, all_c_join)
## Process ITTS Lines International trading ----
cnty2state_lines <- cnty2state_feature %>%
  filter(!duplicated(paste0(pmin(origin, destination), pmax(origin, destination)))) %>%
  filter(origin != "51515", destination != "51515") %>% #independent city of bedford doesn't exist in our county shapefile it's not ideal to drop at ~200 rows
  pivot_longer(cols = c(origin,destination), values_to = c("point"), names_to=NULL) %>% 
  left_join(select(all_cents,"GEOID","long","lat"), by = c("point"="GEOID")) %>%
  st_as_sf(coords = c("long","lat")) %>%
  group_by(lineid) %>%
  summarise() %>% st_make_valid() %>% 
  st_cast("LINESTRING")
gc()
#write.csv(cnty2state_feature, "cnty2state_feature.csv")
#st_write(cnty2state_lines, "cnty2state_lines.shp")
#gc()



# Create Base Maps -----

## Load Data -----
county_base <- st_read("SETTS_sandbox/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")
state_base <- st_read("SETTS_sandbox/cb_2018_us_county_500k/sum_to_state_lvl.shp")

county_selected <- county_base[county_base$STATEFP %in% c("05", "12","13","21","22","28","29","45","48","51"), ]
ITTS_states_choices <- data.frame(statename=c("Arkansas","Florida","Georgia","Kentucky",
                                              "Louisiana","Mississippi","Missouri","South Carolina",
                                              "Texas","Virginia"),
                                  STATEFP = c("05","12","13",
                                              "21","22","28",
                                              "29","45","48",
                                              "51"))

county_choices <- county_selected %>%
  select(STATEFP, NAME, GEOID) %>%
  left_join(ITTS_states_choices) %>% 
  select(-c(STATEFP)) %>%
  arrange(statename)


county_selected$has_data = if_else(county_selected$GEOID %in% (dat$origin %>% unique),'#66CD00',"#666666")
county_selected$has_data = if_else(county_selected$GEOID %in% (dat$origin %>% unique),"#cccccc","#666666")

# 
labels <- str_to_title(county_selected$NAME) %>% lapply(htmltools::HTML)

## Create County Base Map -----
m_cc<-leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron,#providers$CartoDB.DarkMatter,
                   options = providerTileOptions(opacity = .85)) %>%
  
  addPolygons(data = county_selected,
              layerId = ~GEOID,
              color = ~has_data,
              weight = .5,
              smoothFactor = 0.3,
              opacity = 0.4,
              fillOpacity = 0.1,
              label = labels,
              labelOptions = labelOptions(
                style = list("front-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              highlightOptions = highlightOptions(
                weight = 2,
                color = "red",
                fillOpacity = 0.7,
                bringToFront = TRUE))




## Create State Base Map -----
m_cs<-leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron,#providers$CartoDB.DarkMatter,
                   options = providerTileOptions(opacity = .85)) %>%
  addPolygons(data = state_base, 
              layerId = ~STATEFP, #maybe we should discuss whether to use two code or three code geoid for state fips
              color = '#cccccc',
              weight = .3,
              smoothFactor = 0.3,
              opacity = 0.4, 
              fill_opacity = 0.2) %>%
  addPolygons(data = county_selected,
              layerId = ~GEOID,
              color = ~has_data,
              weight = .5,
              smoothFactor = 0.3,
              opacity = 0.4,
              fillOpacity = 0.1,
              label = labels,
              labelOptions = labelOptions(
                style = list("front-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              highlightOptions = highlightOptions(
                weight = 2,
                color = "red",
                fillOpacity = 0.7,
                bringToFront = TRUE))


## Create International Base Map -----
region_map <- st_read("./Data/FAF_Region.shp")
region_centroid <- st_read("./Data/FAF_Region_Centroid_adjusted.shp") %>%
  extract(geometry, into = c('Lon','Lat'),'\\((.*),(.*)\\)', conv = T)

pal <- colorFactor(c("navy", "red"), domain = c("Border Crossing", "Water Port"))

m_ci <- leaflet(All_ports) %>%
  addPolygons(data = region_map, 
              layerId = ~FAF_Region, #maybe we should discuss whether to use two code or three code geoid for state fips
              color = "red",
              weight = .5,
              smoothFactor = 0.3,
              opacity = 0.4,
              fillOpacity = 0.1) %>%
  addCircleMarkers(lng = ~POINT_X,
                   lat = ~POINT_Y,
                   label = ~ZoneNam,
                   #weight = 1,
                   radius = 0.5,
                   color = ~pal(Type))




# Export data for Tool----

save(cnty2cnty_feature,cnty2state_feature, county2Internatioal_feature, 
     cnty2cnty_lines, cnty2state_lines, inttr_lines_df,
     m_cc, m_cs, int_base_map, file='ITTS_tool_data.RData')
save(dat,dat_cs,dat_ss, file = "C:/Users/slarue/Cambridge Systematics/PROJ 190103.002 ITTS LATTS - Commodity Flow/Data Viz Tool/ITTS_tool_data_files_10312022.RData")


## data data to rdata
dat_rs <- dat_ss %>%
  mutate(origin = ifelse(origin %in% c("05", "12","13","21","22","28","29","45","48","51"),'ITTS',origin),
         destination = ifelse(destination %in% c("05", "12","13","21","22","28","29","45","48","51"), 'ITTS',destination))

dat_se <- dat_ss %>%
  mutate(origin = ifelse(origin %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"),'Southeast Region',origin),
         destination = ifelse(destination %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"),'Southeast Region',destination))

# make column names consistent
colnames(dat)[colnames(dat) %in% c("dms_orig", 'dms_dest')] = c("origin",'destination')
colnames(dat_pin)[colnames(dat_pin) %in% c('Tons_2019','Tons_2021','Value_2019','Value_2021')] = c('tons_2019','tons_2021','value_2019','value_2021')
colnames(dat_sin)[colnames(dat_sin) %in% c('Tons_2019','Tons_2021','Value_2019','Value_2021')] = c('tons_2019','tons_2021','value_2019','value_2021')


# ITTS_base <- state_base %>%
#   mutate(NAME = ifelse(GEOID %in% c("05", "12","13","21","22","28","29","45","48","51"),'ITTS',NAME),
#          GEOID = ifelse(GEOID %in% c("05", "12","13","21","22","28","29","45","48","51"),'ITTS',GEOID)) %>%
#   group_by(NAME, GEOID) %>%
#   summarise(NAME = unique(NAME),
#             GEOID = unique(GEOID))%>%
#   ungroup()

ITTS_boundary <- ITTS_base %>% filter(GEOID == 'ITTS') %>% 
  select('GEOID','NAME') %>%
  mutate(type = '',
         mode_nm = '')

all_selected = rbind(all_selected,ITTS_boundary) 

SE_base <- state_base %>%
  mutate(NAME = ifelse(GEOID %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"),'Southeast Region',NAME),
         GEOID = ifelse(GEOID %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"),'Southeast Region',GEOID)) %>%
  group_by(NAME, GEOID) %>%
  summarise(NAME = unique(NAME),
            GEOID = unique(GEOID))%>%
  ungroup()

SE_boundary <- SE_base %>% filter(GEOID == 'Southeast Region') %>% 
  select('GEOID','NAME') %>%
  mutate(type = '',
         mode_nm = '')

all_selected = rbind(all_selected,SE_boundary) 

other_states = state_base %>% 
  filter(!(GEOID %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))) %>%
  select(-'state_lab',-'STATEFP') %>%
  mutate(type = "",
         mode_nm = "")
all_selected <- rbind(all_selected,other_states)  ## add all geographical boundaries to this layer, used in graphs. 

international_base <- international_base %>%
  mutate(type = "",
         mode_nm = "")
all_selected <- rbind(all_selected,international_base)


# this is for hatch pattern on ITTS and SE_hatch
ITTS_hatch <- HatchedPolygons::hatched.SpatialPolygons(ITTS_boundary, density = 1, angle = c(45, 135))
SE_hatch <- HatchedPolygons::hatched.SpatialPolygons(SE_boundary, density  = 1, angle = c(45, 135))


ITTS_base <- sf::st_transform(ITTS_base,CRS("+init=epsg:4269"))
SE_base <- sf::st_transform(SE_base,CRS("+init=epsg:4269"))
all_selected <- sf::st_transform(all_selected,CRS("+init=epsg:4269"))