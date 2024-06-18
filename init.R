#load("/srv/shiny-server/.RData")
load(file = "ITTS_Initial_Data_06182024.RData")
# load(file = "ITTS_Initial_Data_06132024.RData")
# 
# 
# #this is just for editing r data if needed
# 
# dat_pin <- read.csv('USA_Trade_Data/downloads_06172024/ports2international_feature.csv',
#                     colClasses = c("character","character","character",
#                                    "character","character","numeric",
#                                    "numeric"))
# dat_sin <- read.csv('USA_Trade_Data/downloads_06172024/states2international_feature.csv',
#                     colClasses = c("character","character","character",
#                                    "character","character","numeric",
#                                    "numeric"))
# #remove for sure
# rm(tostate1)
# rm(objects_new)
# rm(objects_old)
# rm(remove)
# rm(remove_list)
# rm(mod_resources_ui)
# rm(mod_welcome_ui)
# rm(process_scenario)
# rm(process_scenario_cc)
# rm(process_scenario_in)
# rm(process_scenario_v3)
# rm(summary_tab)
# rm(ITTS_base_comp)
# rm(county_base)
# 
# #maybe remove
# #rm(ITTS_states)
# #rm(ITTS_states_choices)
# 
# ports_base <- st_read("USA_Trade_Data/downloads_06172024/ports_base.shp")
# 
# # Function to transform all sf objects in the environment
# transform_sf_objects <- function(target_crs, env_objects) {
#   # Get a list of all objects in the environment
# 
# 
#   # Iterate through each object
#   for (obj_name in env_objects) {
#     print(obj_name)
#     # Check if the object is of class 'sf'
#     if (exists(obj_name) && inherits(get(obj_name), "sf") && !(obj_name %in% c("ITTS_hatch","SE_hatch"))) {
#       # Transform the sf object to the target CRS
#       assign(obj_name, st_transform(get(obj_name), target_crs), envir = .GlobalEnv)
#       cat("Transformed", obj_name, "to target CRS.\n")
#     }
#   }
# }
# 
# # Example usage:
# # Define your desired target CRS
# target_crs <- ('+proj=longlat +datum=WGS84')
# env_objects <- ls()
# # Call the function to transform all sf objects in the environment
# transform_sf_objects(target_crs, env_objects)
# #remove b/c just used
# rm(target_crs)
# rm(env_objects)
# rm(obj_name)
# rm(transform_sf_objects)
# 
# port_ch <- set_names(ports_base$GEOID, ports_base$NAME)
# all_counties_centr<-all_counties_centr |>
#   select(-c(AFFGEOID, LSAD, ALAND, AWATER))
# 
# all_selected<-all_selected|>
#   filter(!(type %in% c('waterport','airport','border_crossing'))) |>
#   rbind(ports_base |>
#           mutate(mode_nm = case_when(type=='waterport'~3,
#                                      type=='airport'~4,
#                                      type=='border_crossing'~99)))
# 
# county_choices <- left_join(county_choices, county_selected|>st_drop_geometry())
# rm(county_selected)
# save.image(file = 'ITTS_Initial_Data_06182024.RData')
