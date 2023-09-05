cty_border_color='#B0BEC5'
st_border_color='#78909C'
map_height=500 

commasep=function(x){paste0(paste(x[1:(length(x)-1)],collapse='; '),'; and ',x[length(x)])}


int_ports_mode_no_selelect <- data.frame(mode = c("3","4","99"), use = c("3006","4111","99144"))

modes <- c("Truck"="1",
           "Rail" ="2",
           "Water"="3",
           "Air (Includes truck-air)"="4",
           "Multiple Modes and Mail"="5",
           "Pipeline"="6",
           "Other and Unknown"="7")

modes_int <- c("Water"="3","Air (Includes truck-air)"="4", "Border-Point-of-Entry" = "99")

commodities <- c("Agriculture and Fish",
                 "Energy Products", 
                 "Food, Alcohol and Tobacco",
                 "Machinery, Electric, and Precision Instruments",
                 "Mixed Freight",
                 "Waste and Scrap",
                 "Nonmetallic Mineral and Base Metal Products",
                 "Raw and Finished Wood Products",
                 "Chemicals, Pharmaceuticals, Plastics, and Rubber",
                 "Vehicles and Transportation Equipment",
                 "Textiles and Leather",
                 "Aggregates")

od <- c("Both" = "Both","Inbound" = "dms_dest", "Outbound" = "dms_orig")

ITTS_states <- data.frame(state = c("Arkansas","Florida","Georgia","Kentucky","Louisiana","Mississippi","Missouri","South Carolina","Texas","Virginia", "Alabama","Tennessee","North Carolina"),
                          FIPS = c("05","12","13","21","22","28","29","45","48","51","01","47","37"))


ITTS_states_choices <- data.frame(statename=c("Arkansas","Florida","Georgia","Kentucky",
                                              "Louisiana","Mississippi","Missouri","South Carolina",
                                              "Texas","Virginia", "Alabama","Tennessee","North Carolina"),
                                  STATEFP = c("05","12","13",
                                              "21","22","28",
                                              "29","45","48",
                                              "51",
                                              "01","47","37"))


cty_ch= county_choices$GEOID
state_ch = state_choices$GEOID
port_ch = ports_base$GEOID
names(port_ch) = ports_base$NAME
names(cty_ch)=  county_choices$county_lab
names(state_ch)= state_choices$NAME