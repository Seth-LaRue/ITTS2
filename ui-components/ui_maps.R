#ports <- c("Port 1","Port 2","Port 3", "1_3_5_7_9_11_13_15_17_19_21_23_25")
#states <- c("Arkansas","ect.")
#partners <- c("partner1","partner2")

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

#county_base <- readOGR("Date/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")

# county_selected <- county_base[county_base$STATEFP %in% c("05", "12","13","21","22","28","29","45","48","51"), ]
ITTS_states_choices <- data.frame(statename=c("Arkansas","Florida","Georgia","Kentucky",
                                              "Louisiana","Mississippi","Missouri","South Carolina",
                                              "Texas","Virginia", "Alabama","Tennessee","North Carolina"),
                                  STATEFP = c("05","12","13",
                                              "21","22","28",
                                              "29","45","48",
                                              "51",
                                              "01","47","37"))

# county_choices <- county_selected %>% #@data %>%
#   select(STATEFP, NAME, GEOID) %>%
#   left_join(ITTS_states_choices) %>%
#   select(-c(STATEFP)) %>%
#   arrange(statename)

cty_ch= county_choices$GEOID
state_ch = state_choices$GEOID
port_ch = ports_base$GEOID
names(port_ch) = ports_base$NAME
names(cty_ch)=  county_choices$county_lab
names(state_ch)= state_choices$NAME




domestic_tab <- 
  
  
  argonTabItem(
    tabName = "maps_tabs",
    
    argonRow(
      
      argonColumn(
        
        width = 12,
        
        h1("Select map"),
        tags$p('Select a tab to see freight movements between the ITTS members, the ITTS members and US states, or ITTS members and international trade regions'),
        br(),
        argonTabSet(
          
          id = "tabset_maps",
          
          card_wrapper = FALSE,
          
          horizontal = TRUE,
          
          size = "sm",
          
          width = 12, 
          
          argonTab(
            active = T,
            tabName = "ITTS County to County Trade",
            
            argonCard(
              width = 12,
              
              argonRow(
                width = 12,
                center = TRUE,
                argonColumn(width = 12,
                            #argonCard(width =12,
                            tags$h2("County to County Trade Map"),
                            tags$p("This tab displays and summarizes FAF data disaggregated at the county level for the ITTS members states, 
                      as well as other southern states in the region.
                      The map below displays the data for a specific county which can be selected by clicking the county or choosing from the 'County' drop down box to the right. 
                      By default the map displays the combined outbound and inbound freight movements by all modes for the selected county to all others in the region. 
                      The data can be refined further using the 'Inbound/Outbound', 'Mode Type', and 'Commodity' drop down boxes. 
                      These freight movements are summarized for three vintages 2017, 2020, and 2050 by tonnage or value, 
                      any of which can be selected from the 'Freight Measure' drop down box. 
                      Additionally, the 'Top Partners' slide selector can be used to show only the top X trade partners in the region, 
                      by default all counties trading with the selected county are shown."
                                   #)
                            )),
                argonColumn(width = 10,
                            tags$div(
                              class = "map-container",
                              tags$div(
                                id = "leafletBusy",
                                class = "map-loading",
                                tags$p("Loading Map...")
                              ),
                              leafletOutput('odmap',height = map_height)
                            )
                ),
                
                argonColumn(width = 2,
                            
                            tags$div(title = "Select County for analysis.",
                                     selectizeInput('county_opts',
                                                    label='County',
                                                    choices = cty_ch,
                                                    multiple = F,
                                                    selected = '48453',
                                                    options = list(
                                                      placeholder = 'Select County',
                                                      searchField = c('NAME','county_lab','statename', 'GEOID')
                                                    )
                                     )),
                            
                            tags$div(
                              title = "Select whether the county of analysis is treated as an origin, destination, or, if outbound and inbound movements should be combined, select Both",
                              selectInput(inputId = "OD_opts", label = "Inbound/Outbound", choices = od)
                            ),
                            
                            
                            tags$div(
                              title = "Select the a mode of transport to filter the analysis by or All",
                              selectInput(inputId = 'dms_mode_opts',label = 'Mode Type',choices =c("All" = "All",modes))
                            ),
                            
                            tags$div(
                              title = "Select the commodity to filter the analysis by or All",
                              selectInput(inputId = 'sctg2_opts',label = 'Commodity',choices = c("All"="All", commodities))
                            ),
                            
                            tags$div(
                              title = "Select the number of top counties to highlight on the map",
                              sliderTextInput(inputId = 'n_top', label = 'Top Partners',
                                              #min = 1, max = length(unique(county_choices$GEOID)),
                                              #step = 25,
                                              choices = c(1, 5, 10, 50, 100, seq(250,1100,250), length(unique(county_choices$GEOID))),
                                              grid=T,
                                              selected = length(unique(county_choices$GEOID)))
                            ),
                            
                            tags$div(
                              title = "Select a measurement of freight movement to display on the map",
                              selectInput(inputId = "Value_opts", label = "Freight Measure", choices = c("Tons 2017"="tons_2017",
                                                                                                         "Tons 2020" = "tons_2020",
                                                                                                         "Tons 2050" = "tons_2050",
                                                                                                         "Value 2017" = "value_2017",
                                                                                                         "Value 2020" = "value_2020",
                                                                                                         "Value 2050" = "value_2050"),
                                          selected ='value_2017')
                            ),
                            tags$div(
                              title = "Select a scenario for freight movement to display on the map",
                              selectInput(inputId = "Scenario_opt", label = "Scenario Options", choices = c('Baseline',
                                                                                                            'Scenario 1' = '_s1',
                                                                                                            'Scenario 2' = '_s2',
                                                                                                            'Scenario 3' = '_s3',
                                                                                                            'Scenario 4' = '_s4',
                                                                                                            'Scenario 5' = '_s5',
                                                                                                            'Scenario 6' = '_s6'),
                                          selected = 'Baseline')
                            )
                            
                ))), #end of map and inputs card
            
            argonCard(
              width = 12,
              argonRow(
                width = 12,
                argonColumn(width = 12,
                            #argonCard(width =12,
                            tags$h2("County to County Trade Table"),
                            tags$p("The table below displays the data for the specific county and filters selected above. 
                                 The table displays trade between this county and all others by tonnage and value by all three vintages 2017, 2020, and 2050.
                                 The table can be downloaded in a CSV file format using the 'Download Data' button below."
                                   #)
                            )),
                argonColumn(width=10,h2(textOutput('table_title'))),
                argonColumn(width = 10, h2(textOutput('scenario_title'))),
                argonColumn(width=2,downloadButton('download_cc','Selected Data')),# class = "butt1")),
                br(),br(),br()
              ),
              
              argonRow(
                width = 12,
                DT::dataTableOutput("subsetSETTS")
              ))#end of table card
          ),
          
          
          argonTab(tabName = "ITTS County/State to State Trade",
                   #using cs as a suffix
                   argonCard(
                     width = 12,
                     
                     argonRow(
                       width = 12,
                       center = TRUE,
                       argonColumn(width = 12,
                                   #argonCard(width =12,
                                   tags$h2("County/State to State Trade Map"),
                                   tags$p("This tab displays FAF data flows between the ITTS members 
                                      (as well as other southern states in the region) and all US states. 
                                      The 'Focus Map on' filter allows you to switch between the ITTS counties OR states as the basis for summarizing the data. 
                                      A specific county or state can be selected by clicking on the map or choosing from the drop down box to the right. 
                                      By default the map displays the combined outbound and inbound freight movements by all modes for the selected county/state to all other states in the US. 
                                      The data can be refined further using the 'Inbound/Outbound', 'Mode Type', and 'Commodity' drop down boxes. 
                                      These freight movements are summarized for three vintages 2017, 2020, and 2050 by tonnage or value, 
                                      any of which can be selected from the 'Freight Measure' drop down box. 
                                      Additionally, the 'Top Partners' slide selector can be used to show only the top X trade partners."
                                          #)
                                   )),
                       argonColumn(width = 10,
                                   tags$div(
                                     class = "map-container",
                                     tags$div(
                                       id = "leafletBusy",
                                       class = "map-loading",
                                       tags$p("Loading Map...")
                                     ),
                                     leafletOutput('odmap_cs',height = map_height*1.3)
                                   )
                       ),
                       
                       argonColumn(width = 2,
                                   
                                   tags$div(title = "Switch between County to/from State to State to/from State", #start switch
                                            
                                            prettyRadioButtons(
                                              inputId = "cors_opts",
                                              label = "Focus Map on:",
                                              choices = c("County" = "c2c", "State" = "s2s"),
                                              selected= c("County" = "c2c"),
                                              inline = TRUE,
                                              shape='round',
                                              status = "info",
                                              fill = TRUE
                                            )
                                   ), #end
                                   
                                   tags$div(title = "Select County/State for analysis.", #startdiv
                                            selectizeInput('county_opts_cs',
                                                           label='County',
                                                           
                                                           #choices = unique(county_choices$county_lab),
                                                           choices = cty_ch,#cbind(county_choices$county_lab, value = county_choices$GEOID),
                                                           multiple = F,
                                                           selected = '48453',
                                                           #selected = NULL,
                                                           options = list(
                                                             placeholder = 'Select County',
                                                             searchField = c('NAME','county_lab','statename', 'GEOID')
                                                           )
                                            )),#end div
                                   tags$div( #startdiv
                                     title = "Select whether the county/state of analysis is treated as an origin, destination, or, if outbound and inbound movements should be combined, select Both",
                                     selectInput(inputId = "OD_opts_cs", label = "Inbound/Outbound", choices = od)
                                   ), #enddiv
                                   
                                   tags$div(
                                     title = "Select the a mode of transport to filter the analysis by or All.",
                                     selectInput(inputId = 'dms_mode_opts_cs',label = 'Mode Type',choices =c("All" = "All",modes))
                                   ),
                                   
                                   tags$div(
                                     title = "Select the commodity to filter the analysis by or All",
                                     selectInput(inputId = 'sctg2_opts_cs',label = 'Commodity',choices = c("All"="All", commodities))
                                   ),
                                   
                                   
                                   
                                   tags$div(
                                     title = "Select the number of top counties to highlight on the map",
                                     sliderInput(inputId = 'n_top_cs', label = 'Top Partners',min = 0, max = 50, step = 5, value = 50)
                                   ),
                                   
                                   tags$div(
                                     title = "Select a measurement of freight movement to display on the map",
                                     selectInput(inputId = "Value_opts_cs",label = "Freight Measure", choices = c("Tons 2017"="tons_2017",
                                                                                                                  "Tons 2020" = "tons_2020",
                                                                                                                  "Tons 2050" = "tons_2050",
                                                                                                                  "Value 2017" = "value_2017",
                                                                                                                  "Value 2020" = "value_2020",
                                                                                                                  "Value 2050" = "value_2050"),
                                                 selected ='value_2017')),
                                   
                                   tags$div(
                                     title = "Select a scenario for freight movement to display on the map",
                                     selectizeInput(inputId = "Scenario_opt_cs", label = "Scenario Options", choices = c('Baseline',
                                                                                                                         'Scenario 1' = '_s1',
                                                                                                                         'Scenario 2' = '_s2',
                                                                                                                         'Scenario 3' = '_s3',
                                                                                                                         'Scenario 4' = '_s4',
                                                                                                                         'Scenario 5' = '_s5',
                                                                                                                         'Scenario 6' = '_s6'),
                                                    selected = 'Baseline')
                                   )
                       ))), #end of column/row
                   
                   argonCard(
                     width = 12,
                     argonColumn(width = 12,
                                 #argonCard(width =12,
                                 tags$h2("County/State to State Trade Table"),
                                 tags$p("The table below displays the data for the specific county OR state and filters selected above. 
                                 The table displays both tonnage and value by all three vintages 2017, 2020, and 2050.
                                 The table can be downloaded in a CSV file format using the 'Download Data' button below."
                                        #)
                                 )),
                     argonRow(
                       width = 12,
                       argonColumn(width=10,h2(textOutput('table_title_cs'))),
                       argonColumn(width = 10, h2(textOutput('scenario_title_cs'))),
                       argonColumn(width=2,downloadButton('download_cs','Selected Data')),#, class = "butt_down_cs")),
                       br(),br(),br()
                     ),
                     
                     argonRow(
                       width = 12,
                       DT::dataTableOutput("subsetSETTS_cs")
                     ))
          ), #end county to state tab
          
          argonTab(tabName = "ITTS International Trade", 
                   #using in as a suffix
                   argonCard(
                     width = 12,
                     
                     argonRow(
                       width = 12,
                       center = TRUE,
                       argonColumn(width = 12,
                                   #argonCard(width =12,
                                   tags$h2("Port/State to International Trade Map"),
                                   tags$p("This tab displays US Trade Online data for freight movements between the ITTS member states 
                                      (as well as other southern states in the region) and international trade regions.  
                                      The 'Focus Map on' filter allows you to switch between selecting ports OR states as the basis for summarizing the data. 
                                      A specific port or state can be selected by clicking on the map or choosing from the drop down box to the right. 
                                      When focusing on Ports, only the mode type handled by the port is being shown;
                                      the 'Mode Type' drop down box can be used to only disply ports of that mode type e.g. only airports. 
                                      When focusing on states, the 'Mode Type' is used to select which freight movements involing the selected state are summarized,
                                      selecting 'All' will combine all modes together. 
                                      By default the map displays the combined outbound and inbound freight movements 
                                      by all modes for the selected state to international trade regions. 
                                      The freight movements summarized can be refined further using the 'Inbound/Outbound' and 'Commodity' drop down boxes. 
                                      These freight movements are available in three vintages 2017, 2020, and 2050 by tonnage or value, 
                                      any of which can be selected from the 'Freight Measure' drop down box. 
                                      Additionally, the 'Top Partners' slide selector can be used to show only the top X trade partners."
                                          #)
                                   )),
                       argonColumn(width = 10,
                                   tags$div(
                                     class = "map-container",
                                     tags$div(
                                       id = "leafletBusy",
                                       class = "map-loading",
                                       tags$p("Loading Map...")
                                     ),
                                     leafletOutput('odmap_in',height = map_height)
                                   )
                       ),
                       
                       argonColumn(width = 2,
                                   
                                   tags$div(title = "Switch between Port to/from International to State to/from State", #start switch
                                            
                                            prettyRadioButtons(
                                              inputId = "cors_opts_in",
                                              label = "Focus Map on:", 
                                              choices = c("Port" = "p2n", "State" = "s2n"),
                                              selected= c("State" = "s2n"),
                                              inline = TRUE, 
                                              shape='round',
                                              status = "info",
                                              fill = TRUE
                                            )
                                   ), #end
                                   
                                   tags$div(title = "Select Port/State for analysis.", #startdiv
                                            selectizeInput('county_opts_in',
                                                           label='State',
                                                           
                                                           #choices = unique(county_choices$county_lab),
                                                           choices = state_ch,#cbind(county_choices$county_lab, value = county_choices$GEOID),
                                                           multiple = F,
                                                           selected = 'Select State',
                                                           #selected = NULL,
                                                           options = list(
                                                             placeholder = 'Select State', 
                                                             searchField = c('NAME','county_lab','statename', 'GEOID')
                                                           )
                                            )),#end div
                                   tags$div( #startdiv
                                     title = "Select whether the Port/State of analysis is treated as an origin, destination, or, if outbound and inbound movements should be combined, select Both",
                                     selectInput(inputId = "OD_opts_in", label = "Inbound/Outbound", choices = od)
                                   ), #enddiv
                                   
                                   tags$div(
                                     title = "Select the a mode of transport to filter the analysis by or All. Note: Border Crossing include values measurements but tonnage measurements are not known.",
                                     selectInput(inputId = 'dms_mode_opts_in',label = 'Mode Type',choices =c("All" = "All",modes_int))
                                   ),
                                   
                                   tags$div(
                                     title = "Select the commodity to filter the analysis by or All",
                                     selectInput(inputId = 'sctg2_opts_in',label = 'Commodity',choices = c("All"="All", commodities))
                                   ),
                                   
                                   tags$div(
                                     title = "Select the number of top counties to highlight on the map",
                                     sliderInput(inputId = 'n_top_in', label = 'Top Partners',min = 0, max = 8, value = 8)
                                   ),
                                   
                                   tags$div(
                                     title = "Select a measurement of freight movement to display on the map",
                                     selectInput(inputId = "Value_opts_in",label = "Freight Measure", choices = c("Tons 2019"="tons_2019",
                                                                                                                  "Tons 2021" = "tons_2021",
                                                                                                                  "Value 2019" = "value_2019",
                                                                                                                  "Value 2021" = "value_2021"),
                                                 selected ='value_2019')),
                                   tags$div(
                                     title = "Select a scenario for freight movement to display on the map",
                                     selectInput(inputId = "Scenario_opt_in", label = "Scenario Options", choices = c('Baseline',
                                                                                                                      'Scenario 1' = '_s1',
                                                                                                                      'Scenario 2' = '_s2',
                                                                                                                      'Scenario 3' = '_s3',
                                                                                                                      'Scenario 4' = '_s4',
                                                                                                                      'Scenario 5' = '_s5',
                                                                                                                      'Scenario 6' = '_s6'),
                                                 selected = 'Baseline')
                                   )
                       ))), #end of column/row
                   
                   argonCard(
                     width = 12,
                     argonColumn(width = 12,
                                 #argonCard(width =12,
                                 tags$h2("County/State to State Trade Table"),
                                 tags$p("The table below displays the data for the specific port OR state and filters selected above. 
                                 The table displays both tonnage and value by all three vintages 2017, 2020, and 2050.
                                 The table can be downloaded in a CSV file format using the 'Download Data' button below."
                                        #)
                                 )),
                     argonRow(
                       width = 12,
                       argonColumn(width=10,h2(textOutput('table_title_in'))),
                       argonColumn(width = 10, h2(textOutput('scenario_title_in'))),
                       argonColumn(width=2,downloadButton('download_in','Selected Data')),#, class = "butt_down_cs")),
                       br(),br(),br()
                     ),
                     
                     argonRow(
                       width = 12,
                       DT::dataTableOutput("subsetSETTS_in")
                     ))
          ) #end int tab
          
          
        ) #end tab set
      ) #end first colum
    ) #end first row
    
    
    
  )#end maps tab