
domestic_tab <- 
  
  
  argonTabItem(
    tabName = "maps_tabs",
    
    argonRow(
      
      argonColumn(
        
        width = 12,
        
        h1("Select Analysis"),
        
        tags$p("Select one of three analysis tabs to view freight flows:"),
        tags$ol(
          tags$li(tags$p("between counties within the ITTS region")),
          tags$li(tags$p("between ITTS region states/counties and other states")),
          tags$li(tags$p("between the ITTS region and international trade regions"))
        ),
        
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
                            tags$h2("County to County Trade Map"),
                            tags$p("This tab displays and summarizes FAF data disaggregated at the county level for the ITTS members states, as well as other southern states in the region. 
                             Select from the “County” drop down box on the right to display data for a specific county. 
                             By default, the map will display the combined outbound and inbound freight movements for all modes for the selected county to all others in the region. 
                             The data can be refined further using the “Inbound/Outbound”, “Mode Type”, and “Commodity” drop down boxes for 2017, 2020, and 2050 by tonnage or value, which can be selected from the “Freight Measure” drop down box. 
                             Additionally, the “Top Partners” slide selector filters to show only the top X trade partners in the region, by default all trade partners are shown."
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
                                                    choices = NULL,
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
                            )
                            
                ))), 
            
            argonCard(
              width = 12,
              argonRow(
                width = 12,
                argonColumn(width = 12,
                            tags$h2("County to County Trade Table"),
                            tags$p("The table below displays the data for the specific county and filters selected above. 
                                 The table displays trade between this county and all others by tonnage and value by all three vintages 2017, 2020, and 2050.
                                 The table can be downloaded in a CSV file format using the 'Download Data' button below."
                            )),
                argonColumn(width=10,h2(textOutput('table_title'))),
                argonColumn(width=2,downloadButton('download_cc','Download Data')),# class = "butt1")),
                br(),br(),br()
              ),
              
              argonRow(
                width = 12,
                DT::dataTableOutput("subsetSETTS")
              ))
          ),
          
          
          argonTab(tabName = "ITTS County/State to State Trade",
                   argonCard(
                     width = 12,
                     
                     argonRow(
                       width = 12,
                       center = TRUE,
                       argonColumn(width = 12,
                                   tags$h2("County/State to State Trade Map"),
                                   tags$p("This tab displays FAF data flows between the ITTS members (as well as other southern states in the region) and all US states. The “Focus Map on” filter allows you to switch between summarizing data for ITTS counties OR states, then select a specific county or state by clicking on the map or via the drop down box on the right. By default, the map displays the combined outbound and inbound freight movements for all modes for the selected county/state to all other states in the US. The data can be refined further using the “Inbound/Outbound”, “Mode Type”, and “Commodity” drop down boxes for 2017, 2020, and 2050 by tonnage or value, which can be selected from the “Freight Measure” drop down box. 
                                      Additionally, the “Top Partners” slide selector filters to show only the top X trade partners, by default all trade partners are shown."
                                   )),
                       argonColumn(width = 10,
                                   tags$div(
                                     class = "map-container",
                                     tags$div(
                                       id = "leafletBusy",
                                       class = "map-loading",
                                       tags$p("Loading Map...")
                                     ),
                                     leafletOutput('odmap_cs',height = map_height)
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
                                   
                                   tags$div(title = "Select County/State for analysis.", 
                                            selectizeInput('county_opts_cs',
                                                           label='County',
                                                           choices = NULL, 
                                                           multiple = F,
                                                           selected = '48453',
                                                           options = list(
                                                             placeholder = 'Select County',
                                                             searchField = c('NAME','county_lab','statename', 'GEOID')
                                                           )
                                            )),
                                   tags$div( 
                                     title = "Select whether the county/state of analysis is treated as an origin, destination, or, if outbound and inbound movements should be combined, select Both",
                                     selectInput(inputId = "OD_opts_cs", label = "Inbound/Outbound", choices = od)
                                   ), 
                                   
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
                                     
                                     sliderTextInput(inputId = 'n_top_cs', label = 'Top Partners',
                                                     choices = c(1, 5, 10, 25, length(unique(county_choices$GEOID))),
                                                     grid=T,
                                                     selected = length(unique(county_choices$GEOID)))
                                     
                                     
                                   ),
                                   
                                   tags$div(
                                     title = "Select a measurement of freight movement to display on the map",
                                     selectInput(inputId = "Value_opts_cs",label = "Freight Measure", choices = c("Tons 2017"="tons_2017",
                                                                                                                  "Tons 2020" = "tons_2020",
                                                                                                                  "Tons 2050" = "tons_2050",
                                                                                                                  "Value 2017" = "value_2017",
                                                                                                                  "Value 2020" = "value_2020",
                                                                                                                  "Value 2050" = "value_2050"),
                                                 selected ='value_2017'))
                       ))), 
                   
                   argonCard(
                     width = 12,
                     argonColumn(width = 12,
                                 tags$h2("County/State to State Trade Table"),
                                 tags$p("The table below displays the data for the specific county OR state and filters selected above. 
                                 The table displays both tonnage and value by all three vintages 2017, 2020, and 2050.
                                 The table can be downloaded in a CSV file format using the 'Download Data' button below."
                                 )),
                     argonRow(
                       width = 12,
                       argonColumn(width=10,h2(textOutput('table_title_cs'))),
                       argonColumn(width=2,downloadButton('download_cs','Download Data')),
                       br(),br(),br()
                     ),
                     
                     argonRow(
                       width = 12,
                       DT::dataTableOutput("subsetSETTS_cs")
                     ))
          ),
          
          argonTab(tabName = "ITTS International Trade", 
                   argonCard(
                     width = 12,
                     
                     argonRow(
                       width = 12,
                       center = TRUE,
                       argonColumn(width = 12,
                                   tags$h2("Port/State to International Trade Map"),
                                   tags$p("This tab displays US Trade Online data for freight movements between the ITTS member states (as well as other southern states in the region) and international trade regions. The “Focus Map on” filter allows you to switch between summarizing data for specific ports OR states, which can be selected by clicking on the map or choosing from the drop down box on the right. For Ports, which includes water ports, airports, and land ports of entry, only the mode type handled by the port is shown via the “Mode Type” drop down box. For states, “Mode Type” selects which modes are displayed, and selecting 'All' will combine all modes. By default, the map displays the combined outbound and inbound freight movements for all modes for the selected state to international trade regions. The freight movements summarized can be refined further using the “Inbound/Outbound” and “Commodity” drop down boxes for 2017, 2020, and 2050 by tonnage or value, which can be selected from the “Freight Measure” drop down box. Additionally, the “Top Partners” slide selector filters to show only the top X trade partners, by default all trade partners are shown."
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
                                   
                                   tags$div(title = "Switch between Port to/from International to State to/from State", 
                                            
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
                                   ), 
                                   
                                   tags$div(title = "Select Port/State for analysis.", 
                                            selectizeInput('county_opts_in',
                                                           label='State',
                                                           choices = state_ch,
                                                           multiple = F,
                                                           selected = c("Texas", value = "48"),
                                                           options = list(
                                                             placeholder = 'Select State'
                                                           )
                                            )),
                                   tags$div( 
                                     title = "Select whether the Port/State of analysis is treated as an origin, destination, or, if outbound and inbound movements should be combined, select Both",
                                     selectInput(inputId = "OD_opts_in", label = "Inbound/Outbound", choices = od)
                                   ), 
                                   
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
                                     selectInput(inputId = "Value_opts_in",label = "Freight Measure", choices = c("Tons 2019"="Tons_2019",
                                                                                                                  "Tons 2021" = "Tons_2021",
                                                                                                                  "Value 2019" = "Value_2019",
                                                                                                                  "Value 2021" = "Value_2021"),
                                                 selected ='Value_2019'))
                       ))),
                   
                   argonCard(
                     width = 12,
                     argonColumn(width = 12,
                                 tags$h2("County/State to State Trade Table"),
                                 tags$p("The table below displays the data for the specific port OR state and filters selected above. 
                                 The table displays both tonnage and value by all three vintages 2017, 2020, and 2050.
                                 The table can be downloaded in a CSV file format using the 'Download Data' button below."
                                 )),
                     argonRow(
                       width = 12,
                       argonColumn(width=10,h2(textOutput('table_title_in'))),
                       argonColumn(width=2,downloadButton('download_in','Download Data')),
                       br(),br(),br()
                     ),
                     
                     argonRow(
                       width = 12,
                       DT::dataTableOutput("subsetSETTS_in")
                     ))
          ) 
          
          
        ) 
      ) 
    ) 
    
    
    
  )
