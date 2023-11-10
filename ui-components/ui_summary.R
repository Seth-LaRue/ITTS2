
# 
cty_ch= county_choices$GEOID
state_ch = state_choices$GEOID
port_ch = ports_base$GEOID
names(port_ch) = ports_base$NAME
names(cty_ch)=  county_choices$county_lab
names(state_ch)= state_choices$NAME

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

modes <- c("Truck"="1",
           "Rail" ="2",
           "Water"="3",
           "Air (Includes truck-air)"="4",
           "Multiple Modes and Mail"="5",
           "Pipeline"="6",
           "Other and Unknown"="7")

od <- c("Inbound","Outbound","Within ITTS")


#TAB 1 ---------------
        summary_tab<-argonPage(
          

          
          fluidRow(
            
            tags$head(tags$style(".btn-light {background-color: #ffffff;color: black}")),
            
            
            column(12,
                   h1("Scenario Comparison"))),
          #First Card w inputs/button --------------
          argonCard(width = 12,
                    argonRow(width = 12,
                             argonColumn(width = 2,
                                         tags$div(
                                          # class = "clean-picker",
                                           pickerInput(
                                             inputId= "stab2_comps",
                                             label = "Scenario(s)",
                                             choices = c("Baseline" = "s0",
                                                         "Scenario 1"= "s1",
                                                         "Scenario 2"= "s2",
                                                         "Scenario 3"= "s3",
                                                         "Scenario 4"= "s4",
                                                         "Scenario 5"= "s5",
                                                         "Scenario 6"= "s6"),
                                             multiple = TRUE,
                                             options = list(
                                               size = 7,
                                               title = "Please select the Scenario(s) to compare",
                                               'actions-box' = TRUE,
                                               'selected-text-format' = "count > 1"
                                             )))
                             ),
                             
                             argonColumn(width = 2,
                                         tags$div(
                                           #title = "Select States(s) for comparison",
                                           pickerInput(inputId = "stab2_states", 
                                                       label = "State(s)", 
                                                       choices = state_ch,
                                                       multiple = TRUE,
                                                       options = list(
                                                         size = 7,
                                                         title = "Please select the State(s) to include",
                                                         'actions-box' = TRUE,
                                                         'selected-text-format' = "count > 1"
                                                       ))
                             )),
                             
                             argonColumn(width = 2,
                                         tags$div(
                                           #title = "Select whether the analysis is inbound, outbound, or within ITTS trades",
                                           pickerInput(inputId = "stab2_OD", 
                                                       label = "Inbound/Outbound", 
                                                       choices = od, multiple = TRUE, 
                                                       options = list(
                                                         size = 7,
                                                         title = "Please select the Direction(s) to include",
                                                         'actions-box' = TRUE,
                                                         'selected-text-format' = "count > 1"
                                                       )))
                                         ),
                             
                             argonColumn(width = 2,
                                         tags$div(
                                           #title = "Select the a mode of transport to filter the analysis by or All",
                                           pickerInput(inputId = 'stab2_mode',
                                                       label = 'Mode Type',
                                                       choices = modes, 
                                                       multiple = TRUE, 
                                                       options = list(
                                                         size = 7,
                                                         title = "Please select the Scenario(s) you'd like to compare",
                                                         'actions-box' = TRUE,
                                                         'selected-text-format' = "count > 1"
                                                       )))
                                         ),
                             
                             argonColumn(width = 2,
                                         tags$div(
                                           #title = "Select the commodity to filter the analysis by or All",
                                           pickerInput(inputId = 'stab2_commodity',
                                                       label = 'Commodity',
                                                       choices = commodities, 
                                                       multiple = TRUE, 
                                                       options = list(
                                                         size = 7,
                                                         title = "Please select the Scenario(s) you'd like to compare",
                                                         'actions-box' = TRUE,
                                                         'selected-text-format' = "count > 1"
                                                       )))
                                         )),
                             argonRow(width = 12,
                             argonColumn(width = 2,
                                         tags$div(
                                           title = "Run based on selected filter",
                                           actionButton("stab2_mainbutt", "Run based on Page Filters"))
                             )
                    )), #end of input card
          #second card line graphs ---------
          argonCard(width = 12, 
                    argonRow(width = 12,h2("Growth year over year", align = 'center')),
                    argonRow(width = 12,
                             argonColumn(width = 6, 
                                         #h2("Tons Growth", align = "center"), 
                                         plotlyOutput("stab2_line_tons", width = "auto", height = "auto")
                             ),
                             argonColumn(width = 6, 
                                         #h2("Value Growth", align = "center"), 
                                         plotlyOutput("stab2_line_value", width = "auto", height = "auto")
                             )
                    )),
          #third card ----
          argonCard(width = 12,
                    argonRow(width = 12,h2("Percent Growth by State")),
                    argonRow(width = 12,
                             argonColumn(width = 6, 
                                         #h2("State Growth Tons", align = "center"), 
                                         plotlyOutput("stab2_tons_state_growth_dotplot", width = "auto", height = "auto")
                             ),
                             argonColumn(width = 6, 
                                         #h2("State Growth Value", align = "center"), 
                                         plotlyOutput("stab2_value_state_growth_dotplot", width = "auto", height = "auto")
                             )),
                    argonRow(width = 12,h2("Percent Growth by Mode", align = 'center')),
                    argonRow(width = 12,
                             argonColumn(width = 6, 
                                         #h2("Mode Growth Tons", align = "center"), 
                                         plotlyOutput("stab2_tons_mode_growth_dotplot", width = "auto", height = "auto")
                             ),
                             argonColumn(width = 6, 
                                         #h2("Mode Growth Value", align = "center"), 
                                         plotlyOutput("stab2_value_mode_growth_dotplot", width = "auto", height = "auto")
                             )),
                    argonRow(width = 12,h2("Percent Growth by Commodity", align = "center")),
                    argonRow(width = 12,
                             argonColumn(width = 6, 
                                         #h2("Commodity Growth Tons", align = "center"), 
                                         plotlyOutput("stab2_tons_com_growth_dotplot", width = "auto", height = "auto")
                             ),
                             argonColumn(width = 6, 
                                         #h2("Commodity Growth Value", align = "center"), 
                                         plotlyOutput("stab2_value_com_growth_dotplot", width = "auto", height = "auto")
                             ))
          ), #end of card
          
          argonCard(width = 12, 
                    argonRow(width = 12, 
                             argonColumn(width = 2,
                                         
                                         tags$div(
                                           title = "Select a measurement of freight movement to display on the map",
                                           selectInput(inputId = "stab2_value_opts", label = "Freight Measure", choices = c("Tons 2017"="tons_2017",
                                                                                                                            "Tons 2020" = "tons_2020",
                                                                                                                            "Tons 2050" = "tons_2050",
                                                                                                                            "Value 2017" = "value_2017",
                                                                                                                            "Value 2020" = "value_2020",
                                                                                                                            "Value 2050" = "value_2050"),
                                                       selected ='value_2017'))
                                         
                             )),
                    argonRow(width = 12,
                             argonColumn(width = 6, 
                                         h2("Trends in Mode Share", align = "center"), 
                                         plotlyOutput("stab2_mode_bar", width = "auto", height = "auto")
                             ),
                             argonColumn(width = 6, 
                                         h2("Trends in Direction", align = "center"), 
                                         plotlyOutput("stab2_dir_bar", width = "auto", height = "auto")
                             )
                    ),
                    argonRow(width = 12,
                             argonColumn(width = 12, 
                                         h2("Trends in Commodities", align = "center"), 
                                         plotlyOutput("stab2_com_bar", width = "auto", height = "auto")
                             )
                    )),
          argonCard(width = 12,
                    argonRow(width = 12,
                             selectInput(
                               inputId= "stab2_sankey_filt",
                               label = "Scenario(s)",
                               choices = c("Baseline" = "s0",
                                           "Scenario 1"= "s1",
                                           "Scenario 2"= "s2",
                                           "Scenario 3"= "s3",
                                           "Scenario 4"= "s4",
                                           "Scenario 5"= "s5",
                                           "Scenario 6"= "s6")),
                             argonColumn(width = 12, 
                                         h2("Commodity Flow Breakdown", align = "center"), 
                                         plotlyOutput("stab2_sankey", width = "auto", height = "auto")
                             )
                    )
                    #Commodity Flow Trends Graphs argonCard
          )
        )
     
  
