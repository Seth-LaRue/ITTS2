
# 
# cty_ch= county_choices$GEOID
# state_ch = state_choices$GEOID
# port_ch = ports_base$GEOID
# names(port_ch) = ports_base$NAME
# names(cty_ch)=  county_choices$county_lab
# names(state_ch)= state_choices$NAME
# 
# commodities <- c("Agriculture and Fish",
#                  "Energy Products", 
#                  "Food, Alcohol and Tobacco",
#                  "Machinery, Electric, and Precision Instruments",
#                  "Mixed Freight",
#                  "Waste and Scrap",
#                  "Nonmetallic Mineral and Base Metal Products",
#                  "Raw and Finished Wood Products",
#                  "Chemicals, Pharmaceuticals, Plastics, and Rubber",
#                  "Vehicles and Transportation Equipment",
#                  "Textiles and Leather",
#                  "Aggregates")
# 
# modes <- c("Truck"="1",
#            "Rail" ="2",
#            "Water"="3",
#            "Air (Includes truck-air)"="4",
#            "Multiple Modes and Mail"="5",
#            "Pipeline"="6",
#            "Other and Unknown"="7")
# 
# od <- c("Inbound","Outbound","Within ITTS")
# 
# 
# scenario_choices <- c(
#   "Baseline" = "_s0",
#   "Scenario 1: Respond to Heightened Supply Chain Risks"= "_s1",
#   "Scenario 2: Leverage Multi-State Strength"= "_s2",
#   "Scenario 3: Embrace Technology Transformations"= "_s3"
# )


#TAB 1 ---------------
        summary_tab<-argonPage(
          

          
          fluidRow(
            
            tags$head(tags$style(".btn-light {background-color: #ffffff;color: black}")),
            
            
            column(12,
                   h1("Scenario Comparison"),
                   p("This tab is to help users compare differences between the scenarios. Please select which commodities, directions, modes, and scenarios you would like to see displayed and press the button to the right to run the analysis."))),
          #First Card w inputs/button --------------
          tabsetPanel(id = "InputSet",
                      tabPanel(id = "InputPanel",
          argonCard(width = 12,
                    argonRow(width = 12,
                             argonColumn(width = 2,
                                         tags$div(
                                          # class = "clean-picker",
                                           pickerInput(
                                             inputId= "stab2_comps",
                                             label = "Scenario(s)",
                                             choices = c("Baseline" = "_s0",
                                                         "Scenario 1: Respond to Heightened Supply Chain Risks"= "_s1",
                                                         "Scenario 2: Leverage Multi-State Strength"= "_s2",
                                                         "Scenario 3: Embrace Technology Transformations"= "_s3"),
                                             multiple = TRUE,
                                             options = list(
                                               size = 4,
                                               title = "Please select the Scenario(s) to compare",
                                               'actions-box' = TRUE,
                                               'selected-text-format' = "count > 1")))
                             ),
                             
                             argonColumn(width = 2,
                                         tags$div(
                                           #title = "Select States(s) for comparison",
                                           pickerInput(inputId = "stab2_states", 
                                                       label = "State(s)", 
                                                       choices = c("All States" = "99", state_ch),
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
                                                       choices = c("Inbound","Outbound","Within ITTS"), multiple = TRUE, 
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
                    )))), #end of input card
          tags$div(style = 'z-index:1000;',
uiOutput("output_panel_1"),
uiOutput("output_panel_2"),
uiOutput("output_panel_3"),
uiOutput("output_panel_4"))
        )
  
