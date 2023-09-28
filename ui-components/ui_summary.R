
# 
cty_ch= county_choices$GEOID
  state_ch = state_choices$GEOID
  port_ch = ports_base$GEOID
  names(port_ch) = ports_base$NAME
  names(cty_ch)=  county_choices$county_lab
  names(state_ch)= state_choices$NAME
  
  argonTabItem(
    tabName = "summary_tab",
    
    argonTabSet(
      
      id = "tabset_summary",
      
      card_wrapper = FALSE,
      
      horizontal = TRUE,
      
      size = "sm",
      
      width = 12, 
    
    argonTab(
      tabName = 'Baseline Summary',
      
      argonCard(width = 12,
                
                title = textOutput("totalTonsValTXT"),
                
                #first row of graphs
                argonRow(width = 12,
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
                                       title = "Select a measurement of freight movement to display on the map",
                                       selectInput(inputId = "Value_opts", label = "Freight Measure", choices = c("Tons 2017"="tons_2017",
                                                                                                                  "Tons 2020" = "tons_2020",
                                                                                                                  "Tons 2050" = "tons_2050",
                                                                                                                  "Value 2017" = "value_2017",
                                                                                                                  "Value 2020" = "value_2020",
                                                                                                                  "Value 2050" = "value_2050"),
                                                   selected ='value_2017')
                                     )),
                         argonColumn( width = 5, 
                                      #Flow Direction Graph
                                      h2("Flow Direction", align = "center"), 
                                      plotlyOutput("cf_flowDirection", width = "auto", height = "auto")),
                         
                         argonColumn( width = 5, 
                                      #Mode Graph
                                      h2("Domestic Mode", align = "center"), 
                                      plotlyOutput("cf_mode", width = "auto", height = "auto"))
                ),#end of first row of graphs
                
                #second row of graphs
                argonRow(width = 12,
                         argonColumn(width = 12, 
                                     #Commodity Graph
                                     h2("Commodity Type", align = "center"), 
                                     plotlyOutput("cf_commodity", width = "auto", height = "auto")
                                     )
                ),#second row of graphs end
                #third row of graphs
                argonRow(width = 12,
                         argonColumn(width = 6,
                                     #Import Origin Graph
                                     h2("Top 10 Inbound Origins", align = "center"), 
                                     plotlyOutput("cf_topInbound", width = "auto", height = "auto")
                                     ), 
                         argonColumn(width = 6, 
                                     #Export Destination Graphs
                                     h2("Top 10 Outbound Destinations", align = "center"),
                                     plotlyOutput("cf_topOutbound", width = "auto", height = "auto")
                                     )
                )#end of third row of graphs
      ) #end of Commodity Flow Profile Graphs argonCard
      
    ),
    
    argonTab(
      tabName = 'Baseline Vs. Scenario',
      argonCard(width = 12,
                argonRow(width = 12,
                         argonColumn(width = 6, 
                                     h2("Change in Mode Share", align = "center")#, 
                                     #DT::dataTableOutput("cf_modeShareChange", width = "auto", height = "auto")
                                     ),
                         argonColumn(width = 6, 
                                     h2("Trends by Commodities", align = "center")#, 
                                     #plotlyOutput("cf_commTrends", width = "auto", height = "auto")
                                     )
                ),
      ), #end of mode share and commodity change argonCard
      
      #Commodity Flow Trends Graphs argonCard
    ),
    
    argonTab(
      tabName = 'scenarios Comparsion',
      argonCard(width = 12,
                argonRow(width = 12,
                         argonColumn(width = 12#,
                                     #plotlyOutput('cf_scenarioCompare', width = 'auto',
                                     #             height = 'auto')))
                                     )
    )
    )
  )))
