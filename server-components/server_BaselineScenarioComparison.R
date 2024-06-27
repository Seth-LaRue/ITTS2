
#funcitons -----------------------
line_plot <- function(df_in, meas = "Tonnage"){
  
  if(meas == "Value USD"){
    unit = " $Million"
  } else {
    unit = " K tons"
  }
  
  df_temp <- df_in %>% mutate(year = as.numeric(year)) %>%
    left_join(scen_colors)
  
  lplot <- plot_ly(df_temp, x = ~year, y = ~value, type = 'scatter', mode = 'lines', 
                   color = ~I(scen_color),
                   linetype = ~I(lntype),
                   name = ~scen_name,
                   hovertemplate = paste0('Year: %{x}<br>', 
                                          ':%{y:.2s}', unit,'<br>')) %>%
    layout(xaxis = list(title = 'Year'),
           yaxis = list(title = paste0(meas," (",unit,")"), separatethousands= TRUE),
           legend = list(orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = 0.5,
                         y = -0.2)) %>%
    config(displayModeBar = FALSE)
  if(meas == "Value USD"){
    lplot<-lplot %>% layout(yaxis = list(title = paste0(meas," (",unit,")"), tickformat = "$~s"))
  }
  
  return(lplot)
  
}
dot_plot <- function(df_in, meas = "Tonnage"){
  
  trace <- df_in %>% select(-scenario)
  df_temp <- df_in %>% left_join(scen_colors)
  
  dplot <-  plot_ly(df_temp) %>% 
    add_trace(trace, x = ~value, y = ~str_wrap(label,25), type = 'scatter', mode = 'lines', color = ~label,
              line = list(color = "#6a6a6a",
                          dash = 'dash'
              ),
              showlegend = FALSE) %>%
    add_trace(df_temp, x = ~value, y = ~str_wrap(label,25), type = 'scatter', mode = 'markers', 
              color = ~I(scen_color),
              name = ~scen_name,
              symbol = ~I(dotmrk),
              marker = list(size = 20),
              hovertemplate = ~paste0(label,"<br>",
                                      meas,": ",formatC(100*value, format = "f",digits = 2),"%")) %>%
    layout(xaxis = list(title = '',
                        tickformat = '0%',
                        showticklabels = FALSE),
           yaxis = list(title = ''),
           margin = list(l = 100, r = 100),  # Adjust left and right margins
           height = 175 + 35 * (length(unique(df_temp$label))),
           legend = list(orientation = "h",
                         xanchor = "center",
                         x = 0.5,
                         y = -0.2)) %>%
    add_annotations(
      text = "Percent Growth",
      xref = "paper",
      yref = "paper",
      x = 1,  # Position the label at the far right end of the x-axis
      y = -0.1,
      showarrow = FALSE
    )
  #dplot
  return(dplot)
}
bar_plot_singleyear <- function(df_in, measure = 'tons_2022', sourceName = sourceName){
  meas = ifelse(stringr::str_split(measure, '_')[[1]][[1]] == 'tons','Tonnage','Value USD')
  yr = stringr::str_split(measure, '_')[[1]][[2]]
  if(meas == "Value USD"){
    unit = ""
    unit_pre = "$"
  } else {
    unit = "K tons"
    unit_pre = ""
  }
  #print(unique(df_in$scenario))
  #browser()
  df_temp <- df_in %>% left_join(scen_colors)
  
  bar_plot <- plot_ly(df_temp,
                      x = ~str_wrap(group,25),
                      y = ~value,
                      type = 'bar',
                      name = ~scen_name,
                      color = ~I(scen_color),
                      hovertemplate = ~paste0(group, "<br>",
                                              "Year: ", yr, "<br>",
                                              unit_pre, formatC(value, format = "f",digits = 2,big.mark = ","), " ",
                                              unit)) %>%
    layout(yaxis = list(title = paste0(meas," (", unit,")"), separatethousands= TRUE),
           xaxis = list(title = ""),
           margin = list(b=50),
           legend = list(orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = 0.5,
                         y = -0.4)) %>%
    config(displayModeBar = FALSE)
  
  if(meas == "Value USD"){
    bar_plot<-bar_plot %>% layout(yaxis = list(title = "Value USD", tickformat = "$~s"))
  }
  
  return(bar_plot)
}

bar_plot_yearaxis <- function(df_in, meas = 'Tonnage'){
  #browser()
  if(meas == "Value USD"){
    unit_pre = "$"
    unit = " M"
  } else {
    unit_pre = ""
    unit = "K tons"
  }
  df_temp <- df_in %>% left_join(scen_colors)
  
  bar_plot <- plot_ly(df_temp,
                      x = ~year,
                      y = ~value,
                      type = 'bar',
                      name = ~scen_name,
                      color = ~I(scen_color),
                      hovertemplate = ~paste0("Year: ", year, "<br>",
                                              unit_pre, formatC(value, format = "f",digits = 2,big.mark = ","),
                                              unit)) %>%
    layout(
           yaxis = list(title = paste0(meas, " (", unit, ")"),
                        separatethousands= TRUE, 
                        range = c(min(df_temp$value,na.rm=T)/2, max(df_temp$value,na.rm=T)*1.05)),
           xaxis = list(title = ""),
           legend = list(orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = 0.5,
                         y = -0.2)) %>%
    config(displayModeBar = FALSE)
  if(meas == "Value USD"){
    bar_plot<-bar_plot %>% layout(yaxis = list(title = "Value USD", tickformat = "$~s"))
  }
  return(bar_plot)
}
sankey_diagram <- function(df_in, meas = "Tonnage"){
  
  
  blabs <- c("Alabama", "Arkansas", "Florida", "Georgia", "Kentucky",
             "Louisiana", "Mississippi", "Missouri", "North Carolina", "South Carolina",
             "Tennessee", "Texas", "Virginia", "All States",
             
             "Truck", "Rail", "Water", "Air (Includes truck-air)", 
             "Mutliple Modes and Mail", "Pipeline", "Other and Unknown",
             
             "Aggregates", "Agriculture and Fish", "Base chemicals and Pharmaceuticals", "Coal", 
             "Food, Alcohol and Tobacco", "Furniture", "Log", "Machinery, Electric, and Precision Instruments", 
             "Mixed Freight", "Motorized vehicles", "Non-coal Energy Products", 
             "Nonmetallic Mineral and Base Metal Products", "Other Chemicals, Plastics, and Rubber", 
             "Raw and Finished Wood Products", "Textiles and Leather", "Transportation Equipment", "Waste and Scrap")
  
  
  shell_orig <- data.frame(source = blabs,
                           index = c(seq(0:37))
  )
  shell1 <- shell_orig[1:14,] %>% rename(source1 = source,
                                         index1 = index)
  shell2 <- shell_orig[15:21,] %>% rename(source2 = source, 
                                          index2 = index)
  shell3 <- shell_orig[22:38,] %>% rename(source3 = source,
                                          index3 = index)
  
  source_1t2 <- source1_temp <- shell1
  source_1t2$source2 <- shell2$source2[1]
  source_1t2$index2 <- shell2$index2[1]
  
  for(n in 2:nrow(shell2)){
    #print(n)
    source_temp <- source1_temp
    source_temp$source2 <- shell2$source2[n]
    source_temp$index2 <- shell2$index2[n]
    source_1t2 <- rbind(source_1t2, source_temp)
  }
  
  source_2t3 <- source2_temp <- shell2
  source_2t3$source3 <- shell3$source3[1]
  source_2t3$index3 <- shell3$index3[1]
  
  for(n in 2:nrow(shell3)){
    #print(n)
    source_temp <- source2_temp
    source_temp$source3 <- shell3$source3[n]
    source_temp$index3 <- shell3$index3[n]
    source_2t3 <- rbind(source_2t3, source_temp)
  }
  
  #browser()
  
  link_1t2 <- df_in %>% 
    mutate(destination = ifelse(destination == origin, "remoov", destination)) %>%
    pivot_longer(cols = c(origin, destination), values_to = "state") %>% 
    select(-name) %>% 
    filter(state %in% input$stab2_states) %>%
    #mutate(state = ifelse(nchar(origin)==5, str_sub(origin,1,2), str_sub(destination,1,2))) %>%
    left_join(state_join) %>% rename(source1 = state_lab) %>%
    left_join(ini_modecolors %>% mutate(dms_mode = as.character(dms_mode))) %>%
    rename(source2 = mode_group) %>% 
    group_by(source1, source2) %>% summarise(value = sum(value))
  
  
  link_2t3 <- df_in %>%
    mutate(destination = ifelse(destination == origin, "remoov",destination)) %>% #this ensure internal flows don't end up being counted twice
    pivot_longer(cols = c(origin, destination), values_to = "state") %>% 
    select(-name) %>% 
    filter(state %in% input$stab2_states) %>%
    left_join(ini_modecolors %>% mutate(dms_mode = as.character(dms_mode))) %>%
    rename(source2 = mode_group) %>%
    rename(source3 = Grouped_sctg2) %>%
    group_by(source2, source3) %>% summarise(value = sum(value))
  
  source_1t2_full <- left_join(source_1t2, link_1t2) %>% filter(!is.na(value)) |> filter(!duplicated(paste0(source1,source2)))
  source_2t3_full <- left_join(source_2t3, link_2t3) %>% filter(!is.na(value)) |> filter(!duplicated(paste0(source2,source3)))
  
  #source_1t2_full$value[is.na(source_1t2_full$value)] <- 0
  #source_2t3_full$value[is.na(source_2t3_full$value)] <- 0
  #source_3t4_full$value[is.na(source_3t4_full$value)] <- 0
  
  snkey <- plot_ly(type = "sankey",
                   
                   #domain = list(
                   #  x =  c(0,1),
                   #  y =  c(0,1)
                   #),
                   
                   #orientation = "h",
                   #valueformat = ".0f",
                   #valuesuffix = "TWh",
                   
                   node = list(
                     label = blabs,
                     #data$data[[1]]$node$color,
                     pad = 15,
                     thickness = 15,
                     line = list(
                       color = "black",
                       width = 0.5
                     )
                   ),
                   
                   link = list(
                     source = c(source_1t2_full$index1-1, source_2t3_full$index2-1),
                     target = c(source_1t2_full$index2-1, source_2t3_full$index3-1),
                     value = c(source_1t2_full$value, source_2t3_full$value),
                     customdata = c(paste0('from: ', source_1t2_full$source1, '<br>to: ', source_1t2_full$source2),
                                    paste0('from: ', source_2t3_full$source2, '<br>to: ', source_2t3_full$source3)),
                    
                     #label = paste("Tonnage: ", source_1t2_full$value, "\n")
                     hovertemplate = paste0('Tonnage: %{value:.1f}<br>',
                                            '%{customdata}<br><extra></extra>')
                   )
  ) %>% config(displayModeBar = FALSE)
  
  #browser()
  # Add annotations for labels
  # snkey <- snkey %>%
  #   add_annotations(
  #     x = c(0.1, 0.9),
  #     y = c(0.5, 0.5),
  #     text = c(paste0("Tonnage: ", source_1t2_full$value),
  #              paste0("Tonnage: ", sum(source_2t3_full$value))),
  #     showarrow = FALSE,
  #     font = list(size = 14),
  #     xref = "paper",
  #     yref = "paper"
  #   )
  
  
  return(snkey)
}
#dataframe reactive----------------

stop_check_page <- reactiveVal(NA)

stab2_data <- eventReactive(input$stab2_mainbutt,{
  print("RUNNING: stab2_data_creation")
  #browser()
  if(input$stab2_states == '99'){
    return<-dat_ss %>% 
      filter(origin != '99'& destination != '99') %>%
      mutate(origin = ifelse(origin %in% c("05", "12","13","21","22","28","29","45","48","51"),'99',origin),
             destination = ifelse(destination %in% c("05", "12","13","21","22","28","29","45","48","51"), '99',destination))
    
  } else {
    return<-dat_ss %>%
      filter(origin != '99'& destination != '99')
    
  }
  # return<-dat_ss %>% 
  # filter(origin != '99'& destination != '99') %>%
  # mutate(origin = ifelse(origin %in% c("05", "12","13","21","22","28","29","45","48","51"),'99',origin),
  #        destination = ifelse(destination %in% c("05", "12","13","21","22","28","29","45","48","51"), '99',destination)) %>%
  #select(-c(tons_2017, value_2017)) %>%
  #state filter
  #mutate(state = ifelse(nchar(origin)==5, str_sub(origin,1,2), str_sub(destination,1,2))) %>% 
  return <- return %>%
    filter(origin %in% input$stab2_states|destination %in% input$stab2_states) %>% 
    
    #inbound, outbound, within ITTS
    #mutate(direction = ifelse(origin %in% c("05", "12","13","21","22","28","29","45","48","51","99") &
    #                            destination %in% c("05", "12","13","21","22","28","29","45","48","51","99"), "Within ITTS",
    #                          ifelse(origin %in% c("05", "12","13","21","22","28","29","45","48","51","99") &
    #                                   !(destination %in% c("05", "12","13","21","22","28","29","45","48","51","99")),"Outbound","Inbound"))) %>%
    mutate(direction = ifelse(origin %in% input$stab2_states & destination %in%input$stab2_states, "Within",
                              ifelse(origin %in% input$stab2_states & !(destination %in% input$stab2_states), "Outbound", 
                                     ifelse(!(origin %in% input$stab2_states) & destination %in% input$stab2_states, "Inbound", "non-valid direction"))))|>
    filter(direction %in% input$stab2_OD) %>%
    #filter the simple ones
    filter(Grouped_sctg2 %in% input$stab2_commodity) %>% 
    filter(dms_mode %in% input$stab2_mode)
    #browser()
#add scenarios
#curr = c("05", "12","13","21","22","28","29","45","48","51","01","47","37","99")
return <- process_scenario_v3(dat_temp_cs = return, #the filtered datatable
                    Scenario_opt_cs = input$stab2_comps, #scenario selection
                    curr = input$stab2_states #, #select which are the basis of imports and exports
                    #all_flag = 1 #not sure which one
                    )

return <- return %>% 
  rename(tons_2022_s0 = tons_2022,
         #tons_2022_s0 = tons_2022,
         tons_2050_s0 = tons_2050,
         #value_2022_s0 = value_2022,
         value_2022_s0 = value_2022,
         value_2050_s0 = value_2050)

  return(return)
  }) #%>% 
  #bindCache(input$stab2_states, input$stab2_OD, input$stab2_commodity, input$stab2_mode)

observeEvent(input$stab2_mainbutt, {
  
  onFlushed(function() {
    flush_waiter_message()
  })
  
  #browser()
  #show_waiter_message()
  
  #Warning list ------
  warning <- c()
  if(is.null(input$stab2_comps)){
    warning <- c(warning, "You haven't selected any scenario(s)")
  }
  
  if(is.null(input$stab2_states)){
    warning <- c(warning, "You haven't selected any state(s)")
  }
  
  if(is.null(input$stab2_OD)){
    warning <- c(warning, "You haven't selected any trade direction(s)")
  }
  
  if(is.null(input$stab2_mode)){
    warning <- c(warning, "You haven't selected any mode(s)")
  }
  
  if(is.null(input$stab2_mode)){
    warning <- c(warning, "You haven't selected any mode(s)")
  }
  
  if(is.null(input$stab2_commodity)){
    warning <- c(warning, "You haven't selected any commodities")
  }
  
  if(length(warning) == 1){
    showNotification(HTML(warning), type = "warning")
  }
  
  if(length(warning) > 1){
    showNotification(HTML(paste0(warning, sep = ' and <br/>')), type = "warning")
  }
  
  #Render UI ----
  show_waiter_message()
  if(input$stab2_mainbutt >= 1 & length(warning) == 0 & is.na(stop_check_page())){
    stop_check_page("yes")
    output$output_panel_1 <-  renderUI({
      #first card ----
      argonCard(width = 12, 
                argonRow(width = 12,h1("Growth year over year", align = 'center')),
                argonRow(width = 12, p("The total tonnage and value for each chosen scenario for the base and future years.")),
                argonRow(width = 12, 
                         argonColumn(width = 10, p(textOutput("scen_select"))),
                         argonColumn(width = 10, p(textOutput("state_select"))),
                         argonColumn(width = 10, p(textOutput("dir_select"))),
                         argonColumn(width = 10, p(textOutput("mode_select"))),
                         argonColumn(width = 10, p(textOutput("comm_select")))
                ),
                argonRow(width = 12,
                         argonColumn(width = 6,plotlyOutput("stab2_line_tons", width = "auto", height = "auto")),
                         argonColumn(width = 6,plotlyOutput("stab2_line_value", width = "auto", height = "auto"))
                )
      )
    })
    #second card ----
    output$output_panel_2 <-  renderUI({
      
      argonCard(width = 12,
                argonRow(width = 12,h1("Percent Growth")),
                argonRow(width = 12, p("This section compares the percent growth for key measures from the base (2022) and future year (2050) by tonnage on the right and value on the left.
                                           The first row of graph compares the growth for each mode and the second each commodity selected above. Each dot represents a scenario with a higher growth for that scenario the further right the dot is on the line.")),
                argonRow(width = 12, 
                         argonColumn(width = 10, p(textOutput("scen_select_pw"))),
                         argonColumn(width = 10, p(textOutput("state_select_pw"))),
                         argonColumn(width = 10, p(textOutput("dir_select_pw"))),
                         argonColumn(width = 10, p(textOutput("mode_select_pw"))),
                         argonColumn(width = 10, p(textOutput("comm_select_pw")))
                ),
                argonRow(width = 12,h2("Percent Growth For Selected State")),
                argonRow(width = 12,
                         argonColumn(width = 6,plotlyOutput("stab2_tons_state_growth_dotplot", width = "auto", height = "100%")),
                         argonColumn(width = 6,plotlyOutput("stab2_value_state_growth_dotplot", width = "auto", height = "auto"))
                ),
                argonRow(width = 12,h2("Percent Growth by Mode", align = 'center')),
                argonRow(width = 12,
                         argonColumn(width = 6,plotlyOutput("stab2_tons_mode_growth_dotplot", width = "auto", height = "auto")),
                         argonColumn(width = 6,plotlyOutput("stab2_value_mode_growth_dotplot", width = "auto", height = "auto"))
                ),
                argonRow(width = 12,h2("Percent Growth by Commodity", align = "center")),
                argonRow(width = 12,
                         argonColumn(width = 12,plotlyOutput("stab2_tons_com_growth_dotplot", width = "auto", height = "auto"))
                ),
                argonRow(width = 12, 
                         argonColumn(width = 12,plotlyOutput("stab2_value_com_growth_dotplot", width = "auto", height = "auto"))
                )
      )
    }) #end of card
    #third card ----
    output$output_panel_3 <-  renderUI({
      
      argonCard(width = 12, 
                argonRow(width = 12, h1("Trends by year")),
                argonRow(width = 12, p("This section shows the total amount of tonnage (left) and value (right) for different measures (mode, direction, and commodity) for the selected year, see below.")),
                argonRow(width = 12, 
                         argonColumn(width = 10, p(textOutput("scen_select_ty"))),
                         argonColumn(width = 10, p(textOutput("state_select_ty"))),
                         argonColumn(width = 10, p(textOutput("dir_select_ty"))),
                         argonColumn(width = 10, p(textOutput("mode_select_ty"))),
                         argonColumn(width = 10, p(textOutput("comm_select_ty")))
                ),
                argonRow(width = 12, 
                         argonColumn(width = 2,
                                     tags$div(
                                       title = "Select a measurement of freight movement to display on the map",
                                       selectInput(inputId = "stab2_value_opts", 
                                                   label = "Freight Measure", 
                                                   choices = c("Tons 2022" = "tons_2022",
                                                               "Tons 2050" = "tons_2050",
                                                               "Value 2022" = "value_2022",
                                                               "Value 2050" = "value_2050"),
                                                   selected ='tons_2050'))
                         )
                ),
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
                ))
    })
    #fourth card ----
    output$output_panel_4 <-  renderUI({
      
      argonCard(width = 12,
                argonRow(width = 12, h1("Flow Diagram")),
                argonRow(width = 12, p("This section shows the overall flow of tonnage for the selected scenario. Flow lines represent the amount of tonnage moving from one category to another.")),
                argonRow(width = 12,
                         selectInput(
                           inputId= "stab2_sankey_filt",
                           label = "Scenario(s)",
                           choices = c("Baseline" = "s0",
                                       "Scenario 1: Respond to Heightened Supply Chain Risks"= "s1",
                                       "Scenario 2: Leverage Multi-State Strength"= "s2",
                                       "Scenario 3: Embrace Technology Transformations"= "s3")),
                         argonColumn(width = 12, 
                                     h2("Commodity Flow Breakdown", align = "center"), 
                                     plotlyOutput("stab2_sankey", width = "auto", height = "auto")
                         )
                )
      )
    })
    
  }
  
  req(stop_check_page())
  print("RUNNING SCEN_COMP: selection text")
  #outputs ----
  # dynamic text summarize users selection
  output$scen_select_ty <- output$scen_select_pw <- output$scen_select <- renderText({
    if (length(input$stab2_comps) == 0) {
      return("No scenarios selected.")
    }
    
    if (length(input$stab2_comps) == 1) {
      return(paste0("Scenario selected: ", names(scenario_choices)[scenario_choices %in% input$stab2_comps]))
    } else {
      return(paste0("Scenarios selected: ", paste(names(scenario_choices)[scenario_choices %in% input$stab2_comps], collapse = ", ")))
    }
  })
  
  output$state_select_ty <- output$state_select_pw <- output$state_select <- renderText({
    if (length(input$stab2_states) == 0) {
      return("No state selected.")
    } else if (length(input$stab2_states) == 1) {
      return(paste0("State selected: ",  state_join$state_lab[state_join$state %in% input$stab2_states]))
    } else if (length(input$stab2_states) > 1) {
      return(paste0("States selected: ", paste(state_join$state_lab[state_join$state %in% input$stab2_states], collapse = "; ")))
    }
  })
  
  output$dir_select_ty <- output$dir_select_pw <- output$dir_select <- renderText({
    if (length(input$stab2_OD) == 0) {
      return("No mode selected.")
    } else if(length(input$stab2_OD) == 1){
      return(paste0("Direction selected: ", input$stab2_OD))
    } else if (length(input$stab2_OD) > 1){
      return(paste0("Directions selected: ", paste(input$stab2_OD, collapse = "; ")))
    }
  })
  
  output$mode_select_ty <- output$mode_select_pw <- output$mode_select <- renderText({
    if (length(input$stab2_mode) == 0) {
      return("No mode selected.")
    } else if(length(input$stab2_mode) == 1){
      return(paste0("Mode selected: ", names(modes)[modes %in% input$stab2_mode]))
    } else if (length(input$stab2_mode) > 1){
      return(paste0("Modes selected: ", paste(names(modes)[modes %in% input$stab2_mode], collapse = "; ")))
    }
  })
  
  output$comm_select_ty <- output$comm_select_pw <- output$comm_select <- renderText({
    if(length(input$stab2_commodity) == 1){
      return(paste0("Commodity selected: ", input$stab2_commodity))
    } else if (length(input$stab2_commodity) > 1){
      return(paste("Commodities selected: ", paste(input$stab2_commodity, collapse = ", ")))
    }
  })
  #dont close-------
  if(nrow(stab2_data())>0){
    #lineplots ----
    output$stab2_line_tons <- renderPlotly({
  req(stab2_data())
  #browser()
  print("RUNNING SCEN_COMP: tonnage barplot")
  
    df_temp <- stab2_data() %>%
      # mutate(tons_2022_s1 = tons_2022_s0,
      #        tons_2022_s2 = tons_2022_s0,
      #        tons_2022_s3 = tons_2022_s0,
      #        value_2022_s1 = value_2022_s0,
      #        value_2022_s2 = value_2022_s0,
      #        value_2022_s3 = value_2022_s0) %>%
      select(matches(paste(input$stab2_comps, collapse="|")), c(origin, destination, dms_mode, Grouped_sctg2, direction)) %>%
      pivot_longer(cols = ends_with(c("_s0","_s1","_s2","_s3")),
                   names_sep = "_",
                   names_to = c("measure","year", "scenario"),
                   values_to = "value") %>% 
      filter(measure == "tons") %>%
      #filter(year != 2022) %>%
      group_by(scenario, year) %>%
      summarise(value = sum(value,na.rm = T)) %>% ungroup()
    
    bar_plot_yearaxis(df_temp, meas = "Tonnage")
  })
    
    output$stab2_line_value <- renderPlotly({
  req(stab2_data())
  print("RUNNING SCEN_COMP: value lineplot")
  
    df_temp <- stab2_data() %>%
      select(matches(paste(input$stab2_comps, collapse="|")), c(origin, destination, dms_mode, Grouped_sctg2, direction)) %>%
      pivot_longer(cols = ends_with(c("_s0","_s1","_s2","_s3")),
                 names_sep = "_",
                 names_to = c("measure","year", "scenario"),
                 values_to = "value") %>%
    filter(measure == "value") %>%
    filter(year != 2017) %>%
    group_by(scenario, year) %>%
    summarise(value = sum(value))  %>% ungroup()
    
    bar_plot_yearaxis(df_temp, meas = "Value USD")
  })

#dot plots----------


  output$stab2_tons_state_growth_dotplot <- renderPlotly({
    req(stab2_data())
    print("RUNNING SCEN_COMP: tonnage state dot")
    #browser()
    df_temp <- stab2_data() %>%
      mutate(tons_2022_s1 = tons_2022_s0,
             tons_2022_s2 = tons_2022_s0,
             tons_2022_s3 = tons_2022_s0,
             value_2022_s1 = value_2022_s0,
             value_2022_s2 = value_2022_s0,
             value_2022_s3 = value_2022_s0)  %>%
      select(matches(paste(input$stab2_comps, collapse="|")), c(origin, destination, dms_mode, Grouped_sctg2, direction)) %>%
      pivot_longer(cols = c(origin, destination), values_to = "state") %>% 
      select(-name) %>% 
      filter(state %in% input$stab2_states) %>%
      #filter(nchar(origin) == 5) %>%
      #mutate(state = str_sub(origin,1,2)) %>%
      group_by(state) %>% 
      summarise_if(is.numeric, sum) %>%
      pivot_longer(cols = ends_with(c("_s0","_s1","_s2","_s3")),
      names_sep = "_",
      names_to = c("measure","year", "scenario"),
      values_to = "value") %>%
      pivot_wider(id_cols = c(state,measure,scenario), names_from = year, names_prefix = "y", values_from = value) %>%
      group_by(state, measure, scenario) %>% 
      summarise(value = (sum(y2050) - sum(y2022))/sum(y2022)) %>%
      filter(measure == "tons") %>%
      left_join(state_join) %>%
      rename(label = state_lab)

    dot_plot(df_temp) 
    })

  output$stab2_value_state_growth_dotplot <- renderPlotly({
    req(stab2_data())
    print("RUNNING SCEN_COMP: tonnage value state dot")
    
    df_temp <- stab2_data() %>%
      mutate(tons_2022_s1 = tons_2022_s0,
             tons_2022_s2 = tons_2022_s0,
             tons_2022_s3 = tons_2022_s0,
             value_2022_s1 = value_2022_s0,
             value_2022_s2 = value_2022_s0,
             value_2022_s3 = value_2022_s0)  %>%
      select(matches(paste(input$stab2_comps, collapse="|")), c(origin, destination, dms_mode, Grouped_sctg2, direction)) %>%
      pivot_longer(cols = c(origin, destination), values_to = "state") %>% 
      select(-name) %>% 
      filter(state %in% input$stab2_states) %>%
      #filter(nchar(origin) == 5) %>%
      #mutate(state = str_sub(origin,1,2)) %>%
      #filter(nchar(origin) == 5) %>%
      #mutate(state = str_sub(origin,1,2)) %>%
      group_by(state) %>%
      summarise_if(is.numeric, sum) %>%
      pivot_longer(cols = ends_with(c("_s0","_s1","_s2","_s3")),
      names_sep = "_",
      names_to = c("measure","year", "scenario"),
      values_to = "value") %>%
      pivot_wider(id_cols = c(state,measure,scenario), names_from = year, names_prefix = "y", values_from = value) %>%
      group_by(state, measure, scenario) %>%
      summarise(value = (sum(y2050) - sum(y2022))/sum(y2022)) %>%
      filter(measure == "value") %>%
      left_join(state_join)%>%
      rename(label = state_lab)
    dot_plot(df_temp, meas = "Value USD")
    
    })

  output$stab2_tons_mode_growth_dotplot <- renderPlotly({
    req(stab2_data())
    print("RUNNING SCEN_COMP: tonnage mode dot")
    
    df_temp <- stab2_data() %>%
      mutate(tons_2022_s1 = tons_2022_s0,
             tons_2022_s2 = tons_2022_s0,
             tons_2022_s3 = tons_2022_s0,
             value_2022_s1 = value_2022_s0,
             value_2022_s2 = value_2022_s0,
             value_2022_s3 = value_2022_s0)  %>%
      select(matches(paste(input$stab2_comps, collapse="|")), c(origin, destination, dms_mode, Grouped_sctg2, direction)) %>%
      #filter(nchar(origin) == 5) %>%
      #mutate(state = str_sub(origin,1,2)) %>%
      group_by(dms_mode) %>%
      summarise_if(is.numeric, sum) %>%
      pivot_longer(cols = ends_with(c("_s0","_s1","_s2","_s3")),
      names_sep = "_",
      names_to = c("measure","year", "scenario"),
      values_to = "value") %>%
      pivot_wider(id_cols = c(dms_mode,measure,scenario), names_from = year, names_prefix = "y", values_from = value) %>%
      group_by(dms_mode, measure, scenario) %>%
      summarise(value = (sum(y2050) - sum(y2022))/sum(y2022)) %>%
      filter(measure == "tons") %>%
      left_join(ini_modecolors %>% mutate(dms_mode = as.character(dms_mode))) %>%
      rename(label = mode_group)
    
    dot_plot(df_temp) 
    
    })

  output$stab2_value_mode_growth_dotplot <- renderPlotly({
    req(stab2_data())
    print("RUNNING SCEN_COMP: tonnage value dot")
    
    df_temp <- stab2_data() %>%
      mutate(tons_2022_s1 = tons_2022_s0,
             tons_2022_s2 = tons_2022_s0,
             tons_2022_s3 = tons_2022_s0,
             value_2022_s1 = value_2022_s0,
             value_2022_s2 = value_2022_s0,
             value_2022_s3 = value_2022_s0)  %>%
      select(matches(paste(input$stab2_comps, collapse="|")), c(origin, destination, dms_mode, Grouped_sctg2, direction)) %>%
      #filter(nchar(origin) == 5) %>%
      #mutate(state = str_sub(origin,1,2)) %>%
      group_by(dms_mode) %>%
      summarise_if(is.numeric, sum) %>%
      pivot_longer(cols = ends_with(c("_s0","_s1","_s2","_s3")),
      names_sep = "_",
      names_to = c("measure","year", "scenario"),
      values_to = "value") %>%
      pivot_wider(id_cols = c(dms_mode,measure,scenario), names_from = year, names_prefix = "y", values_from = value) %>%
      group_by(dms_mode, measure, scenario) %>%
      summarise(value = (sum(y2050) - sum(y2022))/sum(y2022)) %>%
      filter(measure == "value") %>%
      left_join(ini_modecolors %>% mutate(dms_mode = as.character(dms_mode))) %>%
      rename(label = mode_group)
    
    dot_plot(df_temp, meas = "Value USD") 
    
    })
  
  output$stab2_tons_com_growth_dotplot <- renderPlotly({
    req(stab2_data())
    print("RUNNING SCEN_COMP: tonnage com dot")
    #browser()
    df_temp <- stab2_data() %>% 
      mutate(tons_2022_s1 = tons_2022_s0,
             tons_2022_s2 = tons_2022_s0,
             tons_2022_s3 = tons_2022_s0,
             value_2022_s1 = value_2022_s0,
             value_2022_s2 = value_2022_s0,
             value_2022_s3 = value_2022_s0)  %>%
      select(matches(paste(input$stab2_comps, collapse="|")), c(origin, destination, dms_mode, Grouped_sctg2, direction)) %>%
      #filter(nchar(origin) == 5) %>%
      #mutate(state = str_sub(origin,1,2)) %>%
      group_by(Grouped_sctg2) %>%
      summarise_if(is.numeric, sum) %>%
      pivot_longer(cols = ends_with(c("_s0","_s1","_s2","_s3")),
      names_sep = "_",
      names_to = c("measure","year", "scenario"),
      values_to = "value") %>%
      pivot_wider(id_cols = c(Grouped_sctg2,measure,scenario), names_from = year, names_prefix = "y", values_from = value) %>%
      group_by(Grouped_sctg2, measure, scenario) %>%
      summarise(value = (sum(y2050) - sum(y2022))/sum(y2022)) %>%
      filter(measure == "tons") %>%
      rename(label = Grouped_sctg2)
    
    dot_plot(df_temp) 
    
    })

  output$stab2_value_com_growth_dotplot <- renderPlotly({
    req(stab2_data())
    print("RUNNING SCEN_COMP: value com dot")
    
    #browser()
    df_temp <- stab2_data() %>%
      mutate(tons_2022_s1 = tons_2022_s0,
             tons_2022_s2 = tons_2022_s0,
             tons_2022_s3 = tons_2022_s0,
             value_2022_s1 = value_2022_s0,
             value_2022_s2 = value_2022_s0,
             value_2022_s3 = value_2022_s0)  %>%
      select(matches(paste(input$stab2_comps, collapse="|")), c(origin, destination, dms_mode, Grouped_sctg2, direction)) %>%
      #filter(nchar(origin) == 5) %>%
      #mutate(state = str_sub(origin,1,2)) %>%
      group_by(Grouped_sctg2) %>%
      summarise_if(is.numeric, sum) %>%
      pivot_longer(cols = ends_with(c("_s0","_s1","_s2","_s3")),
      names_sep = "_",
      names_to = c("measure","year", "scenario"),
      values_to = "value") %>%
      pivot_wider(id_cols = c(Grouped_sctg2,measure,scenario), names_from = year, names_prefix = "y", values_from = value) %>%
      group_by(Grouped_sctg2, measure, scenario) %>%
      summarise(value = (sum(y2050) - sum(y2022))/sum(y2022)) %>%
      filter(measure == "value") %>%
      rename(label = Grouped_sctg2)
    #df$label = sapply(df$label, FUN = function(x){paste(strwrap(x, width = 16))})
    
    dot_plot(df_temp, meas = "Value USD") 
    })

#bar plots -----------


  output$stab2_mode_bar <- renderPlotly({
    req(stab2_data())
    print("RUNNING SCEN_COMP: mode bar")
    
    
    dir_temp <- stab2_data() %>% 
      filter(origin == '99000'|destination == '99000'|origin %in% input$stab2_states|destination %in% input$stab2_states) %>% 
      pivot_longer(cols = ends_with(c("_s0","_s1","_s2","_s3")),
                   names_sep = "_",
                   names_to = c("measure","year", "scenario"),
                   values_to = "value") %>% 
      left_join(ini_modecolors %>% mutate(dms_mode = as.character(dms_mode))) %>%
      filter(measure == stringr::str_split(input$stab2_value_opts, "_")[[1]][1]) %>%
      filter(year == stringr::str_split(input$stab2_value_opts, "_")[[1]][2]) %>%
      group_by(mode_group, scenario) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>% left_join(scen_colors) %>%
      rename(group = mode_group)
    

    bar_plot_singleyear(dir_temp, #county = input$stab1_county_opts,
                        measure = input$stab2_value_opts,
                        sourceName = "mode_bar_plot")

  })

  output$stab2_dir_bar <- renderPlotly({
    req(stab2_data())
    print("RUNNING SCEN_COMP: direction bar")
    
    
    dir_temp <- stab2_data() %>%
      pivot_longer(cols = ends_with(c("_s0","_s1","_s2","_s3")),
                   names_sep = "_",
                   names_to = c("measure","year", "scenario"),
                   values_to = "value") %>%
      filter(measure == stringr::str_split(input$stab2_value_opts, "_")[[1]][1]) %>%
      filter(year == stringr::str_split(input$stab2_value_opts, "_")[[1]][2])
    
    dir_temp<-dir_temp %>%
      group_by(direction, scenario) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>% left_join(scen_colors) %>%
      rename(group = direction)
    

    bar_plot_singleyear(dir_temp, #county = input$stab1_county_opts,
                        measure = input$stab2_value_opts,
                        sourceName = "mode_bar_plot")
})

  output$stab2_com_bar <- renderPlotly({
    req(stab2_data())
    print("RUNNING SCEN_COMP: com bar")
    
    dir_temp <- stab2_data() %>%
      pivot_longer(cols = ends_with(c("_s0","_s1","_s2","_s3")),
                   names_sep = "_",
                   names_to = c("measure","year", "scenario"),
                   values_to = "value") %>%
      filter(measure == stringr::str_split(input$stab2_value_opts, "_")[[1]][1]) %>%
      filter(year == stringr::str_split(input$stab2_value_opts, "_")[[1]][2])
    
    dir_temp<-dir_temp %>%
      group_by(Grouped_sctg2, scenario) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>% 
      rename(group = Grouped_sctg2)
    
    
    bar_plot_singleyear(dir_temp, #county = input$stab1_county_opts,
                        measure = input$stab2_value_opts,
                        sourceName = "mode_bar_plot")
    
  })
  

#saney diagram-----------



  output$stab2_sankey <- renderPlotly({
    req(stab2_data())
    print("RUNNING SCEN_COMP: sankey")
    
    dat_temp<-stab2_data() %>%
      pivot_longer(cols = ends_with(c("_s0","_s1","_s2","_s3")),
                   names_sep = "_",
                   names_to = c("measure","year", "scenario"),
                   values_to = "value") %>%
      filter(measure == stringr::str_split(input$stab2_value_opts, "_")[[1]][1]) %>%
      filter(year == stringr::str_split(input$stab2_value_opts, "_")[[1]][2]) %>%
      filter(scenario == input$stab2_sankey_filt)
    
    sankey_diagram(dat_temp)
  })
  

  #waiter_hide() # hide the waiter  
  } else {showNotification("Your filter selection does not include any freight flows.", type = "warning")}
  
 
  
  })


#old lineplots
# output$stab2_line_tons <- renderPlotly({
#   req(stab2_data())
#   
#   print("RUNNING SCEN_COMP: tonnage lineplot")
#   
#   df_temp <- stab2_data() %>%
#     mutate(tons_2022_s1 = tons_2022_s0,
#            tons_2022_s2 = tons_2022_s0,
#            tons_2022_s3 = tons_2022_s0,
#            value_2022_s1 = value_2022_s0,
#            value_2022_s2 = value_2022_s0,
#            value_2022_s3 = value_2022_s0) %>%
#     select(matches(paste(input$stab2_comps, collapse="|")), c(origin, destination, dms_mode, Grouped_sctg2, direction)) %>%
#     pivot_longer(cols = ends_with(c("_s0","_s1","_s2","_s3")),
#                  names_sep = "_",
#                  names_to = c("measure","year", "scenario"),
#                  values_to = "value") %>% 
#     filter(measure == "tons") %>%
#     filter(year != 2017) %>%
#     group_by(scenario, year) %>%
#     summarise(value = sum(value,na.rm = T)) %>% ungroup()
#   
#   line_plot(df_temp, meas = "Tonnage")
# })
# 
# output$stab2_line_value <- renderPlotly({
#   req(stab2_data())
#   print("RUNNING SCEN_COMP: value lineplot")
#   
#   df_temp <- stab2_data() %>%
#     mutate(tons_2022_s1 = tons_2022_s0,
#            tons_2022_s2 = tons_2022_s0,
#            tons_2022_s3 = tons_2022_s0,
#            value_2022_s1 = value_2022_s0,
#            value_2022_s2 = value_2022_s0,
#            value_2022_s3 = value_2022_s0) %>%
#     select(matches(paste(input$stab2_comps, collapse="|")), c(origin, destination, dms_mode, Grouped_sctg2, direction)) %>%
#     pivot_longer(cols = ends_with(c("_s0","_s1","_s2","_s3")),
#                  names_sep = "_",
#                  names_to = c("measure","year", "scenario"),
#                  values_to = "value") %>%
#     filter(measure == "value") %>%
#     filter(year != 2017) %>%
#     group_by(scenario, year) %>%
#     summarise(value = sum(value))  %>% ungroup()
#   
#   line_plot(df_temp, meas = "Value USD")
# })