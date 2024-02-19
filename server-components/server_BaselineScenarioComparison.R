#load inputs#--------------------------------------
state_join <- data.frame(state = c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", 
                                   "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27",
                                   "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
                                   "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53",
                                   "54", "55", "56", "60", "66", "69","72", "78"),
                         state_lab =c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
                                        "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
                                      "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
                                      "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                      "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
                                      "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", 
                                      "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", 
                                      "American Samoa", "Guam", "Northern Mariana Islands", "Puerto Rico", "U.S. Virgin Islands"))

scen_colors <- data.frame(
  scenario = c("s0","s1","s2","s3"),
  scen_name = c("Baseline",
                "Scenario 1: Respond to Heightened Supply Chain Risks",
                "Scenario 2: Leverage Multi-State Strength",
                "Scenario 3: Embrace Technology Transformations"),
  scen_color = c("#EE4B2B","#7752FE","#495E57","#99B080"),
  lntype = c("dash","solid","solid","solid"),
  dotmrk = c("square","circle","circle","circle"))

ini_modecolors <- data.frame(
  dms_mode = c("1","2","3","4","5","6","7","99"),
  mode_group = c("Truck", "Rail", "Water", "Air (Includes truck-air)", "Mutliple Modes and Mail", "Pipeline", "Other and Unknown","Unknown"),
  color = c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#66c2a5","#E11111"))

ini_international <- data.frame(
  country_lab = c("Africa","Asia","Australia and Oceania",
              "North America",
              "South/Central America",
              "Antarctica","Europe"),
  country = c("1","2","3","4","6","7","8")
)

#dataframe reactive----------------
#withProgress(message = "Running Data Processing",{
observeEvent(input$stab2_mainbutt, {print('working')})

stab2_data <- eventReactive(input$stab2_mainbutt, 

  #   #dir_temp$direction[(nchar(dir_temp$origin) == 5 & str_sub(dir_temp$origin,1,2) %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))|(nchar(dir_temp$destination) == 5 & str_sub(dir_temp$destination,1,2) %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))] <- "Within ITTS"
  #   #dir_temp$direction[nchar(dir_temp$origin) == 5 & !(dir_temp$destination %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))] <- "Export"
  #   #dir_temp$direction[nchar(dir_temp$destination) == 5  & !(dir_temp$origin %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))] <- "Import"
  dat_cs %>% 
    #state filter
    mutate(state = ifelse(nchar(origin)==5, str_sub(origin,1,2), str_sub(destination,1,2))) %>%
    filter(state %in% input$stab2_states) %>%
    
    #inbound, outbound, within ITTS
    mutate(direction = ifelse((nchar(origin) == 5 & (str_sub(destination,1,2) %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))), "Within ITTS",
                              ifelse((nchar(destination) == 5 & (str_sub(origin,1,2) %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))), "Within ITTS",
                                     ifelse(nchar(origin) == 5 & !(str_sub(destination,1,2) %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37")),"Outbound","Inbound")))) %>%
    filter(direction %in% input$stab2_OD) %>%
    
    #filter the simple ones
    filter(Grouped_sctg2 %in% input$stab2_commodity) %>% 
    filter(dms_mode %in% input$stab2_mode) %>%
    
    #make scenarios
    mutate(s1_tons_2017 = 1.22*tons_2017,
           s1_tons_2020 = 1.663*tons_2020,
           s1_tons_2050 = 1.455*tons_2050,
           s2_tons_2017 = 1.54*tons_2017,
           s2_tons_2020 = 1.633*tons_2020,
           s2_tons_2050 = 1.72*tons_2050) %>%
    mutate(s1_value_2017 = 1.80*value_2017,
           s1_value_2020 = 1.91*value_2020,
           s1_value_2050 = 2*value_2050,
         s2_value_2017 = 2.12*value_2017,
         s2_value_2020 = 2.21*value_2020,
         s2_value_2050 = 2.32*value_2050) %>%
  mutate(s3_tons_2017 = 1.35*tons_2017,
         s3_tons_2020 = 1.54*tons_2020,
         s3_tons_2050 = 2*tons_2050,
         s3_value_2017 = 2.45*value_2017,
         s3_value_2020 = 2.56*value_2020,
         s3_value_2050 = 2.6*value_2050) %>%
    rename(s0_tons_2017 = tons_2017,
           s0_tons_2020 = tons_2020,
           s0_tons_2050 = tons_2050,
           s0_value_2017 = value_2017,
           s0_value_2020 = value_2020,
           s0_value_2050 = value_2050) %>%
    
    #filter scenarios
    select(matches(paste(input$stab2_comps, collapse="|")), c(origin, destination, dms_mode, Grouped_sctg2, state, direction)) 
  )
#})

#observeEvent(input$stab2_mainbutt, {browser()}) 

#lineplots --------
line_plot <- function(df_in, meas = "Tonnage"){

  df_temp <- df_in %>% mutate(year = as.numeric(year)) %>%
    left_join(scen_colors)
  
  lplot <- plot_ly(df_temp, x = ~year, y = ~value, type = 'scatter', mode = 'lines', 
                   color = ~I(scen_color),
                   linetype = ~I(lntype),
                   name = ~scen_name,
                   hovertemplate = paste0('Year: %{x}<br>', 
                                         meas, 
                                         ':%{y:.2s} <br>')) %>%
    layout(xaxis = list(title = 'Year'),
           yaxis = list(title = meas, separatethousands= TRUE)) %>%
    config(displayModeBar = FALSE)
  if(meas == "Value USD"){
    lplot<-lplot %>% layout(yaxis = list(title = meas, tickformat = "$~s"))
  }
  
  return(lplot)
  
}

observeEvent(ignoreInit = TRUE, input$stab2_mainbutt, {
  req(input$stab2_comps, input$stab2_states, input$stab2_OD, input$stab2_mode, input$stab2_commodity)
  
  # dynamic text summarize users selection
  output$scen_select <- renderText({
    if (length(input$stab2_comps) == 0) {
      return("No scenarios selected.")
    }
    
    if (length(input$stab2_comps) == 1) {
      return(paste0("Scenario selected: ", names(scenario_choices)[scenario_choices %in% input$stab2_comps]))
    } else {
      return(paste0("Scenarios selected: ", paste(names(scenario_choices)[scenario_choices %in% input$stab2_comps], collapse = ", ")))
    }
  })
  
  output$state_select <- renderText({
    if (length(input$stab2_states) == 0) {
      return("No state selected.")
    } else if (length(input$stab2_states) == 1) {
      return(paste0("State selected: ", names(state_ch)[state_ch %in% input$stab2_states]))
    } else if (length(input$stab2_states) > 1) {
      return(paste0("States selected: ", paste(names(state_ch)[state_ch %in% input$stab2_states], collapse = "; ")))
    }
  })
  
  output$dir_select <- renderText({
    if (length(input$stab2_OD) == 0) {
      return("No mode selected.")
    } else if(length(input$stab2_OD) == 1){
      return(paste0("Direction selected: ", input$stab2_OD))
    } else if (length(input$stab2_OD) > 1){
      return(paste0("Directions selected: ", paste(input$stab2_OD, collapse = "; ")))
    }
  })
  
  output$mode_select <- renderText({
    if (length(input$stab2_mode) == 0) {
      return("No mode selected.")
    } else if(length(input$stab2_mode) == 1){
      return(paste0("Mode selected: ", names(modes)[modes %in% input$stab2_mode]))
    } else if (length(input$stab2_mode) > 1){
      return(paste0("Modes selected: ", paste(names(modes)[modes %in% input$stab2_mode], collapse = "; ")))
    }
  })
  
  output$comm_select <- renderText({
    if(length(input$stab2_commodity) == 1){
      return(paste0("Commodity selected: ", input$stab2_commodity))
    } else if (length(input$stab2_commodity) > 1){
      return(paste("Commodities selected: ", paste(input$stab2_commodity, collapse = ", ")))
    }
  })
})



observeEvent(ignoreInit = TRUE, input$stab2_mainbutt,{
  df_temp <- stab2_data() %>%
    pivot_longer(cols = starts_with(c("s1_tons","s1_value",
                                      "s2_tons","s2_value",
                                      "s3_tons","s3_value",
                                      "s0_tons","s0_value")),
                 names_sep = "_",
                 names_to = c("scenario","measure", "year"),
                 values_to = "value") %>%
    filter(measure == "tons") %>%
    group_by(scenario, year) %>%
    summarise(value = sum(value)) %>% ungroup()
  
    
  output$stab2_line_tons <- renderPlotly({
    line_plot(df_temp, meas = "Tonnage")
  })
})


observeEvent(ignoreInit = TRUE, input$stab2_mainbutt,{
  df_temp <- stab2_data() %>%
    pivot_longer(cols = starts_with(c("s1_tons","s1_value",
                                      "s2_tons","s2_value",
                                      "s3_tons","s3_value",
                                      "s0_tons","s0_value")),
                 names_sep = "_",
                 names_to = c("scenario","measure", "year"),
                 values_to = "value") %>%
    filter(measure == "value") %>%
    group_by(scenario, year) %>%
    summarise(value = sum(value))  %>% ungroup()
  
  
  output$stab2_line_value <- renderPlotly({
    line_plot(df_temp, meas = "Value USD")
  })
})

#dot plots----------
dot_plot <- function(df_in, meas = "Tonnage"){
  #names <- unique(df_in$state_lab)
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
    layout(xaxis = list(title = 'Percent Growth',
                        tickformat = '0%'),
           yaxis = list(title = '')) %>%
    config(displayModeBar = FALSE)
  #dplot
  return(dplot)
}

observeEvent(ignoreInit = TRUE, input$stab2_mainbutt, {
  print('dp1')
  df_temp <- stab2_data() %>%
    #filter(nchar(origin) == 5) %>%
    #mutate(state = str_sub(origin,1,2)) %>%
    group_by(state) %>%
    summarise_if(is.numeric, sum) %>%
    pivot_longer(cols = starts_with(c("s1_tons","s1_value",
                                      "s2_tons","s2_value",
                                      "s3_tons","s3_value",
                                      "s0_tons","s0_value"
    )),
    names_sep = "_",
    names_to = c("scenario","measure", "year"),
    values_to = "value") %>%
    pivot_wider(id_cols = c(state,measure,scenario), names_from = year, names_prefix = "y", values_from = value) %>% 
    group_by(state, measure, scenario) %>%
    dplyr::summarise(value = (sum(y2050) - sum(y2017))/sum(y2017)) %>%
    filter(measure == "tons") %>%
    left_join(state_join) %>%
    rename(label = state_lab)
  
  output$stab2_tons_state_growth_dotplot <- renderPlotly({ dot_plot(df_temp) })
})

observeEvent(ignoreInit = TRUE, input$stab2_mainbutt, {
  print('dp2')
  df_temp <- stab2_data() %>%
    #filter(nchar(origin) == 5) %>%
    #mutate(state = str_sub(origin,1,2)) %>%
    group_by(state) %>%
    summarise_if(is.numeric, sum) %>%
    pivot_longer(cols = starts_with(c("s1_tons","s1_value",
                                      "s2_tons","s2_value",
                                      "s3_tons","s3_value",
                                      "s0_tons","s0_value"
    )),
    names_sep = "_",
    names_to = c("scenario","measure", "year"),
    values_to = "value") %>%
    pivot_wider(id_cols = c(state,measure,scenario), names_from = year, names_prefix = "y", values_from = value) %>%
    group_by(state, measure, scenario) %>%
    dplyr::summarise(value = (sum(y2050) - sum(y2017))/sum(y2017)) %>%
    filter(measure == "value") %>%
    left_join(state_join)%>%
    rename(label = state_lab)
  
  output$stab2_value_state_growth_dotplot <- renderPlotly({ dot_plot(df_temp,meas = "Value USD")})
})

observeEvent(ignoreInit = TRUE, input$stab2_mainbutt, {
  print('dp3')
  df_temp <- stab2_data() %>%
    #filter(nchar(origin) == 5) %>%
    #mutate(state = str_sub(origin,1,2)) %>%
    group_by(dms_mode) %>%
    summarise_if(is.numeric, sum) %>%
    pivot_longer(cols = starts_with(c("s1_tons","s1_value",
                                      "s2_tons","s2_value",
                                      "s3_tons","s3_value",
                                      "s0_tons","s0_value"
    )),
    names_sep = "_",
    names_to = c("scenario","measure", "year"),
    values_to = "value") %>%
    pivot_wider(id_cols = c(dms_mode,measure,scenario), names_from = year, names_prefix = "y", values_from = value) %>%
    group_by(dms_mode, measure, scenario) %>%
    dplyr::summarise(value = (sum(y2050) - sum(y2017))/sum(y2017)) %>%
    filter(measure == "tons") %>%
    left_join(ini_modecolors) %>%
    rename(label = mode_group)
  
  output$stab2_tons_mode_growth_dotplot <- renderPlotly({ dot_plot(df_temp) })
})

observeEvent(ignoreInit = TRUE, input$stab2_mainbutt, {
  print('dp4')
  df_temp <- stab2_data() %>%
    #filter(nchar(origin) == 5) %>%
    #mutate(state = str_sub(origin,1,2)) %>%
    group_by(dms_mode) %>%
    summarise_if(is.numeric, sum) %>%
    pivot_longer(cols = starts_with(c("s1_tons","s1_value",
                                      "s2_tons","s2_value",
                                      "s3_tons","s3_value",
                                      "s0_tons","s0_value"
    )),
    names_sep = "_",
    names_to = c("scenario","measure", "year"),
    values_to = "value") %>%
    pivot_wider(id_cols = c(dms_mode,measure,scenario), names_from = year, names_prefix = "y", values_from = value) %>%
    group_by(dms_mode, measure, scenario) %>%
    dplyr::summarise(value = (sum(y2050) - sum(y2017))/sum(y2017)) %>%
    filter(measure == "value") %>%
    left_join(ini_modecolors) %>%
    rename(label = mode_group)
  
  output$stab2_value_mode_growth_dotplot <- renderPlotly({ dot_plot(df_temp, meas = "Value USD") })
})

observeEvent(ignoreInit = TRUE, input$stab2_mainbutt, {
  print('dp5')
  df_temp <- stab2_data() %>%
    #filter(nchar(origin) == 5) %>%
    #mutate(state = str_sub(origin,1,2)) %>%
    group_by(Grouped_sctg2) %>%
    summarise_if(is.numeric, sum) %>%
    pivot_longer(cols = starts_with(c("s1_tons","s1_value",
                                      "s2_tons","s2_value",
                                      "s3_tons","s3_value",
                                      "s0_tons","s0_value"
    )),
    names_sep = "_",
    names_to = c("scenario","measure", "year"),
    values_to = "value") %>%
    pivot_wider(id_cols = c(Grouped_sctg2,measure,scenario), names_from = year, names_prefix = "y", values_from = value) %>%
    group_by(Grouped_sctg2, measure, scenario) %>%
    dplyr::summarise(value = (sum(y2050) - sum(y2017))/sum(y2017)) %>%
    filter(measure == "tons") %>%
    rename(label = Grouped_sctg2)
  
  output$stab2_tons_com_growth_dotplot <- renderPlotly({ dot_plot(df_temp) })
})

observeEvent(ignoreInit = TRUE, input$stab2_mainbutt, {
  print('dp6')

  df_temp <- stab2_data() %>%
    #filter(nchar(origin) == 5) %>%
    #mutate(state = str_sub(origin,1,2)) %>%
    group_by(Grouped_sctg2) %>%
    summarise_if(is.numeric, sum) %>%
    pivot_longer(cols = starts_with(c("s1_tons","s1_value",
                                      "s2_tons","s2_value",
                                      "s3_tons","s3_value",
                                      "s0_tons","s0_value"
    )),
    names_sep = "_",
    names_to = c("scenario","measure", "year"),
    values_to = "value") %>% 
    pivot_wider(id_cols = c(Grouped_sctg2,measure,scenario), names_from = year, names_prefix = "y", values_from = value) %>%
    group_by(Grouped_sctg2, measure, scenario) %>% 
    dplyr::summarise(value = (sum(y2050) - sum(y2017))/sum(y2017)) %>%
    filter(measure == "value") %>%
    rename(label = Grouped_sctg2)
  
  output$stab2_value_com_growth_dotplot <- renderPlotly({ dot_plot(df_temp, meas = "Value USD") })
})

#bar plots -----------
bar_plot_singleyear <- function(df_in, measure = 'tons_2017', sourceName = sourceName){
  meas = ifelse(stringr::str_split(measure, '_')[[1]][[1]] == 'tons','Tonnage','Value USD')
  yr = stringr::str_split(measure, '_')[[1]][[2]]
 #print(unique(df_in$scenario))
  #browser()
  df_temp <- df_in %>% left_join(scen_colors)
  
  bar_plot <- plot_ly(df_temp,
                      x = ~str_wrap(group,16),
                      y = ~value,
                      type = 'bar',
                      name = ~scen_name,
                      color = ~I(scen_color),
                      hovertemplate = ~paste0(group, "<br>",
                                             "Year: ", yr, "<br>",
                                             meas,": ", formatC(value, format = "f",digits = 2,big.mark = ","))) %>%
    layout(yaxis = list(title = meas, separatethousands= TRUE),
           xaxis = list(title = "")) %>%
    config(displayModeBar = FALSE)
  if(meas == "Value USD"){
    bar_plot<-bar_plot %>% layout(yaxis = list(title = "Value USD", tickformat = "$~s"))
  }
  return(bar_plot)
}

observeEvent(ignoreInit = TRUE, input$stab2_mainbutt,{

  dir_temp <- stab2_data() %>%
    filter(str_sub(origin,1,2) %in% input$stab2_states|str_sub(destination,1,2) %in% input$stab2_states) %>%
    pivot_longer(cols = starts_with(c("s1_tons","s1_value",
                                      "s2_tons","s2_value",
                                      "s3_tons","s3_value",
                                      "s0_tons","s0_value")),
                 names_sep = "_",
                 names_to = c("scenario","measure", "year"),
                 values_to = "value") %>% 
    left_join(ini_modecolors) %>%
    filter(measure == stringr::str_split(input$stab2_value_opts, "_")[[1]][1]) %>%
    filter(year == stringr::str_split(input$stab2_value_opts, "_")[[1]][2]) %>%
    group_by(mode_group, scenario) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>% left_join(scen_colors) %>%
    rename(group = mode_group)

  #dir_temp$direction[(nchar(dir_temp$origin) == 5 & str_sub(dir_temp$origin,1,2) %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))|(nchar(dir_temp$destination) == 5 & str_sub(dir_temp$destination,1,2) %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))] <- "Within ITTS"
  #dir_temp$direction[nchar(dir_temp$origin) == 5 & !(dir_temp$destination %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))] <- "Export"
  #dir_temp$direction[nchar(dir_temp$destination) == 5  & !(dir_temp$origin %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))] <- "Import"
  
  output$stab2_mode_bar <- renderPlotly({

    bar_plot_singleyear(dir_temp, #county = input$stab1_county_opts,
                        measure = input$stab2_value_opts,
                        sourceName = "mode_bar_plot")

  })

})

observeEvent(ignoreInit = TRUE, input$stab2_mainbutt,{

  #print(stringr::str_split(input$stab2_value_opts, "_"))
  dir_temp <- stab2_data() %>%
    pivot_longer(cols = starts_with(c("s1_tons","s1_value",
                                      "s2_tons","s2_value",
                                      "s3_tons","s3_value",
                                      "s0_tons","s0_value")),
                 names_sep = "_",
                 names_to = c("scenario","measure", "year"),
                 values_to = "value") %>%
    filter(measure == stringr::str_split(input$stab2_value_opts, "_")[[1]][1]) %>%
    filter(year == stringr::str_split(input$stab2_value_opts, "_")[[1]][2])

  dir_temp<-dir_temp %>%
    group_by(direction, scenario) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>% left_join(scen_colors) %>%
    rename(group = direction)

  output$stab2_dir_bar <- renderPlotly({

    bar_plot_singleyear(dir_temp, #county = input$stab1_county_opts,
                        measure = input$stab2_value_opts,
                        sourceName = "mode_bar_plot")

  })

})

observeEvent(ignoreInit = TRUE, input$stab2_mainbutt,{
  
  #print(stringr::str_split(input$stab2_value_opts, "_"))
  
  
  dir_temp <- stab2_data() %>%
    pivot_longer(cols = starts_with(c("s1_tons","s1_value",
                                      "s2_tons","s2_value",
                                      "s3_tons","s3_value",
                                      "s0_tons","s0_value")),
                 names_sep = "_",
                 names_to = c("scenario","measure", "year"),
                 values_to = "value") %>%
    filter(measure == stringr::str_split(input$stab2_value_opts, "_")[[1]][1]) %>%
    filter(year == stringr::str_split(input$stab2_value_opts, "_")[[1]][2])
  
  dir_temp<-dir_temp %>%
    group_by(Grouped_sctg2, scenario) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    rename(group = Grouped_sctg2)
  
  output$stab2_com_bar <- renderPlotly({
    
    bar_plot_singleyear(dir_temp, #county = input$stab1_county_opts,
                        measure = input$stab2_value_opts,
                        sourceName = "mode_bar_plot")
    
  })
  
})
# #slope charts ----------
# slope_chart <- function(df_in, meas = "tons"){
# 
# 
#   lplot <- plot_ly(df_in, x = ~scen_name, 
#                    y = ~com_rank, type = 'scatter', mode = 'lines+markers', color = ~Grouped_sctg2, line = list(shape = 'hvh')) %>%
#     layout(yaxis = list(title = 'Rank',
#                         nticks = 13,
#                         tick0 = 1,
#                         dtick = 1),
#            xaxis = list(title = '')) %>%
#     config(displayModeBar = FALSE)
# 
#   return(lplot)
# 
# }
# 
# observeEvent(ignoreInit = TRUE, input$stab2_mainbutt, {
# 
#   df_temp <- stab2_data() %>%
#     pivot_longer(cols = starts_with(c("s1_tons","s1_value",
#                                       "s2_tons","s2_value",
#                                       "s3_tons","s3_value",
#                                       "s0_tons","s0_value")),
#                  names_sep = "_",
#                  names_to = c("scenario","measure", "year"),
#                  values_to = "value") %>%
#     filter(measure == stringr::str_split(input$stab2_value_opts, "_")[[1]][1]) %>%
#     filter(year == stringr::str_split(input$stab2_value_opts, "_")[[1]][2]) %>%
#     group_by(Grouped_sctg2, scenario) %>%
#     summarise(value = sum(value)) %>%
#     ungroup() %>% left_join(scen_colors) %>%
#     mutate(rand = sample(0:2, n(), replace = TRUE)) %>%
#     group_by(scen_name) %>%
#     mutate(com_rank = rank(value*rand)) %>% ungroup() %>%
#     #filter(com_rank <= 5) %>%
#     select(scen_name, value, Grouped_sctg2, com_rank)
#   output$stab2_top_coms <- renderPlotly({
#    slope_chart(df_temp)
#     })
# })

#saney diagram-----------

sankey_diagram <- function(df_in, meas = "Tonnage"){

  blabs <- c("Alabama", "Arkansas", "Florida", "Georgia", "Kentucky",
             "Louisiana", "Mississippi", "Missouri", "North Carolina", "South Carolina",
             "Tennessee", "Texas", "Virginia",
            
             "Truck", "Rail", "Water", "Air", 
             "Mutliple Modes and Mail", "Pipeline", "Other and Unknown",
             
             "Aggregates", "Agriculture and Fish", "Energy Products", 
             "Machinery, Electric, and Precision Instruments", "Mixed Freight", "Nonmetallic Mineral and Base Metal Products", 
             "Raw and Finished Wood Products", "Waste and Scrap", "Chemicals, Pharmaceuticals, Plastics, and Rubber", 
             "Food, Alcohol and Tobacco", "Textiles and Leather", "Vehicles and Transportation Equipment")

  
  shell_orig <- data.frame(source = blabs,
                      index = c(seq(0:31))
                      )
  shell1 <- shell_orig[1:13,] %>% rename(source1 = source,
                                         index1 = index)
  shell2 <- shell_orig[14:20,] %>% rename(source2 = source, 
                                          index2 = index)
  shell3 <- shell_orig[21:32,] %>% rename(source3 = source,
                                          index3 = index)
  
  source_1t2 <- source1_temp <- shell1
  source_1t2$source2 <- shell2$source2[1]
  source_1t2$index2 <- shell2$index2[1]
  
  for(n in 2:nrow(shell2)){
    print(n)
    source_temp <- source1_temp
    source_temp$source2 <- shell2$source2[n]
    source_temp$index2 <- shell2$index2[n]
    source_1t2 <- rbind(source_1t2, source_temp)
  }
  
  source_2t3 <- source2_temp <- shell2
  source_2t3$source3 <- shell3$source3[1]
  source_2t3$index3 <- shell3$index3[1]
  
  for(n in 2:nrow(shell3)){
    print(n)
    source_temp <- source2_temp
    source_temp$source3 <- shell3$source3[n]
    source_temp$index3 <- shell3$index3[n]
    source_2t3 <- rbind(source_2t3, source_temp)
  }
  
  
  
  link_1t2 <- df_in %>% 
    #mutate(state = ifelse(nchar(origin)==5, str_sub(origin,1,2), str_sub(destination,1,2))) %>%
    left_join(state_join) %>% rename(source1 = state_lab) %>%
    left_join(ini_modecolors) %>% rename(source2 = mode_group) %>%
    group_by(source1, source2) %>% summarise(value = sum(value))
  

  link_2t3 <- df_in %>%
    left_join(ini_modecolors) %>% rename(source2 = mode_group) %>%
    rename(source3 = Grouped_sctg2) %>%
    group_by(source2, source3) %>% summarise(value = sum(value))
  
  source_1t2_full <- left_join(source_1t2, link_1t2) %>% filter(!is.na(value))
  source_2t3_full <- left_join(source_2t3, link_2t3) %>% filter(!is.na(value))

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
    value = c(source_1t2_full$value-1, source_2t3_full$value-1),
    label = paste("Tonnage: ", source_1t2_full$value, "\n")
  )
  )
  
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

observeEvent(ignoreInit = TRUE, input$stab2_mainbutt, {
  dat_temp<-stab2_data() %>%
    pivot_longer(cols = starts_with(c("s1_tons","s1_value",
                                      "s2_tons","s2_value",
                                      "s3_tons","s3_value",
                                      "s0_tons","s0_value")),
                 names_sep = "_",
                 names_to = c("scenario","measure", "year"),
                 values_to = "value") %>%
    filter(measure == stringr::str_split(input$stab2_value_opts, "_")[[1]][1]) %>%
    filter(year == stringr::str_split(input$stab2_value_opts, "_")[[1]][2]) %>%
    filter(scenario == input$stab2_sankey_filt)
  
  output$stab2_sankey <- renderPlotly({
    
    sankey_diagram(dat_temp)
  })
})

#international sankey----
# input <- list("stab2_commodity" = c("Agriculture and Fish",
#                                  "Energy Products", 
#                                  "Food, Alcohol and Tobacco",
#                                  "Machinery, Electric, and Precision Instruments",
#                                  "Mixed Freight",
#                                  "Waste and Scrap",
#                                  "Nonmetallic Mineral and Base Metal Products",
#                                  "Raw and Finished Wood Products",
#                                  "Chemicals, Pharmaceuticals, Plastics, and Rubber",
#                                  "Vehicles and Transportation Equipment",
#                                  "Textiles and Leather",
#                                  "Aggregates"),
#            "stab2_states" = c("01","05","12","13"))
# 
# df_in <- dat_sin %>%
#   mutate(state = ifelse(nchar(origin)!=1, str_sub(origin,1,2), str_sub(destination,1,2)),
#          country = ifelse(nchar(origin)==1, origin, destination)) %>%
#   filter(state %in% input$stab2_states) %>%
#   #mutate(direction = ifelse(nchar(origin)!=1, "Export","Import"))
#   #mutate(direction_filt = ifelse(direction == "Export","Outbound","Inbound")) %>%
#   #filter(direction %in% input$stab2_OD) %>%
#   #filter the simple ones
#   filter(Grouped_sctg2 %in% input$stab2_commodity) %>% 
#   #filter(dms_mode %in% input$stab2_mode) %>%
#   group_by(state, country, dms_mode, Grouped_sctg2) %>%
#   summarise(value = sum(Tons_2019))
#   
# 
# ini_sankey_diagram <- function(df_in, meas = "Tonnage"){
#   
#   blabs <- c("Alabama", "Arkansas", "Florida", "Georgia", "Kentucky",
#              "Louisiana", "Mississippi", "Missouri", "North Carolina", "South Carolina",
#              "Tennessee", "Texas", "Virginia",
#              
#              "Water", "Air", "Unknown",
#              
#              "Aggregates", "Agriculture and Fish", "Energy Products", 
#              "Machinery, Electric, and Precision Instruments", "Mixed Freight", "Nonmetallic Mineral and Base Metal Products", 
#              "Raw and Finished Wood Products", "Waste and Scrap", "Chemicals, Pharmaceuticals, Plastics, and Rubber", 
#              "Food, Alcohol and Tobacco", "Textiles and Leather", "Vehicles and Transportation Equipment",
#              
#              "Africa","Asia","Australia and Oceania","North America","South/Central America","Antarctica","Europe")
#   
#   
#   shell_orig <- data.frame(source = blabs,
#                            index = c(seq(0:34))
#   )
#   shell1 <- shell_orig[1:13,] %>% rename(source1 = source,
#                                          index1 = index)
#   shell2 <- shell_orig[14:16,] %>% rename(source2 = source, 
#                                           index2 = index)
#   shell3 <- shell_orig[17:28,] %>% rename(source3 = source,
#                                           index3 = index)
#   shell4 <- shell_orig[29:35,] %>% rename(source4 = source,
#                                           index4 = index)
#   source_1t2 <- source1_temp <- shell1
#   source_1t2$source2 <- shell2$source2[1]
#   source_1t2$index2 <- shell2$index2[1]
#   
#   for(n in 2:nrow(shell2)){
#     print(n)
#     source_temp <- source1_temp
#     source_temp$source2 <- shell2$source2[n]
#     source_temp$index2 <- shell2$index2[n]
#     source_1t2 <- rbind(source_1t2, source_temp)
#   }
#   
#   source_2t3 <- source2_temp <- shell2
#   source_2t3$source3 <- shell3$source3[1]
#   source_2t3$index3 <- shell3$index3[1]
#   
#   for(n in 2:nrow(shell3)){
#     print(n)
#     source_temp <- source2_temp
#     source_temp$source3 <- shell3$source3[n]
#     source_temp$index3 <- shell3$index3[n]
#     source_2t3 <- rbind(source_2t3, source_temp)
#   }
#   
#   source_3t4 <- source3_temp <- shell3
#   source_3t4$source4 <- shell4$source4[1]
#   source_3t4$index4 <- shell4$index4[1]
#   
#   for(n in 2:nrow(shell4)){
#     print(n)
#     source_temp <- source3_temp
#     source_temp$source4 <- shell4$source4[n]
#     source_temp$index4 <- shell4$index4[n]
#     source_3t4 <- rbind(source_3t4, source_temp)
#   }
#   
#   link_1t2 <- df_in %>% 
#     #mutate(state = ifelse(nchar(origin)==5, str_sub(origin,1,2), str_sub(destination,1,2))) %>%
#     left_join(state_join) %>% rename(source1 = state_lab) %>%
#     left_join(ini_modecolors) %>% rename(source2 = mode_group) %>%
#     group_by(source1, source2) %>% summarise(value = sum(value))
#   
#   
#   link_2t3 <- df_in %>%
#     left_join(ini_modecolors) %>% rename(source2 = mode_group) %>%
#     rename(source3 = Grouped_sctg2) %>%
#     group_by(source2, source3) %>% summarise(value = sum(value))
#   
#   link_3t4 <- df_in %>%
#     left_join(ini_international) %>% rename(source3 = Grouped_sctg2) %>%
#     rename(source4 = country_lab) %>%
#     group_by(source3, source4) %>% summarise(value = sum(value))
#   
#   source_1t2_full <- left_join(source_1t2, link_1t2) %>% filter(!is.na(value))
#   source_2t3_full <- left_join(source_2t3, link_2t3) %>% filter(!is.na(value))
#   source_3t4_full <- left_join(source_3t4, link_3t4) %>% filter(!is.na(value))
#   #source_1t2_full$value[is.na(source_1t2_full$value)] <- 0
#   #source_2t3_full$value[is.na(source_2t3_full$value)] <- 0
#   #source_3t4_full$value[is.na(source_3t4_full$value)] <- 0
#   
#   snkey <- plot_ly(type = "sankey",
#                    
#                    #domain = list(
#                    #  x =  c(0,1),
#                    #  y =  c(0,1)
#                    #),
#                    
#                    #orientation = "h",
#                    #valueformat = ".0f",
#                    #valuesuffix = "TWh",
#                    
#                    node = list(
#                      label = blabs,
#                      #data$data[[1]]$node$color,
#                      pad = 15,
#                      thickness = 15,
#                      line = list(
#                        color = "black",
#                        width = 0.5
#                      )
#                    ),
#                    
#                    link = list(
#                      source = c(source_1t2_full$index1-1,
#                                 source_2t3_full$index2-1,
#                                 source_3t4_full$index3-1
#                      ),
#                      target = c(source_1t2_full$index2-1,
#                                 source_2t3_full$index3-1,
#                                 source_3t4_full$index4-1
#                      ),
#                      value =  c(source_1t2_full$value,
#                                 source_2t3_full$value,
#                                 source_3t4_full$value
#                      )#,
#                      #label =  blabs
#                    )
#   ) %>% config(displayModeBar = FALSE)
#   
#   return(snkey)
# }