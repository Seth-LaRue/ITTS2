click_counties_cs <- reactiveValues(curr=NULL,prev=NULL)


#initial datatables -----------------------------------------------------------------------

dat_ini_rs <- dat_ss %>%
  mutate(origin = ifelse(origin %in% c("05", "12","13","21","22","28","29","45","48","51"),'ITTS',origin),
         destination = ifelse(destination %in% c("05", "12","13","21","22","28","29","45","48","51"), 'ITTS',destination)) %>%
  filter(origin == 'ITTS'|destination == 'ITTS') %>%
  mutate(GEOID = if_else(origin == 'ITTS', destination, origin)) %>% 
  group_by(GEOID)%>% 
  summarise(tons_2022 = sum(tons_2022),
            tons_2050 = sum(tons_2050),
            value_2022 = sum(value_2022),
            value_2050 = sum(value_2050)
  ) %>%
  ungroup() %>%
  mutate(rank = rank(desc(value_2022)))

ln_select_cs_ini<- ITTS_base %>%
        select(GEOID, NAME) %>%
        inner_join(dat_ini_rs,by = "GEOID")

output$odmap_cs <- renderLeaflet({

  pal_factor_ini_cs <- colorQuantile(palette = "Blues",domain = ln_select_cs_ini$value_2022,probs = seq(0, 1, .2))
  pulsecolor_ini_cs='red'
  
  pal_factor_colors_ini_cs <- unique(pal_factor_ini_cs(sort(ln_select_cs_ini$value_2022)))
  pal_factor_labs_ini_cs <- round(quantile(round(ln_select_cs_ini$value_2022, 1), probs = seq(0, 1, .2)), 1)
  pal_factor_labs_ini_cs <- paste(scales::comma(lag(pal_factor_labs_ini_cs)), scales::comma(pal_factor_labs_ini_cs), sep = " - ")[-1]
  
  con_name_ini_cs=ITTS_base$NAME[ITTS_base$GEOID == 'ITTS']
  titl_ini_cs = paste0("Inbound & Outbound to </br>", con_name_ini_cs, str_replace(str_to_title('value_2022'),'_',' '), " (Thousand tons)")
  
  
  m_cs %>%
    addPolygons(data = ln_select_cs_ini,
                layerId = ~paste("data", ln_select_cs_ini$GEOID),
                fillColor = ~pal_factor_ini_cs(value_2022),
                stroke=TRUE,
                smoothFactor = 0.3,
                color = '#5a5a5a',#~hili,
                weight = 1,
                label = ~NAME,
                labelOptions = labelOptions(
                  style = list("front-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"),
                fillOpacity  = 1) %>%
    addPolygons(data = ITTS_hatch,
                color = st_border_color,
                weight = 1,
                opacity = 1
    ) %>%
    addPolygons(data = ITTS_base[ITTS_base$NAME == 'ITTS',],
                layerId = ~GEOID,
                color = '#27A570',
                weight = 6,
                smoothFactor = 0.3,
                opacity = .4,
                fillOpacity = 0,
                label = ~NAME,
                labelOptions = labelOptions(
                  style = list("front-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"),
                highlightOptions = highlightOptions(
                  weight = 2,
                  color = "red",
                  fillOpacity = 0,
                  bringToFront = TRUE)) %>%
    addLegend(position = "bottomright",
              layerId = 'leg',
              colors = pal_factor_colors_ini_cs,
              labels = pal_factor_labs_ini_cs,
              title = titl_ini_cs) %>%
    addPulseMarkers(
      layerId = 'pulsemarker',
      lng = -88.69515507554627, lat = 34.67670663249491, 
      label = con_name_ini_cs,
      labelOptions = labelOptions(
        style = list("front-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"),
      icon = makePulseIcon(heartbeat = 1,iconSize=10,
                           color=pulsecolor_ini_cs))

  })

# reactive ---------------------------------------------------------------------

#This is a basic map of reactive values intended to fix redundant updating of certain things

#input$cors_opts > input$county_opts_cs > click_counties_cs
#                                       >
#input$Value_opts_cs > input$Scenario_opt_cs
#input$odmap_cs_shape_click ~> input$county_opts_cs
#                           ~> click_counties (removing this just to see)


#UI Updates based on user selections--------------------------------------------
#This updates the state/county/region input options
observeEvent(input$cors_opts, {
  
  print("UPDATING: input$county_ops_cs")
  if(input$cors_opts=="c2c"){
    updateSelectizeInput(session, 'county_opts_cs', label = "County", choices = cty_ch, selected = c("Travis County, TX", value = "48453"), server = FALSE)
    click_counties_cs$curr <- "48453"
  } else if(input$cors_opts=="s2s"){
    updateSelectizeInput(session, 'county_opts_cs', label = "State", choices = state_ch, selected = c("Texas", value = "48"), server = FALSE)
    click_counties_cs$curr <- "48"
  } else if(input$cors_opts=="r2s"){
    updateSelectizeInput(session, 'county_opts_cs', label = "Region", choices = c('ITTS', 'Southeast Region'), selected = "ITTS", server = FALSE)
    click_counties_cs$curr <- "ITTS"
  }
  
  print(paste("CHECK input$county_ops_cs: ",input$county_ops_cs))
  
})

#this updates click_counties_cs
observeEvent(input$county_opts_cs, {
  print("UPDATING: click_counties_cs")
  req(input$county_opts_cs)
  cnty_cs = all_selected$GEOID[all_selected$GEOID == input$county_opts_cs]
  
  click_counties_cs$prev = click_counties_cs$curr
  click_counties_cs$curr <- cnty_cs
})

#this updates scenario selection
observeEvent(input$Value_opts_cs, {
  req(input$county_opts_cs, input$Value_opts_cs, input$Scenario_opt_cs)
  isolate({
    if (grepl('2022', input$Value_opts_cs)) {
      updateSelectizeInput(session, 'Scenario_opt_cs', label = 'Scenario Options', choices = c('Baseline'), selected = 'Baseline', server = TRUE)
    } else {
      # Check if the current selection is not "Baseline"
      if (input$Scenario_opt_cs != "Baseline") {
        # No need to update, as it's already set to a scenario option
        return(NULL)
      } else {
        # Update to the selected scenario
        updateSelectizeInput(session, 'Scenario_opt_cs', label = 'Scenario Options', choices = c('Baseline' = 'Baseline',
                                                                                                 'Scenario 1: Respond to Heightened Supply Chain Risks' = '_s1',
                                                                                                 'Scenario 2: Leverage Multi-State Strength' = '_s2',
                                                                                                 'Scenario 3: Embrace Technology Transformations' = '_s3'),
                             selected = 'Baseline', server = FALSE)
      }
    }
  })
},ignoreInit = TRUE)



observeEvent(input$odmap_cs_shape_click, {
  req(input$odmap_cs_shape_click)
  print("UPDATING: county_opts_cs")
  
  if (!is.null(input$odmap_cs_shape_click$id)) {
    
    if (input$odmap_cs_shape_click$id %in% all_selected$GEOID) {
      
      sel = all_selected$NAME[all_selected$GEOID == input$odmap_cs_shape_click$id]
      
      if (input$cors_opts == "c2c") {
        
        updateSelectizeInput(
          session,
          'county_opts_cs',
          choices = cty_ch,
          selected = c(sel, value = input$odmap_cs_shape_click$id),
          server = TRUE
        )
        
        #click_counties_cs$prev = click_counties_cs$curr
        #click_counties_cs$curr = input$odmap_cs_shape_click$id
        
      } else if (input$cors_opts == "s2s") {
        
        updateSelectizeInput(
          session,
          'county_opts_cs',
          choices = state_ch,
          selected = c(sel, value = input$odmap_cs_shape_click$id),
          server = TRUE
        )
        
        #click_counties_cs$prev = click_counties_cs$curr
        #click_counties_cs$curr = input$odmap_cs_shape_click$id
        
      } else if (input$cors_opts == "r2s") {
        #do nothing
      }
      
    }
  }
  
}) 

#what is this for?
SETTS_ss_cs_r <- reactiveValues(SETTS_ss_cs=ln_select_cs_ini %>% st_drop_geometry())

data_ss_click_cs<- reactive({ 
  #req(n_lines_disp$curr)
  req(click_counties_cs$curr)
  req(input$dms_mode_opts_cs)
  req(input$sctg2_opts_cs)
  req(input$Value_opts_cs)
  req(input$Scenario_opt_cs)
  req(input$OD_opts_cs)
  req(input$county_opts_cs)
  req(input$n_top_cs)
  req(input$cors_opts)
  
  #browser()
  print('Running: data_ss_click_cs')
  #additional filtering can go here
  #browser()
  
  #filters for type of map
  if(input$cors_opts == "c2c"){
    dat_temp_cs <- dat_cs
  } else if(input$cors_opts == "s2s"){
    dat_temp_cs <- dat_ss
  } else if (input$cors_opts == 'r2s'){
    
    if (input$county_opts_cs == 'ITTS'){ # itts region 
      dat_temp_cs <- dat_ss %>%
        mutate(origin = ifelse(origin %in% c("05", "12","13","21","22","28","29","45","48","51"),'ITTS',origin),
               destination = ifelse(destination %in% c("05", "12","13","21","22","28","29","45","48","51"), 'ITTS',destination))
    } else { # southeast region data
      dat_temp_cs <- dat_ss %>%
        mutate(origin = ifelse(origin %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"),'Southeast Region',origin),
               destination = ifelse(destination %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"),'Southeast Region',destination))
    }
  }
  
  #filter for direction
  if(input$OD_opts_cs != "Both"){
    
    if(input$OD_opts_cs == "dms_orig"){
      
      dat_temp_cs <- dat_temp_cs %>%
        filter(origin == click_counties_cs$curr) %>%
        mutate(GEOID = destination)
      
    } else {
      
      dat_temp_cs <- dat_temp_cs %>%
        filter(destination == click_counties_cs$curr) %>%
        mutate(GEOID = origin)
    }
    
  } else {
    
    dat_temp_cs <- dat_temp_cs %>%
      filter(origin == click_counties_cs$curr|destination == click_counties_cs$curr) %>%
      mutate(GEOID = ifelse(origin == click_counties_cs$curr, destination, origin))
    
  }
  
    #filtering for mode
    if(input$dms_mode_opts_cs != "All" & nrow(dat_temp_cs) >=1) {
      dat_temp_cs = dat_temp_cs %>%
        filter(dms_mode == input$dms_mode_opts_cs)}
  
    #filter for commodity
    if(input$sctg2_opts_cs != "All" & nrow(dat_temp_cs) >=1) {
      
      dat_temp_cs = dat_temp_cs %>%
        filter(Grouped_sctg2==input$sctg2_opts_cs)
      
      }
  
  
    #adding scenarios ands summarising
    if(input$sctg2_opts_cs == "All" | input$dms_mode_opts_cs == "All"){
      
      if(input$Scenario_opt_cs == 'Baseline' |grepl('2022',input$Value_opts_cs)){
        
        dat_temp_cs = dat_temp_cs %>%
          group_by(GEOID)%>%
          summarise(tons_2022 = sum(tons_2022),
                     tons_2050 = sum(tons_2050),
                     value_2022 = sum(value_2022),
                     value_2050 = sum(value_2050)
          ) %>%
          ungroup()
        
        selected_col = input$Value_opts_cs
        
      } else{
        
        dat_temp_cs = process_scenario(dat_temp_cs,
                                       input$Value_opts_cs,
                                       input$Scenario_opt_cs,
                                       click_counties_cs$curr,
                                       c('origin', 'destination', 'GEOID'),
                                       1) 
        
        selected_col <- paste0(input$Value_opts_cs, input$Scenario_opt_cs)
        
      }
      
    } else {
      
      if(input$Scenario_opt_cs == 'Baseline' | grepl('2022',input$Value_opts_cs)){
        
        dat_temp_cs = dat_temp_cs %>%
          select(origin, destination, GEOID, contains('tons_'),contains('value_'))
        
        selected_col = input$Value_opts_cs
        
      }else{
        
        dat_temp_cs = process_scenario(dat_temp_cs,
                                       input$Value_opts_cs,
                                       input$Scenario_opt_cs,
                                       click_counties_cs$curr,
                                       c('origin', 'destination', 'GEOID','Grouped_sctg2','dms_mode'),
                                       1)
        
        selected_col <- paste0(input$Value_opts_cs, input$Scenario_opt_cs)
        
      }
    }
  
  dat_temp_cs = dat_temp_cs %>%
    rename(factor_lab = selected_col) %>%
    mutate(rank = rank(desc(factor_lab)))
  
  
  if(nrow(dat_temp_cs)>0 & input$cors_opts %in% c('s2s','c2c')){
    
    ln_select_cs <- state_base %>%
      select(GEOID, NAME) %>%
      inner_join(dat_temp_cs,by = "GEOID") %>%
      mutate(tranp=ifelse(rank <= input$n_top_cs, 1,.25))
    
  } else if (nrow(dat_temp_cs)>0 & input$cors_opts == 'r2s') {
    
    if (input$county_opts_cs == 'ITTS'){
      
      ln_select_cs <- ITTS_base %>%
        select(GEOID, NAME) %>%
        inner_join(dat_temp_cs,by = "GEOID") %>%
        mutate(tranp=ifelse(rank <= input$n_top_cs, 1,.25))
      
      
    } else {
      
      ln_select_cs <- SE_base %>%
        select(GEOID, NAME) %>%
        inner_join(dat_temp_cs,by = "GEOID") %>%
        mutate(tranp=ifelse(rank <= input$n_top_cs, 1,.25))
      
    }
    
    
    
  } else {ln_select_cs=NULL}
  
  
  return(ln_select_cs)
  
})

#cs map update
observeEvent(eventExpr = data_ss_click_cs(),{
  req(click_counties_cs$curr,
      input$dms_mode_opts_cs, 
      input$sctg2_opts_cs, 
      input$Value_opts_cs,
      #input$Scenario_opt_cs, 
      input$n_top_cs#,
      #input$cors_opts
      #input$county_opts_cs
      )
  #browser()
  print("RUNNING: Observe of map_update_cs()")
  #do we have to use the entire line file to remove?
  #Print Running Observe event based on map_update_cs
  print("CALLING: data_ss_click_cs()")
  ln_select_cs=data_ss_click_cs()
  if(is.null(ln_select_cs)&input$cors_opts=='c2c'){
    leafletProxy(mapId = "odmap_cs",session = session) %>%
      removeShape(layerId = paste("data", state_base$GEOID)) %>%
      removeShape(layerId = paste(all_selected$GEOID)) %>%
      #removeShape(layerId = 'leg') %>%
      clearControls() %>%
      removeShape(layerId = 'pulsemarker')
  } else if(is.null(ln_select_cs)&input$cors_opts=='s2s') {
    
    leafletProxy(mapId = "odmap_cs",session = session) %>%
      removeShape(layerId = paste("data", state_base$GEOID)) %>%
      removeShape(layerId = paste(all_selected$GEOID)) %>%
      #removeShape(layerId = 'leg') %>%
      clearControls() %>%
      removeShape(layerId = 'pulsemarker')}
  else if(is.null(ln_select_cs)&input$cors_opts=='r2s') {
    if (input$county_opts_cs == 'ITTS'){
      leafletProxy(mapId = "odmap_cs",session = session) %>%
        removeShape(layerId = paste("data", ITTS_base$GEOID)) %>%
        removeShape(layerId = paste(all_selected$GEOID)) %>%
        #removeShape(layerId = 'leg') %>%
        clearControls() %>%
        removeShape(layerId = 'pulsemarker')
    }else {
      leafletProxy(mapId = "odmap_cs",session = session) %>%
        removeShape(layerId = paste("data", SE_base$GEOID)) %>%
        removeShape(layerId = paste(all_selected$GEOID)) %>%
        #removeShape(layerId = 'leg') %>%
        clearControls() %>%
        removeShape(layerId = 'pulsemarker')
    }
    
  }
  
  
  
  
  
  
  if(!is.null(ln_select_cs)) {
    if(input$cors_opts %in% c('s2s','c2c')){
      leafletProxy(mapId = "odmap_cs",session = session) %>%
        removeShape(layerId = paste("data", ITTS_base$GEOID)) %>%
        removeShape(layerId = paste(all_selected$GEOID)) %>%
        #removeShape(layerId = 'leg') %>%
        clearControls() %>%
        removeShape(layerId = 'pulsemarker')}
    else if(input$cors_opts == 'r2s'){
      if (input$county_opts_cs == 'ITTS'){
        
        leafletProxy(mapId = "odmap_cs",session = session) %>%
          removeShape(layerId = paste("data", ITTS_base$GEOID)) %>%
          removeShape(layerId = paste(all_selected$GEOID)) %>%
          #removeShape(layerId = 'leg') %>%
          clearControls() %>%
          removeShape(layerId = 'pulsemarker')
      } else {
        leafletProxy(mapId = "odmap_cs",session = session) %>%
          removeShape(layerId = paste("data", SE_base$GEOID)) %>%
          removeShape(layerId = paste(all_selected$GEOID)) %>%
          #removeShape(layerId = 'leg') %>%
          clearControls() %>%
          removeShape(layerId = 'pulsemarker')
      }
    }
    
    ln_select_cs <- ln_select_cs %>% 
      arrange(-rank)
    
    #lines_labs <- paste0(input$Value_opts_cs,': ',round(ln_select_cs$rank,6)) %>% lapply(htmltools::HTML)
    con_name = all_selected$NAME[all_selected$GEOID == click_counties_cs$curr]
    
    
    if(input$OD_opts_cs == "Both"){
      dir = "Inbound & Outbound to "
    }else if(input$OD_opts_cs == "dms_orig"){
      dir = "Outbound from "
    } else {
      dir = "Inbound to "
    }
    
    
    
    if(grepl('tons',input$Value_opts_cs)){
      titl = paste0(dir, con_name, "</br>", str_replace(str_to_title(input$Value_opts_cs,),'_',' '), " (Thousand tons)")
      pulsecolor='blue'
      
      pal_factor <- colorQuantile(
        palette = "YlOrRd",
        domain = ln_select_cs$factor_lab[!duplicated(ln_select_cs$factor_lab)],
        probs = seq(0, 1, .2)
      )
      
    } else {
      titl = paste0(dir, con_name, "</br>", str_replace(str_to_title(input$Value_opts_cs,),'_',' '), " ($Million)")
      pulsecolor='red'
      pal_factor <- colorQuantile(
        palette = "Blues",
        domain = ln_select_cs$factor_lab[!duplicated(ln_select_cs$factor_lab)],
        probs = seq(0, 1, .2)
      )
      
    }
    
    
    if(nrow(ln_select_cs)== 1){
      pal_factor_labs = round(ln_select_cs$factor_lab, 1)
      if(grepl('tons',input$Value_opts_cs)){
        pal_factor = function(nada){return("#bd0026")}
        pal_factor_colors = "#bd0026"
      } else {
        pal_factor = function(nada){return('#08306b')}
        pal_factor_colors = '#08306b'
      }
    } else {
      pal_factor_colors <- unique(pal_factor(sort(ln_select_cs$factor_lab[!duplicated(ln_select_cs$factor_lab)])))
    }
    
    steps = 1/length(pal_factor_colors)
    pal_factor_labs <- round(quantile(round(ln_select_cs$factor_lab[!duplicated(ln_select_cs$factor_lab)], 1), probs = seq(0, 1, steps)), 1)
    pal_factor_labs <- paste(scales::comma(lag(pal_factor_labs)), scales::comma(pal_factor_labs), sep = " - ")[-1]
    
    if(input$cors_opts=="c2c"){
      pulse_name = county_selected$county_lab[county_selected$GEOID == click_counties_cs$curr]
      
      all_counties_centr_sel=all_counties_centr %>% 
        filter(GEOID==click_counties_cs$curr)
      
      leafletProxy(mapId = "odmap_cs",session = session) %>%
        addPolygons(data = ln_select_cs,
                    layerId = ~paste("data", ln_select_cs$GEOID),
                    fillColor = ~pal_factor(factor_lab),
                    stroke=TRUE,
                    color = st_border_color,
                    weight = 1,
                    label = ~NAME, 
                    labelOptions = labelOptions(
                      style = list("front-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"),
                    #opacity  = .85,#~rank/max(rank),
                    fillOpacity  =  ~tranp#,#~rank/max(rank),
                    #popup = lines_labs
        ) %>% 
        addPolygons(data = county_selected,
                    layerId = ~GEOID,
                    color = cty_border_color,
                    weight = .5,
                    smoothFactor = 0.3,
                    opacity = 0.4,
                    fillOpacity = 0.1,
                    label = cty_labels,
                    labelOptions = labelOptions(
                      style = list("front-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"),
                    highlightOptions = highlightOptions(
                      weight = 2,
                      color = "red",
                      fillOpacity = 0.7,
                      bringToFront = TRUE)) %>%
        addLegend(position = "bottomright",
                  layerId = 'leg',
                  colors = pal_factor_colors,
                  labels = pal_factor_labs, 
                  title = titl) %>%
        addPulseMarkers(
          layerId = 'pulsemarker',
          lng = all_counties_centr_sel$long, lat = all_counties_centr_sel$lat,
          label = pulse_name,
          labelOptions = labelOptions(
            style = list("front-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"),
          icon = makePulseIcon(heartbeat = 1,iconSize=10,
                               color=pulsecolor))
    } else if(input$cors_opts == "s2s"){
      all_states_centr_sel=all_states_centr %>% 
        filter(GEOID==click_counties_cs$curr)
      pulse_name = state_choices$NAME[state_choices$GEOID == click_counties_cs$curr]
      
      leafletProxy(mapId = "odmap_cs",session = session) %>%
        addPolygons(data = ln_select_cs,
                    layerId = ~paste("data", GEOID),
                    fillColor = ~pal_factor(factor_lab),
                    stroke=TRUE,
                    color = st_border_color,
                    weight = 1,
                    #opacity  = .85,#~rank/max(rank),
                    fillOpacity  =  ~tranp,
                    label = ~NAME,
                    labelOptions = labelOptions(
                      style = list("front-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")
        ) %>%
        addPolygons(data = all_selected[all_selected$GEOID %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"),],
                    layerId = ~GEOID,
                    #fillColor = NULL,
                    stroke=TRUE,
                    color = st_border_color,
                    weight = 1,
                    #opacity  = .85,#~rank/max(rank),
                    fillOpacity  = 0,#,#~rank/max(rank),
                    label = ~NAME,
                    labelOptions = labelOptions(
                      style = list("front-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"),
                    highlightOptions = highlightOptions(
                      weight = 2,
                      color = "red",
                      fillOpacity = 0)#,
                    #bringToFront = TRUE)
        ) %>%
        addLegend(position = "bottomright",
                  layerId = 'leg',
                  colors = pal_factor_colors,
                  labels = pal_factor_labs,
                  title = titl) %>%
        addPulseMarkers(
          layerId = 'pulsemarker',
          lng = all_states_centr_sel$long, lat = all_states_centr_sel$lat,
          label = pulse_name,
          labelOptions = labelOptions(
            style = list("front-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"),
          icon = makePulseIcon(heartbeat = 1,iconSize=10,
                               color=pulsecolor))
    }else if(input$cors_opts == "r2s"){
      if (input$county_opts_cs == 'ITTS'){
        
        # all_states_centr_sel=all_states_centr %>% 
        #   filter(GEOID==click_counties_cs$curr)
        pulse_name = click_counties_cs$curr
        
        leafletProxy(mapId = "odmap_cs",session = session) %>%
          addPolygons(data = ln_select_cs,
                      layerId = ~paste("data", GEOID),
                      fillColor = ~pal_factor(factor_lab),
                      stroke=TRUE,
                      color = st_border_color,
                      weight = 1,
                      #opacity  = .85,#~rank/max(rank),
                      fillOpacity  =  ~tranp,
                      label = ~NAME,
                      labelOptions = labelOptions(
                        style = list("front-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")
          ) %>%
          addPolygons(data = ITTS_hatch,
                      color = st_border_color,
                      weight = 1,
                      opacity = 1
          ) %>%
          addPolygons(data = all_selected[all_selected$GEOID == 'ITTS',],
                      layerId = ~GEOID,
                      #fillColor = NULL,
                      stroke=TRUE,
                      color = '#27A570',      
                      weight = 6,
                      #opacity  = .85,#~rank/max(rank),
                      fillOpacity  = 0,#,#~rank/max(rank),
                      label = ~NAME,
                      labelOptions = labelOptions(
                        style = list("front-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"),
                      highlightOptions = highlightOptions(
                        weight = 2,
                        color = "red",
                        fillOpacity = 0)#,
                      #bringToFront = TRUE)
          ) %>%
          addLegend(position = "bottomright",
                    layerId = 'leg',
                    colors = pal_factor_colors,
                    labels = pal_factor_labs,
                    title = titl) %>%
          addPulseMarkers(
            layerId = 'pulsemarker',
            lng = -88.69515507554627, lat = 34.67670663249491,
            label = pulse_name,
            labelOptions = labelOptions(
              style = list("front-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"),
            icon = makePulseIcon(heartbeat = 1,iconSize=10,
                                 color=pulsecolor))
      } else {
        # all_states_centr_sel=all_states_centr %>% 
        #   filter(GEOID==click_counties_cs$curr)
        pulse_name = click_counties_cs$curr
        
        leafletProxy(mapId = "odmap_cs",session = session) %>%
          addPolygons(data = ln_select_cs,
                      layerId = ~paste("data", GEOID),
                      fillColor = ~pal_factor(factor_lab),
                      stroke=TRUE,
                      color = st_border_color,
                      weight = 1,
                      #opacity  = .85,#~rank/max(rank),
                      fillOpacity  =  ~tranp,
                      label = ~NAME,
                      labelOptions = labelOptions(
                        style = list("front-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")
          ) %>%
          addPolygons(data = SE_hatch,
                      color = st_border_color,
                      weight = 1,
                      opacity = 1
          ) %>%
          addPolygons(data = all_selected[all_selected$GEOID == 'Southeast Region',],
                      layerId = ~GEOID,
                      #fillColor = NULL,
                      stroke=TRUE,
                      color = '#27A570',  #st_border_color
                      weight = 6,
                      #opacity  = .85,#~rank/max(rank),
                      fillOpacity  = 0,#,#~rank/max(rank),
                      label = ~NAME,
                      labelOptions = labelOptions(
                        style = list("front-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"),
                      highlightOptions = highlightOptions(
                        weight = 2,
                        color = "red",
                        fillOpacity = 0)#,
                      #bringToFront = TRUE)
          ) %>%
          addLegend(position = "bottomright",
                    layerId = 'leg',
                    colors = pal_factor_colors,
                    labels = pal_factor_labs,
                    title = titl) %>%
          addPulseMarkers(
            layerId = 'pulsemarker',
            lng = -88.69515507554627, lat = 34.67670663249491,
            label = pulse_name,
            labelOptions = labelOptions(
              style = list("front-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"),
            icon = makePulseIcon(heartbeat = 1,iconSize=10,
                                 color=pulsecolor))
      }
    }
  }
  print("END: Observe")
},ignoreInit = TRUE)

outputOptions(output, 'odmap_cs', suspendWhenHidden = FALSE)

table_titl <- reactiveVal("")
# output$table_title_cs <- renderText({
#   req(click_counties_cs$curr)
observe({
  if(input$OD_opts_cs != "Both"){
    if(input$OD_opts_cs == "dms_orig"){
      output$table_title_cs = renderText(paste0("Origin: ", all_selected$NAME[all_selected$GEOID == click_counties_cs$curr]))
      
    } else {
      output$table_title_cs = renderText(paste0("Destination: ", all_selected$NAME[all_selected$GEOID == click_counties_cs$curr]))
    }
  } else {
    output$table_title_cs = renderText(paste0("Selected: ", all_selected$NAME[all_selected$GEOID == click_counties_cs$curr]))
  }
  
})
#output$table_title_cs <- renderText({ table_titl() })

output$scenario_title_cs <- renderText({
  
  if (input$Scenario_opt_cs == '_s1'){
    sencario = paste0("Selected Scenario: ", 'Scenario 1- Respond to Heightened Supply Chain Risks')
  }else if (input$Scenario_opt_cs == '_s2'){
    sencario = paste0("Selected Scenario: ", 'Scenario 2- Leverage Multi-State Strength')
  }else if (input$Scenario_opt_cs == '_s3'){
    sencario = paste0("Selected Scenario: ", 'Scenario 3- Embrace Technology Transformations')
  }else{
    sencario = paste0("Selected Scenario: ", input$Scenario_opt_cs)}
  return(sencario)
})

output$scenario_text_output_cs <- renderText({
  req(input$Scenario_opt_cs)
  
  if(input$Scenario_opt_cs == '_s1'){
    return('In Scenario 1, due to the resilience and readiness of the region’s supply chains, the Southeast
           region increases its production of consumer goods and agricultural goods over time. The Southeast
           grows its production of consumer goods at a 2.9% annual rate. For agriculture, the tonnage of 
           these goods originating in the Southeast will increase by about 1.3% annually.')
  } else if (input$Scenario_opt_cs == '_s2'){
    return('Recognizing that economies are cyclic, production occurs throughout the world, and weather and 
    other events can cause major shocks to the system, and that production (particularly resource-based) depends
    on weather and other high-variability factors, no region of the US can be self-sufficient. For reasons of 
    regional protection (from disruption and negative change) and also economic competitiveness and opportunity, 
    the ITTS region aims to: (1) produce a majority share of goods consumed in the Southeast region within the 
    Southeast region; (2) become a net exporter of goods to the rest of the US and the world; (3) serve as the 
    leading global gateway for international imports and exports for the entire US; (4) operate from a platform 
    of heightened energy self-sufficiency. In Scenario 2, Southeast trade overall increases, and the region is 
    assumed to grow more energy-independent. As a result, non-energy Southeast imports and exports grow at 2.9% 
    and 2.3% annually, respectively. For energy products, Southeast imports of energy products decline at an annual
    rate of -1.4%, and exports increase 2.95% annually.')
  }else if (input$Scenario_opt_cs == '_s3'){
    return('As the U.S. reduces its use of coal and gasoline and embraces alternative energy, energy production 
    technologies are rapidly evolving, impacting the movement of various commodities. Comparable technology 
    transformations in agriculture and other resource industries may occur, and distributed manufacturing (3-D 
    printing) may “flatten” or simplify the traditional three-tiered (resources, intermediate products, final 
    products) supply chain. In Scenario 3, coal shipments in the Southeast are projected to decline by approximately
    -4.5% annually until they vanish from the network. And high-tech durable manufacturing goods will grow by 
    2.8% annually through 2050.')
  }
  
})


output$subsetSETTS_cs<-renderDataTable({#server = FALSE,{
  print("RENDERING: subsetSETTS_cs")
  
  if(input$Scenario_opt_cs == 'Baseline'){
    print("CALLING: data_ss_click_cs point 1")
    #browser()
    #ln_select_cs=ln_select_cs_ini()
    ln_select_cs=data_ss_click_cs()
    
    print(paste0("Printing Names: ", names(ln_select_cs)))
    
    names(ln_select_cs)[names(ln_select_cs)=='factor_lab'] = input$Value_opts_cs
    
    print('is the issue here 0?')
  }else{
    
    print("CALLING: data_ss_click_cs point 2")
    ln_select_cs=data_ss_click_cs()
    names(ln_select_cs)[names(ln_select_cs)=='factor_lab']=paste0(input$Value_opts_cs, input$Scenario_opt_cs)
    
  }
  
  SETTS_ss_cs<-ln_select_cs %>% 
    st_drop_geometry() 
  
  if(input$OD_opts_cs == "Both"){
    SETTS_ss_cs<-SETTS_ss_cs %>% 
      left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("GEOID"))
  } else if(input$OD_opts_cs == "dms_orig"){
    
    SETTS_ss_cs<-SETTS_ss_cs %>%
      left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("GEOID"))
    
  } else if(input$OD_opts_cs == "dms_dest"){
    
    SETTS_ss_cs<-SETTS_ss_cs %>%
      left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("GEOID"))
    
  }
  
  SETTS_ss_cs<-SETTS_ss_cs %>%
    arrange(rank) %>% 
    select('NAME',starts_with('tons_'), starts_with('value_'))
  
  SETTS_ss_cs_r$SETTS_ss_cs=SETTS_ss_cs
  
  SETTS_ss_cs<-SETTS_ss_cs %>%
    mutate_at(vars(contains('tons_'),contains('value_')),~round(.,1)) %>% 
    rename('Tons 2022</br>(Thousand Tons)'='tons_2022',
           'Tons 2050</br>(Thousand Tons)'='tons_2050',
           'Value 2022</br>($Million)'='value_2022',
           'Value 2050</br>($Million)'='value_2050')
  names(SETTS_ss_cs)[grepl('_',names(SETTS_ss_cs))] <- str_to_title(gsub("_"," ",names(SETTS_ss_cs)[grepl('_',names(SETTS_ss_cs))]))
  
  #rename_all(~str_replace_all(.,'_',' ') %>% str_to_title(.)) 
  
  if(input$cors_opts=="c2c"){
    SETTS_ss_cs<-SETTS_ss_cs %>%
      rename('State'='NAME')#%>%
    #filter(rank <= input$n_top_cs)
  } else if(input$cors_opts=="s2s"){
    SETTS_ss_cs<-SETTS_ss_cs %>%
      rename('State'='NAME') #%>%
    #filter(rank <= input$n_top_cs)
    
  }
  
  
  
  
  SETTS_tbl_cs=datatable(SETTS_ss_cs,
                         filter = list(position = 'top', clear = FALSE),
                         extensions = 'Buttons',
                         options = list(lengthMenu=c(5,10,100),
                                        pageLength=10, 
                                        scrollX = TRUE,
                                        dom = 'lftp'#, 
                                        # buttons = list(list(extend = "copy", 
                                        #                     text = "Copy Table", 
                                        #                     exportOptions = list(
                                        #                       modifier = list(page = "all")
                                        #                     )),
                                        #                list(extend = "excel", 
                                        #                     text = "Export to Excel", 
                                        #                     filename = "SETTS_Tool_Data",
                                        #                     exportOptions = list(
                                        #                       modifier = list(page = "all")
                                        #                     )))
                         ), 
                         rownames=FALSE,
                         escape = FALSE
  ) %>% 
    DT::formatRound(grepl("20", colnames(SETTS_ss_cs)),digits = 1,mark = ",")
  
  return(SETTS_tbl_cs)
  
})

proxy_cty2state_tbl = dataTableProxy('subsetSETTS_cs')


observeEvent(eventExpr = data_ss_click_cs(), {
  req(click_counties_cs$curr,
      input$dms_mode_opts_cs,
      input$county_opts_cs,
      input$n_top_cs,
      input$OD_opts_cs,
      input$sctg2_opts_cs, 
      input$Value_opts_cs,
      input$Scenario_opt_cs) #
  
  print("RUNNING: eventless observe")
  print("CALLING: data_ss_click_cs()")
  ln_select_cs=data_ss_click_cs()
  
  # validate(need(nrow(ln_select_cs)>0,
  #               'There is no data for the selected subset.'))
  
  if(!is.null(ln_select_cs)){
    
    if(input$Scenario_opt_cs == 'Baseline' |grepl('2022',input$Value_opts_cs)){
      names(ln_select_cs)[names(ln_select_cs)=='factor_lab']=input$Value_opts_cs}
    else{
      names(ln_select_cs)[names(ln_select_cs)=='factor_lab']=paste0(input$Value_opts_cs, input$Scenario_opt_cs)
    }
    
    SETTS_ss_cs<-ln_select_cs %>%
      st_drop_geometry()
    
    if(input$OD_opts_cs == "Both"){
      SETTS_ss_cs<-SETTS_ss_cs %>%
        left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("GEOID"))
    } else if(input$OD_opts_cs == "dms_orig"){
      SETTS_ss_cs<-SETTS_ss_cs %>%
        left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("GEOID"))
    } else if(input$OD_opts_cs == "dms_dest"){
      SETTS_ss_cs<-SETTS_ss_cs %>%
        left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("GEOID"))
    }
    
    
    
    SETTS_ss_cs<-SETTS_ss_cs %>%
      arrange(rank) %>%
      filter(rank <= input$n_top_cs) %>%
      select(contains('NAME'),starts_with('tons_'), starts_with('value_'))
    
    SETTS_ss_cs_r$SETTS_ss_cs=SETTS_ss_cs
    
    SETTS_ss_cs<-SETTS_ss_cs %>%
      mutate_at(vars(contains('tons_'),contains('value_')),~round(.,1)) %>%
      rename('Tons 2022</br>(Thousand Tons)'='tons_2022',
             'Tons 2050</br>(Thousand Tons)'='tons_2050',
             'Value 2022</br>($Million)'='value_2022',
             'Value 2050</br>($Million)'='value_2050')
    #rename_all(~str_replace_all(.,'_',' ') %>% str_to_title(.))
    SETTS_ss_cs_r$SETTS_ss_cs=SETTS_ss_cs
    replaceData(proxy_cty2state_tbl, SETTS_ss_cs, rownames = FALSE)
    
  } else {
    SETTS_ss_cs <- data.frame('EmptyData'=c(),
                              'Tons 2022</br>(Thousand Tons)'=c(),
                              'Tons 2050</br>(Thousand Tons)'=c(),
                              'Value 2022</br>($Million)'=c(),
                              'Value 2050</br>($Million)'=c())
    SETTS_ss_cs_r$SETTS_ss_cs=SETTS_ss_cs
    replaceData(proxy_cty2state_tbl, SETTS_ss_cs, rownames = FALSE)
    
  }
  
  print("END: eventless observe")
  
})


outputOptions(output, 'subsetSETTS_cs', suspendWhenHidden = FALSE)

output$download_cs <- downloadHandler(
  filename = function(){
    paste("data_export",Sys.Date(), ".csv", sep="")},
  content = function(file) {
    tbl_out=SETTS_ss_cs_r$SETTS_ss_cs %>% 
      rename()
    write.csv(tbl_out, file,row.names = F)
  })


observe({
  req(click_counties_cs$curr,input$dms_mode_opts_cs,input$county_opts_cs,input$n_top_cs,
      input$OD_opts_cs, input$sctg2_opts_cs, input$Value_opts_cs, input$Scenario_opt_cs)
  
  if(input$cors_opts == "c2c"){
    dat_in <- dat_cs
  } else if(input$cors_opts == "s2s"){
    dat_in <- dat_ss
  } else if (input$cors_opts == 'r2s'){
    if (input$county_opts_cs == 'ITTS'){
      dat_in <- dat_ss %>%
        mutate(origin = ifelse(origin %in% c("05", "12","13","21","22","28","29","45","48","51"),'ITTS',origin),
               destination = ifelse(destination %in% c("05", "12","13","21","22","28","29","45","48","51"), 'ITTS',destination))
    } else {
      dat_in <- dat_ss %>%
        mutate(origin = ifelse(origin %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"),'Southeast Region',origin),
               destination = ifelse(destination %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"),'Southeast Region',destination))
    }
  }
  
  if (input$OD_opts_cs == 'Both'){
    dat_in <- dat_in %>% filter(origin %in% input$county_opts_cs | destination %in% input$county_opts_cs)
  } else if (input$OD_opts_cs == 'dms_orig'){
    dat_in <- dat_in %>% filter(origin %in% input$county_opts_cs)
  }else if (input$OD_opts_cs == 'dms_dest'){
    dat_in <- dat_in %>% filter(destination %in% input$county_opts_cs)}
  
  #filter for mode
  if(input$dms_mode_opts_cs != "All" & nrow(dat_in) >=1) {
    dat_in = dat_in %>%
      filter(dms_mode == input$dms_mode_opts_cs)}
  #filter for commodity
  if(input$sctg2_opts_cs != "All" & nrow(dat_in) >=1) {
    dat_in = dat_in %>%
      filter(Grouped_sctg2==input$sctg2_opts_cs)}
  
  selected_value_cs = input$Value_opts_cs
  
  # if scenario applied
  if (input$Scenario_opt_cs != 'Baseline'){
    dat_in = process_scenario(dat_in,
                              input$Value_opts_cs,
                              input$Scenario_opt_cs,
                              click_counties_cs$curr,
                              c('origin', 'destination','Grouped_sctg2','dms_mode'),
                              1)
    selected_value_cs = paste0(input$Value_opts_cs,input$Scenario_opt_cs)
  }
  #str_to_title(gsub("_"," ",paste0(input$Value_opts, input$Scenario_opt)))
  
  
  output$c2s_flowDirection <- renderPlotly({
    direction_pie_graph_countyselected(dat_in,
                                       county = input$county_opts_cs,
                                       tons_value_selection = selected_value_cs,
                                       commcolors = init_commcolors,
                                       sourceName = "c2s_flowDirection")
  })
  
  output$c2s_mode <- renderPlotly({
    #browser()
    mode_pie_graph_v2(dat_in,
                   #county = input$county_opts,
                   tons_value_selection = selected_value_cs,
                   ini_modecolors = ini_modecolors2,
                   sourceName = "c2s_mode")
  })
  
  output$c2s_cf_commodity <- renderPlotly({
    tile_graph(dat_in,
               tons_value_selection = selected_value_cs,
               sourceName = "c2s_cf_commodity")
  })
  
  output$c2s_cf_topInbound <- renderPlotly({
    top_importing_all(dat_in,
                      tons_value_selection = selected_value_cs,
                      ton_color = "#66c2a5",
                      value_color = "#3288bd",
                      location = click_counties_cs$curr, 
                      sourceName = "c2s_cf_topInbound")
    
  })
  
  output$c2s_cf_topOutbound <- renderPlotly({
    top_exporting_all(dat_in,
                      tons_value_selection = selected_value_cs,
                      ton_color = "#66c2a5",
                      value_color = "#3288bd",
                      location = click_counties_cs$curr, 
                      sourceName = "c2s_cf_topOutbound")
  })
})



