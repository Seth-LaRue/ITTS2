#this intializes the county options
#updateSelectizeInput(session, 'county_opts', choices = cty_ch, selected = '48453', server = TRUE)

click_counties <- reactiveValues(curr=NULL,prev=NULL)


all_counties_centr_sel_ini=all_counties_centr %>% 
  filter(GEOID=='48453')


dat_ini <- dat %>%
  filter(origin == '48453'|destination == '48453') %>%
  mutate(dms_imp_exp = if_else(origin == '48453', destination, origin),
         GEOID = dms_imp_exp) %>% 
  group_by(dms_imp_exp, GEOID)%>%
  summarise(tons_2022 = sum(tons_2022),
            tons_2050 = sum(tons_2050),
            value_2022 = sum(value_2022),
            value_2050 = sum(value_2050)) %>%
  ungroup() %>%
  mutate(rank = rank(desc(value_2022)))

ln_select_ini <- county_selected %>%
  select(GEOID,county_lab) %>%
  inner_join(dat_ini,by = "GEOID")



#observeEvent(input$county_opts,{browser()})

observeEvent(input$county_opts, {
  req(input$county_opts)
  cnty = county_selected$GEOID[county_selected$GEOID == input$county_opts]
  
  click_counties$curr <- cnty
  
})

observeEvent(input$odmap_shape_click, {
  req(input$odmap_shape_click)
  #print(input$odmap_shape_click$id)
  
  click_counties$prev=click_counties$curr
  click_counties$curr=input$odmap_shape_click$id
  
  cnty = county_selected$county_lab[county_selected$GEOID == click_counties$curr]
  val = click_counties$curr
  
  
  
  updateSelectizeInput(session, 'county_opts', choices = cty_ch, selected = c(cnty, value = val), server = TRUE)#cbind(county_choices, value = county_choices$GEOID), selected = c(cnty, value = val), server = TRUE)
  
  
})

observeEvent(input$Value_opts, {
  req(input$county_opts, input$Value_opts, input$Scenario_opt)
  isolate({
    if (grepl('2022', input$Value_opts)) {
      updateSelectizeInput(session, 'Scenario_opt', label = 'Scenario Options', choices = c('Baseline'), selected = 'Baseline', server = FALSE)
    } else {
      # Check if the current selection is not "Baseline"
      if (input$Scenario_opt != "Baseline") {
        return(NULL)
      } else {
        # Update to the selected scenario
        updateSelectizeInput(session, 'Scenario_opt', label = 'Scenario Options', choices = c('Baseline' = 'Baseline',
                                                                                                 'Scenario 1: Respond to Heightened Supply Chain Risks' = '_s1',
                                                                                                 'Scenario 2: Leverage Multi-State Strength' = '_s2',
                                                                                                 'Scenario 3: Embrace Technology Transformations' = '_s3'),
                             selected = 'Baseline', server = FALSE)
      }
    }
  })
})


output$odmap <- renderLeaflet({
  
  pal_factor_ini <- colorQuantile(palette = "Blues",domain = ln_select_ini$value_2022,probs = seq(0, 1, .2))
  pulsecolor_ini='red'
  
  pal_factor_colors_ini <- unique(pal_factor_ini(sort(ln_select_ini$value_2022)))
  pal_factor_labs_ini <- round(quantile(round(ln_select_ini$value_2022, 1), probs = seq(0, 1, .2)), 1)
  pal_factor_labs_ini <- paste(scales::comma(lag(pal_factor_labs_ini)), scales::comma(pal_factor_labs_ini), sep = " - ")[-1]
  
  con_name_ini=county_selected$county_lab[county_selected$GEOID == '48453']
  titl_ini = paste0("Inbound & Outbound to </br>", con_name_ini, " county </br>", str_replace(str_to_title('value_2022'),'_',' '), " ($Million)")
  
  
  
  m %>%
    addPolygons(data = ln_select_ini,
                layerId = ~paste(ln_select_ini$GEOID),
                fillColor = ~pal_factor_ini(value_2022),
                stroke=TRUE,
                smoothFactor = 0.3,
                color = cty_border_color,#~hili,
                weight = 1,
                fillOpacity  = 1,
                label = ~county_lab,
                labelOptions = labelOptions(
                  style = list("front-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"),
                highlightOptions = highlightOptions(
                  weight = 3,
                  color = "cyan",
                  fillOpacity = 0.7,
                  bringToFront = TRUE)) %>% 
    addLegend(position = "bottomright",
              layerId = 'leg',
              colors = pal_factor_colors_ini,
              labels = pal_factor_labs_ini, 
              title = titl_ini) %>% 
    addPulseMarkers(
      layerId = 'pulsemarker',
      lng = all_counties_centr_sel_ini$long, lat = all_counties_centr_sel_ini$lat,
      label = con_name_ini,
      labelOptions = labelOptions(
        style = list("front-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"),
      icon = makePulseIcon(heartbeat = 1,iconSize=10,
                           color=pulsecolor_ini))#,color = 'EE4B2B'))#'#AA4A44'))
})



SETTS_ss_r <- reactiveValues(SETTS_ss=ln_select_ini %>% st_drop_geometry())



data_ss_click<- reactive({
  #req(n_lines_disp$curr)
  req(click_counties$curr)
  req(input$dms_mode_opts)
  req(input$sctg2_opts)
  req(input$Value_opts)
  req(input$Scenario_opt)
  req(input$OD_opts)
  #req(input$county_opts)
  req(input$n_top)
  
  #additional filtering can go here
  
  if(input$OD_opts != "Both"){
    # dat_temp <- dat %>%
    #   filter(!!input$OD_opts == click_counties$curr) this method didn't work unfortunately
    if(input$OD_opts == "dms_orig"){
      dat_temp <- dat %>%
        filter(origin == click_counties$curr) %>%
        mutate(GEOID = destination)
    } else {
      dat_temp <- dat %>%
        filter(destination == click_counties$curr) %>%
        mutate(GEOID = origin)
    }
    #filtering for mode
    if(input$dms_mode_opts != "All" & nrow(dat_temp) >=1) {
      dat_temp = dat_temp %>%
        filter(dms_mode == input$dms_mode_opts)}
    #filter for commodity
    if(input$sctg2_opts != "All" & nrow(dat_temp) >=1) {
      dat_temp = dat_temp %>%
        filter(Grouped_sctg2==input$sctg2_opts)}
    
    # output$c2c_flowDirection <- renderPlotly({
    #   direction_pie_graph_countyselected(dat_temp,
    #                                      county = input$county_opts,
    #                                      tons_value_selection = input$Value_opts,
    #                                      commcolors = init_commcolors,
    #                                      sourceName = "c2c_flowDirection")
    # })
    # 
    # output$c2c_mode <- renderPlotly({
    #   mode_pie_graph(dat_temp,
    #                  #county = input$county_opts,
    #                  tons_value_selection = input$Value_opts,
    #                  ini_modecolors = ini_modecolors,
    #                  sourceName = "c2c_mode")
    # })
    # 
    # output$c2c_cf_commodity <- renderPlotly({
    #   tile_graph(dat_temp,
    #              tons_value_selection = input$Value_opts,
    #              sourceName = "c2c_cf_commodity")
    # })
    # 
    # output$c2c_cf_topInbound <- renderPlotly({
    #   top_importing_county(dat_temp,
    #                        tons_value_selection = input$Value_opts,
    #                        ton_color = "#66c2a5",
    #                        value_color = "#3288bd",
    #                        sourceName = "c2c_cf_topInbound")
    #   
    # })
    # 
    # output$c2c_cf_topOutbound <- renderPlotly({
    #   top_exporting_county(dat_temp,
    #                        tons_value_selection = input$Value_opts,
    #                        ton_color = "#66c2a5",
    #                        value_color = "#3288bd",
    #                        sourceName = "c2c_cf_topOutbound")
    # })
    # 
    if(input$sctg2_opts == "All" | input$dms_mode_opts == "All"){
      
      if(input$Scenario_opt == 'Baseline' |grepl('2022',input$Value_opts)){
        
        dat_temp = dat_temp %>%
          group_by(origin, destination, GEOID)%>%
          summarise(tons_2022 = sum(tons_2022),
                    tons_2050 = sum(tons_2050),
                    value_2022 = sum(value_2022),
                    value_2050 = sum(value_2050)) %>%
          ungroup()
        selected_col = input$Value_opts
        
      } else{
        dat_temp = process_scenario(dat_temp,
                                    input$Value_opts,
                                    input$Scenario_opt,
                                    click_counties$curr,
                                    c('origin', 'destination', 'GEOID'),
                                    1)
        col_name <-  tail(names(dat_temp), 5)
        dat_temp = dat_temp %>% 
          group_by(GEOID) %>%
          summarise(
            across(col_name, sum, na.rm = TRUE)
          )
        selected_col <- paste0(input$Value_opts, input$Scenario_opt)}
    }
    else {
      
      if(input$Scenario_opt == 'Baseline' |grepl('2022',input$Value_opts)){
        dat_temp = dat_temp %>%
          select(origin, destination, GEOID, contains('tons_'),contains('value_'))
        selected_col = input$Value_opts
        
      }
      else{
        
        dat_temp = process_scenario(dat_temp,
                                    input$Value_opts,
                                    input$Scenario_opt,
                                    click_counties$curr,
                                    c('origin', 'destination', 'GEOID','Grouped_sctg2','dms_mode'),
                                    1)
        selected_col <- paste0(input$Value_opts, input$Scenario_opt)
      }
      
    }
    #this is in case someone select origin and destination for import/export
  } else {
    dat_temp <- dat %>%
      filter(origin == click_counties$curr|destination == click_counties$curr)
    #filtering for mode
    if(input$dms_mode_opts != "All" & nrow(dat_temp) >=1) {
      dat_temp = dat_temp %>%
        filter(dms_mode == input$dms_mode_opts)}
    #filter for commodity
    if(input$sctg2_opts != "All" & nrow(dat_temp) >=1) {
      dat_temp = dat_temp %>%
        filter(Grouped_sctg2==input$sctg2_opts)}
    
    #   output$c2c_flowDirection <- renderPlotly({
    #     direction_pie_graph_countyselected(dat_temp,
    #                    county = input$county_opts,
    #                    tons_value_selection = factor_lab,
    #                    commcolors = init_commcolors,
    #                    sourceName = "c2c_flowDirection")
    #   })
    # 
    #   output$c2c_mode <- renderPlotly({
    #     mode_pie_graph(dat_temp,
    #                    #county = input$county_opts,
    #                    tons_value_selection = factor_lab,
    #                    ini_modecolors = ini_modecolors,
    #                    sourceName = "c2c_mode")
    #   })
    # 
    #   output$c2c_cf_commodity <- renderPlotly({
    #     tile_graph(dat_temp,
    #                tons_value_selection = input$Value_opts,
    #                sourceName = "c2c_cf_commodity")
    #   })
    # 
    #   output$c2c_cf_topInbound <- renderPlotly({
    #     top_importing_county(dat_temp,
    #                          tons_value_selection = input$Value_opts,
    #                          ton_color = "#66c2a5",
    #                          value_color = "#3288bd",
    #                          sourceName = "c2c_cf_topInbound")
    # 
    # })
    # 
    #   output$c2c_cf_topOutbound <- renderPlotly({
    #     top_exporting_county(dat_temp,
    #                          tons_value_selection = input$Value_opts,
    #                          ton_color = "#66c2a5",
    #                          value_color = "#3288bd",
    #                          sourceName = "c2c_cf_topOutbound")
    #   })
    
    if(input$Scenario_opt == 'Baseline' |grepl('2022',input$Value_opts)){
      
      dat_temp = dat_temp %>%
        mutate(dms_imp_exp = ifelse(origin == click_counties$curr, destination, origin),
               GEOID = dms_imp_exp) %>% #you actually don't need this could just group by lineid, but then the map is missing
        group_by(dms_imp_exp, GEOID)%>%
        summarise( tons_2022 = sum(tons_2022),
                   tons_2050 = sum(tons_2050),
                   value_2022 = sum(value_2022),
                   value_2050 = sum(value_2050)) %>%
        ungroup()
      selected_col = input$Value_opts
      
    }else{
      
      dat_temp_input = dat_temp
      dat_temp = process_scenario(dat_temp_input,
                                  input$Value_opts,
                                  input$Scenario_opt,
                                  click_counties$curr,
                                  col_list = c('dms_imp_exp', 'GEOID'),
                                  0)
      
      selected_col <- paste0(input$Value_opts, input$Scenario_opt)
    }
  }
  
  
  
  dat_temp = dat_temp %>%
    rename(factor_lab = selected_col) %>%
    mutate(rank = rank(desc(factor_lab))) 
  
  if(nrow(dat_temp)>0){
    ln_select <- county_selected %>%
      select(GEOID,county_lab) %>%
      inner_join(dat_temp,by = "GEOID") %>%
      mutate(tranp=ifelse(rank <= input$n_top, 1,.25))
  } else {ln_select=NULL}
  
  return(ln_select)
})

map_update <- reactive({
  #req(n_lines_disp$curr)
  req(click_counties$curr)
  req(input$dms_mode_opts)
  req(input$sctg2_opts)
  req(input$Value_opts)
  req(input$Scenario_opt)
  req(input$OD_opts)
  req(input$n_top)
  paste(click_counties$curr,input$dms_mode_opts,input$sctg2_opts,input$Value_opts,input$Scenario_opt, input$OD_opts, input$n_top) ## we will have to add complete list of filters eventually
})

observeEvent(eventExpr = map_update(), {
  req(click_counties$curr,input$dms_mode_opts,
      input$sctg2_opts, input$Value_opts,input$Scenario_opt, input$n_top)
  show_waiter_message()
  
  #do we have to use the entire line file to remove?
  leafletProxy(mapId = "odmap",session = session) %>%
    removeShape(layerId = paste(county_selected$GEOID)) %>%
    removeShape(layerId = paste("data",county_selected$GEOID)) %>%
    #removeShape(layerId = 'leg') %>% 
    clearControls() %>%
    removeShape(layerId = 'pulsemarker')
  
  ln_select=data_ss_click()
  
  
  
  con_name = county_selected$county_lab[county_selected$GEOID == click_counties$curr]
  
  if(input$OD_opts == "Both"){
    dir = "Inbound & Outbound to "
  }else if(input$OD_opts == "dms_orig"){
    dir = "Outbound from "
  } else {
    dir = "Inbound to "
  }
  
  if(!is.null(ln_select)) {
    
    #lines_labs <- paste0(input$Value_opts,': ',round(ln_select$rank,6)) %>% lapply(htmltools::HTML)
    
    ln_select <- ln_select %>% 
      arrange(-rank)
    
    if(grepl('tons',input$Value_opts)){
      titl = paste0(dir, con_name, "</br>", str_replace(str_to_title(input$Value_opts,),'_',' '), " (Thousand tons)")
      pal_factor <- colorQuantile(
        palette = "YlOrRd",
        domain = ln_select$factor_lab[!duplicated(ln_select$factor_lab)],
        probs = seq(0, 1, .2)
      )
      pulsecolor='blue'
    } else {
      titl = paste0(dir, con_name, "</br>", str_replace(str_to_title(input$Value_opts,),'_',' '), " ($Million)")
      pal_factor <- colorQuantile(
        palette = "Blues",
        domain = ln_select$factor_lab[!duplicated(ln_select$factor_lab)],
        probs = seq(0, 1, .2)
      )
      pulsecolor='red'
    }
    
    all_counties_centr_sel=all_counties_centr %>% 
      filter(GEOID==click_counties$curr)
    if(nrow(ln_select)== 1){
      pal_factor_labs = round(ln_select$factor_lab, 1)
      if(grepl('tons',input$Value_opts)){
        pal_factor = function(nada){return("#bd0026")}
        pal_factor_colors = "#bd0026"
      } else {
        pal_factor = function(nada){return('#08306b')}
        pal_factor_colors = '#08306b'
      }
    } else {
      pal_factor_colors <- unique(pal_factor(sort(ln_select$factor_lab[!duplicated(ln_select$factor_lab)])))
    }
    
    steps = 1/length(pal_factor_colors)
    pal_factor_labs <- round(quantile(round(ln_select$factor_lab[!duplicated(ln_select$factor_lab)], 1), probs = seq(0, 1, steps)), 1)
    pal_factor_labs <- paste(scales::comma(lag(pal_factor_labs)), scales::comma(pal_factor_labs), sep = " - ")[-1]
    
    #ctysel_labels <- ln_select$county_lab %>% lapply(htmltools::HTML)
    
    
    
    leafletProxy(mapId = "odmap",session = session) %>%
      addPolygons(data = ln_select,
                  layerId = ~paste("data",ln_select$GEOID),
                  fillColor = ~pal_factor(factor_lab),
                  stroke=TRUE,
                  smoothFactor = 0.3,
                  color = cty_border_color,#~hili,
                  weight = 1,#~wgt,
                  #opacity  = .85,#~rank/max(rank),
                  fillOpacity  = ~tranp,#~rank/max(rank),
                  #popup = lines_labs,
                  label = ~county_lab,
                  labelOptions = labelOptions(
                    style = list("front-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
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
        label = con_name,
        labelOptions = labelOptions(
          style = list("front-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        icon = makePulseIcon(heartbeat = 1,iconSize=10,
                             color=pulsecolor))#,color = 'EE4B2B'))#'#AA4A44'))
    
  } else {
    if(grepl('tons',input$Value_opts)){
      titl = paste0(dir, con_name, "</br>", str_replace(str_to_title(input$Value_opts,),'_',' '), " (Thousand tons)")
      pulsecolor='blue'
    } else {
      titl = paste0(dir, con_name, "</br>", str_replace(str_to_title(input$Value_opts,),'_',' '), " ($Million)")
      pulsecolor='red'
    }
    
    all_counties_centr_sel=all_counties_centr %>% 
      filter(GEOID==click_counties$curr)
    
    leafletProxy(mapId = "odmap",session = session) %>%
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
      addPulseMarkers(
        layerId = 'pulsemarker',
        lng = all_counties_centr_sel$long, lat = all_counties_centr_sel$lat,
        label = con_name,
        labelOptions = labelOptions(
          style = list("front-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        icon = makePulseIcon(heartbeat = 1,iconSize=10,
                             color=pulsecolor))
  }
  onFlushed(function() {
    flush_waiter_message()
  })
})

outputOptions(output, 'odmap', suspendWhenHidden = FALSE)

output$table_title <- renderText({
  req(click_counties$curr)
  
  if(input$OD_opts != "Both"){
    if(input$OD_opts == "dms_orig"){
      title = paste0("Origin County: ", county_selected$county_lab[county_selected$GEOID == click_counties$curr])
      
    } else {
      title = paste0("Destination County: ", county_selected$county_lab[county_selected$GEOID == click_counties$curr])
    }
  } else {
    title = paste0("Selected County: ", county_selected$county_lab[county_selected$GEOID == click_counties$curr])
  }
  
  return(title)
})

output$scenario_title <- renderText({
  
  if (input$Scenario_opt == '_s1'){
    sencario = paste0("Selected Scenario: ", 'Scenario 1- Respond to Heightened Supply Chain Risks')
  }else if (input$Scenario_opt == '_s2'){
    sencario = paste0("Selected Scenario: ", 'Scenario 2- Leverage Multi-State Strength')
  }else if (input$Scenario_opt == '_s3'){
    sencario = paste0("Selected Scenario: ", 'Scenario 3- Embrace Technology Transformations')
  }else{
    sencario = paste0("Selected Scenario: ", input$Scenario_opt)}
  return(sencario)
})

output$scenario_text_output <- renderText({
  req(input$Scenario_opt)
  
  if(input$Scenario_opt == '_s1'){
    return('In Scenario 1, due to the resilience and readiness of the region’s supply chains, the Southeast
           region increases its production of consumer goods and agricultural goods over time. The Southeast
           grows its production of consumer goods at a 2.9% annual rate. For agriculture, the tonnage of 
           these goods originating in the Southeast will increase by about 1.3% annually.')
  } else if (input$Scenario_opt == '_s2'){
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
  }else if (input$Scenario_opt == '_s3'){
    return('As the U.S. reduces its use of coal and gasoline and embraces alternative energy, energy production 
    technologies are rapidly evolving, impacting the movement of various commodities. Comparable technology 
    transformations in agriculture and other resource industries may occur, and distributed manufacturing (3-D 
    printing) may “flatten” or simplify the traditional three-tiered (resources, intermediate products, final 
    products) supply chain. In Scenario 3, coal shipments in the Southeast are projected to decline by approximately
    -4.5% annually until they vanish from the network. And high-tech durable manufacturing goods will grow by 
    2.8% annually through 2050.')
  }
  
})



output$subsetSETTS<-renderDataTable({#server = FALSE,{
  
  
  if(input$Scenario_opt == 'Baseline' &
     grepl('2022',input$Value_opts) &
     input$dms_mode_opts == 'All' &
     input$sctg2_opts == 'All' &
     input$OD_opts == 'Both'){
    ln_select=ln_select_ini
    names(ln_select)[names(ln_select)=='factor_lab']=input$Value_opts}
  else if(input$Scenario_opt == 'Baseline' ){
    ln_select=data_ss_click()
    names(ln_select)[names(ln_select)=='factor_lab']=input$Value_opts
  }else{
    ln_select=data_ss_click()
    names(ln_select)[names(ln_select)=='factor_lab']=paste0(input$Value_opts, input$Scenario_opt)
  }
  
  
  SETTS_ss<-ln_select %>% 
    st_drop_geometry() 
  
  if(input$OD_opts == "Both"){
    SETTS_ss<-SETTS_ss %>% 
      left_join(st_drop_geometry(select(county_selected, GEOID)), by = c("dms_imp_exp" = "GEOID"))
  } else if(input$OD_opts == "dms_orig"){
    SETTS_ss<-SETTS_ss %>%
      left_join(st_drop_geometry(select(county_selected, GEOID)), by = c("destination" = "GEOID"))
  } else if(input$OD_opts == "dms_dest"){
    SETTS_ss<-SETTS_ss %>%
      left_join(st_drop_geometry(select(county_selected, GEOID)), by = c("origin" = "GEOID"))
  }
  
  SETTS_ss<-SETTS_ss %>%
    arrange(rank) %>% 
    select(contains('county'),starts_with('tons_'), starts_with('value_')) %>% 
    rename('County' = 'county_lab')
  
  SETTS_ss_r$SETTS_ss=SETTS_ss
  
  SETTS_ss<-SETTS_ss %>%
    mutate_at(vars(contains('tons_'),contains('value_')),~round(.,1)) %>% 
    rename( 'Tons 2022</br>(Thousand Tons)'='tons_2022',
            'Tons 2050</br>(Thousand Tons)'='tons_2050',
            'Value 2022</br>($Million)'='value_2022',
            'Value 2050</br>($Million)'='value_2050')
  
   names(SETTS_ss)[grepl('_',names(SETTS_ss))] <- str_to_title(gsub("_"," ",names(SETTS_ss)[grepl('_',names(SETTS_ss))])) 
  #rename_all(~str_replace_all(.,'_',' ') %>% str_to_title(.)) 

  #SETTS_ss<-SETTS_ss %>%
  #rename('Partner'='County Lab')
  
  SETTS_tbl=datatable(SETTS_ss,
                      filter = list(position = 'top', clear = FALSE),
                      extensions = 'Buttons',
                      options = list(lengthMenu=c(5,10,100),
                                     pageLength=10, 
                                     scrollX = TRUE,
                                     dom = 'lftp'#, 
                                     #buttons = list(list(extend = "copy", 
                                     #      text = "Copy Table", 
                                     #      exportOptions = list(
                                     #        modifier = list(page = "all")
                                     #      )),
                                     # list(extend = "excel", 
                                     #      text = "Export to Excel", 
                                     #      filename = "SETTS_Tool_Data",
                                     #             exportOptions = list(
                                     #               modifier = list(page = "all")
                                     #             )))
                      ), 
                      rownames=FALSE,
                      escape = FALSE
  ) %>% 
    DT::formatRound(grepl("20", colnames(SETTS_ss)),digits = 1,mark = ",")
  
  return(SETTS_tbl)
  
})




proxy_cty2cty_tbl = dataTableProxy('subsetSETTS')


observe({
  
  req(click_counties$curr,input$dms_mode_opts,input$county_opts,input$n_top,
      input$OD_opts, input$sctg2_opts, input$Value_opts, input$Scenario_opt)
  ln_select=data_ss_click()
  
  # validate(need(!is.null(ln_select),
  #               'There is no data for the selected subset.'),
  #          need(nrow(ln_select)>0,
  #               'There is no data for the selected subset.'))
  if(!is.null(ln_select)){
    
    if(input$Scenario_opt == 'Baseline' |grepl('2022',input$Value_opts)){
      names(ln_select)[names(ln_select)=='factor_lab']=input$Value_opts}
    else{
      names(ln_select)[names(ln_select)=='factor_lab']=str_to_title(gsub("_"," ",paste0(input$Value_opts, input$Scenario_opt)))
    }
    
    
    SETTS_ss<-ln_select %>% 
      st_drop_geometry() 
    
    if(input$OD_opts == "Both"){
      SETTS_ss<-SETTS_ss %>% 
        left_join(st_drop_geometry(select(county_selected, GEOID)), by = c("dms_imp_exp" = "GEOID"))
    } else if(input$OD_opts == "dms_orig"){
      SETTS_ss<-SETTS_ss %>%
        left_join(st_drop_geometry(select(county_selected, GEOID)), by = c("destination" = "GEOID"))
    } else if(input$OD_opts == "dms_dest"){
      SETTS_ss<-SETTS_ss %>%
        left_join(st_drop_geometry(select(county_selected, GEOID)), by = c("origin" = "GEOID"))
    }
    
    SETTS_ss<-SETTS_ss %>%
      arrange(rank) %>% 
      filter(rank <= input$n_top) %>% 
      select(contains('county'),starts_with('tons_'), starts_with('value_')) %>% 
      rename('County' = 'county_lab')
    
    SETTS_ss<-SETTS_ss %>%
      mutate_at(vars(contains('tons_'),contains('value_')),~round(.,1)) %>% 
      rename('Tons 2022</br>(Thousand Tons)'='tons_2022',
             'Tons 2050</br>(Thousand Tons)'='tons_2050',
             'Value 2022</br>($Million)'='value_2022',
             'Value 2050</br>($Million)'='value_2050')
    
    SETTS_ss <- SETTS_ss %>% rename_all(~str_replace_all(.,'_',' ') %>% str_to_title(.)) 
    
    
    SETTS_ss_r$SETTS_ss=SETTS_ss
    
    replaceData(proxy_cty2cty_tbl, SETTS_ss, rownames = FALSE)} else {
      SETTS_ss <- data.frame('County'=c(),
                             'Tons 2022</br>(Thousand Tons)'=c(),
                             'Tons 2050</br>(Thousand Tons)'=c(),
                             'Value 2022</br>($Million)'=c(),
                             'Value 2050</br>($Million)'=c())
      
      replaceData(proxy_cty2cty_tbl, SETTS_ss, rownames = FALSE)
    }
  
  
})

outputOptions(output, 'subsetSETTS', suspendWhenHidden = FALSE)  

output$download_cc <- downloadHandler(
  filename = function(){
    paste("data_export",Sys.Date(), ".csv", sep="")},
  content = function(file) {
    tbl_out=SETTS_ss_r$SETTS_ss %>% 
      rename()
    write.csv(tbl_out, file,row.names = F)
  })


observe({
  req(click_counties$curr,input$dms_mode_opts,input$county_opts,input$n_top,
      input$OD_opts, input$sctg2_opts, input$Value_opts, input$Scenario_opt)
  print("Observe: cnty2cnty")
  #browser()
  if (input$OD_opts == 'Both'){
    dat_in <- dat %>% filter(origin %in% input$county_opts | destination %in% input$county_opts)
  } else if (input$OD_opts == 'dms_orig'){
    dat_in <- dat %>% filter(origin %in% input$county_opts)
  }else if (input$OD_opts == 'dms_dest'){
    dat_in <- dat %>% filter(destination %in% input$county_opts)}
  
  #filter for mode
  if(input$dms_mode_opts != "All" & nrow(dat_in) >=1) {
    dat_in = dat_in %>%
      filter(dms_mode == input$dms_mode_opts)}
  #filter for commodity
  if(input$sctg2_opts != "All" & nrow(dat_in) >=1) {
    dat_in = dat_in %>%
      filter(Grouped_sctg2==input$sctg2_opts)}
  
  selected_value = input$Value_opts
  
  # if scenario applied
  if (input$Scenario_opt != 'Baseline'){
    dat_in = process_scenario(dat_in,
                              input$Value_opts,
                              input$Scenario_opt,
                              click_counties$curr,
                              c('origin', 'destination','Grouped_sctg2','dms_mode'),
                              1)
    selected_value = paste0(input$Value_opts,input$Scenario_opt)
  }
  #str_to_title(gsub("_"," ",paste0(input$Value_opts, input$Scenario_opt)))
  
  
  output$c2c_flowDirection <- renderPlotly({
    direction_pie_graph_countyselected(dat_in,
                                       county = input$county_opts,
                                       tons_value_selection = selected_value,
                                       commcolors = init_commcolors,
                                       sourceName = "c2c_flowDirection")
  })
  
  output$c2c_mode <- renderPlotly({
    mode_pie_graph_v2(dat_in,
                   #county = input$county_opts,
                   tons_value_selection = selected_value,
                   ini_modecolors = ini_modecolors,
                   sourceName = "c2c_mode")
  })
  
  output$c2c_cf_commodity <- renderPlotly({
    tile_graph(dat_in,
               tons_value_selection = selected_value,
               sourceName = "c2c_cf_commodity")
  })
  
  output$c2c_cf_topInbound <- renderPlotly({
    top_importing_all(dat_in,
                      tons_value_selection = selected_value,
                      ton_color = "#66c2a5",
                      value_color = "#3288bd",
                      location = click_counties$curr,
                      sourceName = "c2c_cf_topInbound")
    
  })
  
  output$c2c_cf_topOutbound <- renderPlotly({
    top_exporting_all(dat_in,
                      tons_value_selection = selected_value,
                      ton_color = "#66c2a5",
                      value_color = "#3288bd",
                      location = click_counties$curr,
                      sourceName = "c2c_cf_topOutbound")
  })
})

