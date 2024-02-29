click_counties_in <- reactiveValues(curr=NULL,prev=NULL)

#initialization---- 
#make a version for ports and internationals
all_states_centr_sel_ini_in=all_states_centr %>% 
  filter(GEOID=='48')

port_pulse_pini=ports_base %>% 
  filter(GEOID=='4026') %>%
  mutate(long = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])
# all_state_centr_sel_ini_in=all_state_centr %>% 
#   filter(GEOID=='48453')
pulse_name_pini = ports_base$NAME[ports_base$GEOID == '4026']

dat_ini_in <- dat_sin %>%
  filter(origin == '48'|destination == '48') %>%
  mutate(dms_imp_exp = if_else(origin == '48', destination, origin),
         GEOID = dms_imp_exp) %>% 
  group_by(dms_imp_exp, GEOID)%>% 
  summarise(tons_2019 = sum(tons_2019), 
            tons_2021 = sum(tons_2021),
            value_2019 = sum(value_2019),
            value_2021 = sum(value_2021)) %>%
  ungroup() %>%
  mutate(rank = rank(desc(value_2019)))

dat_pini_in <- dat_pin %>%
  filter(origin == '4026'|destination == '4026') %>%
  mutate(dms_imp_exp = if_else(origin == '4026', destination, origin),
         GEOID = dms_imp_exp) %>% 
  group_by(dms_imp_exp, GEOID)%>% 
  summarise(tons_2019 = sum(tons_2019), 
            tons_2021 = sum(tons_2021),
            value_2019 = sum(value_2019),
            value_2021 = sum(value_2021)) %>%
  ungroup() %>%
  mutate(rank = rank(desc(value_2019)))

ln_select_in_ini <- international_base %>%
  select(GEOID, NAME) %>%
  inner_join(dat_ini_in,by = "GEOID")

ln_select_in_pini <- international_base %>%
  select(GEOID, NAME) %>%
  inner_join(dat_pini_in, by = "GEOID")

output$odmap_in <- renderLeaflet({
  pal_factor_ini_in <- colorQuantile(palette = "Blues",domain = ln_select_in_ini$value_2019,probs = seq(0, 1, .2))
  pulsecolor_ini_in='red'
  
  pal_factor_colors_ini_in <- unique(pal_factor_ini_in(sort(ln_select_in_ini$value_2019)))
  pal_factor_labs_ini_in <- round(quantile(round(ln_select_in_ini$value_2019, 1), probs = seq(0, 1, .2)), 1)
  pal_factor_labs_ini_in <- paste(scales::comma(lag(pal_factor_labs_ini_in)), scales::comma(pal_factor_labs_ini_in), sep = " - ")[-1]
  
  con_name_ini_in=all_selected$NAME[all_selected$GEOID == '48']
  titl_ini_in = paste0("Inbound & Outbound to </br>", con_name_ini_in, "</br>", str_replace(str_to_title('Value_2019'),'_',' '), " (Thousand tons)")
  
  m_in %>%
    addPolygons(data = ln_select_in_ini,
                layerId = ~paste("data", ln_select_in_ini$GEOID),
                fillColor = ~pal_factor_ini_in(value_2019),
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
              colors = pal_factor_colors_ini_in,
              labels = pal_factor_labs_ini_in,
              title = titl_ini_in) %>%
    addPulseMarkers(
      layerId = 'pulsemarker',
      lng = all_states_centr_sel_ini_in$long, lat = all_states_centr_sel_ini_in$lat,
      label = con_name_ini_in,
      labelOptions = labelOptions(
        style = list("front-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"),
      icon = makePulseIcon(heartbeat = 1,iconSize=10,
                           color=pulsecolor_ini_in))
})

#port to state selection----
observeEvent(input$cors_opts_in, {
  
  req(input$cors_opts_in)
  
  if(input$cors_opts_in=="p2n"){
    updateSelectizeInput(session, 'county_opts_in', label = "Port", choices = port_ch, selected = c("Dallas-Fort Worth Airport", value = "4026"), server = TRUE)
    updateSelectInput(session, 'dms_mode_opts_in',label = 'Mode Type',selected = "All", choices =c("All" = "All",modes_int))
    click_counties_in$curr <- "4026"
    
    output$odmap_in <- renderLeaflet({
      pal_factor_pini_in <- colorQuantile(palette = "Blues",domain = ln_select_in_pini$value_2019,probs = seq(0, 1, .2))
      pulsecolor_pini_in='red'
      
      pal_factor_colors_pini_in <- unique(pal_factor_pini_in(sort(ln_select_in_pini$value_2019)))
      pal_factor_labs_pini_in <- round(quantile(round(ln_select_in_pini$value_2019, 1), probs = seq(0, 1, .2)), 1)
      pal_factor_labs_pini_in <- paste(scales::comma(lag(pal_factor_labs_pini_in)), scales::comma(pal_factor_labs_pini_in), sep = " - ")[-1]
      
      con_name_pini_in=all_selected$NAME[all_selected$GEOID == '4026']
      titl_pini_in = paste0("Inbound & Outbound to </br>", con_name_pini_in, "</br>", str_replace(str_to_title('Value_2019'),'_',' '), " (Thousand tons)")
      
      m_pin %>%
        addPolygons(data = ln_select_in_pini,
                    layerId = ~paste("data", ln_select_in_pini$GEOID),
                    fillColor = ~pal_factor_pini_in(value_2019),
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
        addMarkers(data = ports_base, 
                   layerId = ~GEOID,
                   label = ~NAME,
                   labelOptions = labelOptions(
                     style = list("front-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto"),
                   icon = ~map_icons[type]
        ) %>%
        addLegend(position = "bottomright",
                  layerId = 'leg',
                  colors = pal_factor_colors_pini_in,
                  labels = pal_factor_labs_pini_in, 
                  title = titl_pini_in) %>%
        addPulseMarkers(
          layerId = 'pulsemarker',
          lng = port_pulse_pini$long, lat = port_pulse_pini$lat,
          label = pulse_name_pini,
          labelOptions = labelOptions(
            style = list("front-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"),
          icon = makePulseIcon(heartbeat = 1,iconSize=10,
                               color=pulsecolor_pini_in))
    })
  } else if(input$cors_opts_in=="s2n"){
    updateSelectizeInput(session, 'county_opts_in', label = "State", choices = state_ch, selected = c("Texas", value = "48"), server = TRUE)
    updateSelectInput(session, 'dms_mode_opts_in',label = 'Mode Type',selected = "All", choices =c("All" = "All",modes_int))
    click_counties_in$curr <- "48"
    
    output$odmap_in <- renderLeaflet({
      pal_factor_ini_in <- colorQuantile(palette = "Blues",domain = ln_select_in_ini$value_2019,probs = seq(0, 1, .2))
      pulsecolor_ini_in='red'
      
      pal_factor_colors_ini_in <- unique(pal_factor_ini_in(sort(ln_select_in_ini$value_2019)))
      pal_factor_labs_ini_in <- round(quantile(round(ln_select_in_ini$value_2019, 1), probs = seq(0, 1, .2)), 1)
      pal_factor_labs_ini_in <- paste(scales::comma(lag(pal_factor_labs_ini_in)), scales::comma(pal_factor_labs_ini_in), sep = " - ")[-1]
      
      con_name_ini_in=all_selected$NAME[all_selected$GEOID == '48']
      titl_ini_in = paste0("Inbound & Outbound to </br>", con_name_ini_in, "</br>", str_replace(str_to_title('Value_2019'),'_',' '), " (Thousand tons)")
      
      m_in %>%
        addPolygons(data = ln_select_in_ini,
                    layerId = ~paste("data", ln_select_in_ini$GEOID),
                    fillColor = ~pal_factor_ini_in(value_2019),
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
                  colors = pal_factor_colors_ini_in,
                  labels = pal_factor_labs_ini_in,
                  title = titl_ini_in) %>%
        addPulseMarkers(
          layerId = 'pulsemarker',
          lng = all_states_centr_sel_ini_in$long, lat = all_states_centr_sel_ini_in$lat,
          label = con_name_ini_in,
          labelOptions = labelOptions(
            style = list("front-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"),
          icon = makePulseIcon(heartbeat = 1,iconSize=10,
                               color=pulsecolor_ini_in))
    })
  }
  
})

observeEvent(input$county_opts_in, {
  req(input$county_opts_in)
  cnty_in = all_selected$GEOID[all_selected$GEOID == input$county_opts_in]
  click_counties_in$curr <- cnty_in
})

observeEvent(input$dms_mode_opts_in, {

  if(input$cors_opts_in == "p2n"&input$dms_mode_opts_in != "All"){
    pb_temp<- ports_base[ports_base$mode_nm == input$dms_mode_opts_in,]
    port_ch_tmp = pb_temp$GEOID
    names(port_ch_tmp) = pb_temp$NAME
    if(!(input$county_opts_in %in% pb_temp$GEOID)){
      p_val <- int_ports_mode_no_selelect$use[int_ports_mode_no_selelect$mode == input$dms_mode_opts_in]
      p_nm <- pb_temp$NAME[pb_temp$GEOID == p_val]
      updateSelectizeInput(session, 'county_opts_in', choices = port_ch_tmp, selected = c(p_nm, value = p_val), server = TRUE)
    } else {
      updateSelectizeInput(session, 'county_opts_in', choices = port_ch_tmp, server = TRUE)
    }
  }

})

observeEvent(input$odmap_in_shape_click, {
  req(input$odmap_in_shape_click)
  if(!is.null(input$odmap_in_shape_click$id)){
    if(input$odmap_in_shape_click$id %in% all_selected$GEOID){
      sel = all_selected$NAME[all_selected$GEOID == input$odmap_in_shape_click$id]
      if(input$cors_opts_in == "p2n"&input$dms_mode_opts_in != "All"){
        pb_temp<- ports_base[ports_base$mode_nm == input$dms_mode_opts_in,]
        port_ch_tmp = pb_temp$GEOID
        names(port_ch_tmp) = pb_temp$NAME
        if(!(input$county_opts_in %in% pb_temp$GEOID)){
          p_nm <- pb_temp$NAME[1]
          p_val <- pb_temp$GEOID[1]
          updateSelectizeInput(session, 'county_opts_in', choices = port_ch_tmp, selected = c(p_nm, value = p_val), server = TRUE)
        } else {
          updateSelectizeInput(session, 'county_opts_in', choices = port_ch_tmp, server = TRUE)
        }
      } else if(input$cors_opts_in=="s2n"){
        updateSelectizeInput(session, 'county_opts_in', choices = state_ch, selected = c(sel, value = input$odmap_in_shape_click$id), server = TRUE)
      }
      click_counties_in$prev=click_counties_in$curr
      click_counties_in$curr=input$odmap_in_shape_click$id
    }}
}) 

observeEvent(input$odmap_in_marker_click, {
  req(input$odmap_in_marker_click)
  if(!is.null(input$odmap_in_marker_click$id)){
    if(input$odmap_in_marker_click$id %in% all_selected$GEOID){
      sel = all_selected$NAME[all_selected$GEOID == input$odmap_in_marker_click$id]
      if(input$cors_opts_in == "p2n"){
        if(input$dms_mode_opts_in != "All"){
        pb_temp<- ports_base[ports_base$mode_nm == input$dms_mode_opts_in,]
        port_ch_tmp = pb_temp$GEOID
        names(port_ch_tmp) = pb_temp$NAME
        updateSelectizeInput(session, 'county_opts_in', choices = port_ch_tmp, selected = c(sel, value = input$odmap_in_marker_click$id), server = TRUE)}
        else {
          updateSelectizeInput(session, 'county_opts_in', choices = port_ch, selected = c(sel, value = input$odmap_in_marker_click$id), server = TRUE)}
        click_counties_in$prev=click_counties_in$curr
        click_counties_in$curr=input$odmap_in_marker_click$id
      } else if(input$cors_opts_in=="s2n"){
        updateSelectizeInput(session, 'county_opts_in', choices = state_ch, selected = c(sel, value = input$odmap_in_marker_click$id), server = TRUE)
        click_counties_in$prev=click_counties_in$curr
        click_counties_in$curr=input$odmap_in_shape_click$id
      }

    }}
}) 

observeEvent(input$Value_opts_in,{
  if(grepl('2019',input$Value_opts_in)){
    updateSelectizeInput(session, 'Scenario_opt_in', label = 'Scenario Options', choices = c('Baseline'), selected = 'Baseline',server = TRUE)
  } else if (grepl(c('2021'),input$Value_opts_in)) {
    updateSelectizeInput(session, 'Scenario_opt_in', label = 'Scenario Options', choices = c('Baseline',
                                                                                             'Scenario 1' = '_s1',
                                                                                             'Scenario 2' = '_s2',
                                                                                             'Scenario 3' = '_s3',
                                                                                             'Scenario 4' = '_s4',
                                                                                             'Scenario 5' = '_s5',
                                                                                             'Scenario 6' = '_s6'),
                         selected = 'Baseline',server = TRUE)
  }
})

SETTS_ss_in_r <- reactiveValues(SETTS_ss_in=ln_select_in_ini %>% st_drop_geometry())


data_ss_click_in<- reactive({
  #req(n_lines_disp$curr)
  req(click_counties_in$curr)
  req(input$dms_mode_opts_in)
  req(input$sctg2_opts_in)
  req(input$Value_opts_in)
  req(input$Scenario_opt_in)
  req(input$OD_opts_in)
  req(input$county_opts_in)
  req(input$n_top_in)
  req(input$cors_opts_in)
  #additional filtering can go here
  if(input$cors_opts_in == "p2n"){
    dat_temp_in <- dat_pin
  } else if(input$cors_opts_in == "s2n"){
    dat_temp_in <- dat_sin
  }
  
  if(input$OD_opts_in != "Both"){
    if(input$OD_opts_in == "dms_orig"){
      dat_temp_in <- dat_temp_in %>%
        filter(origin == click_counties_in$curr) %>%
        mutate(GEOID = destination)
    } else {
      dat_temp_in <- dat_temp_in %>%
        filter(destination == click_counties_in$curr) %>%
        mutate(GEOID = origin)
    }
    #filtering for mode
    if(input$dms_mode_opts_in != "All" & nrow(dat_temp_in) >=1) {
     dat_temp_in = dat_temp_in %>%
       filter(dms_mode == input$dms_mode_opts_in)
     
     }
    #filter for commodity
    if(input$sctg2_opts_in != "All" & nrow(dat_temp_in) >=1) {
      dat_temp_in = dat_temp_in %>%
        filter(Grouped_sctg2==input$sctg2_opts_in)}
    
    if(input$sctg2_opts_in == "All"| input$dms_mode_opts_in == "All"){
      if(input$Scenario_opt_in == 'Baseline' |grepl('2019',input$Value_opts_in)){
        
      dat_temp_in = dat_temp_in %>%
        group_by(origin, destination, GEOID)%>%
        summarise(tons_2019 = sum(tons_2019), 
                  tons_2021 = sum(tons_2021),
                  value_2019 = sum(value_2019),
                  value_2021 = sum(value_2021)) %>%
        ungroup()
      selected_col = input$Value_opts_in
      }else{
        dat_temp_in = process_scenario_in(dat_temp_in,
                                       input$Value_opts_in,
                                       input$Scenario_opt_in,
                                       click_counties_in$curr,
                                       c('origin', 'destination', 'GEOID'),
                                       1)
        selected_col <- paste0(input$Value_opts_in, input$Scenario_opt_in)}
    } 
    
    else {
      if(input$Scenario_opt_in == 'Baseline' |grepl('2019',input$Value_opts_in)){
        dat_temp_in = dat_temp_in %>%
          select(origin, destination, GEOID, contains('tons_'),contains('value_'))
        selected_col = input$Value_opts_in
        
      }
      else{
        dat_temp_in = process_scenario_in(dat_temp_in,
                                       input$Value_opts_in,
                                       input$Scenario_opt_in,
                                       click_counties_in$curr,
                                       c('origin', 'destination', 'GEOID','Grouped_sctg2','dms_mode'),
                                       1)
        selected_col <- paste0(input$Value_opts_in, input$Scenario_opt_in)
      }
      
    }
    #this is in case someone select origin and destination for import/export
  } else {
    dat_temp_in <- dat_temp_in %>%
      filter(origin == click_counties_in$curr|destination == click_counties_in$curr)
    #filtering for mode
    if(input$dms_mode_opts_in != "All" & nrow(dat_temp_in) >=1) {
      dat_temp_in = dat_temp_in %>%
        filter(dms_mode == input$dms_mode_opts_in)}
    #filter for commodity
    if(input$sctg2_opts_in != "All" & nrow(dat_temp_in) >=1) {
      dat_temp_in = dat_temp_in %>%
        filter(Grouped_sctg2==input$sctg2_opts_in)}
    
    if(input$Scenario_opt_in == 'Baseline' |grepl('2019',input$Value_opts_in)){
    dat_temp_in = dat_temp_in %>%
      mutate(dms_imp_exp = ifelse(origin == click_counties_in$curr, destination, origin),
             GEOID = dms_imp_exp) %>% 
      group_by(dms_imp_exp, GEOID)%>%
      summarise(tons_2019 = sum(tons_2019), 
                tons_2021 = sum(tons_2021),
                value_2019 = sum(value_2019),
                value_2021 = sum(value_2021)) %>%
      ungroup()
    selected_col = input$Value_opts_in
    
    } else{
      dat_temp_in_input = dat_temp_in
      dat_temp_in = process_scenario_in(dat_temp_in_input,
                                     input$Value_opts_in,
                                     input$Scenario_opt_in,
                                     click_counties_in$curr,
                                     col_list = c('dms_imp_exp', 'GEOID'),
                                     0)
      
      selected_col <- paste0(input$Value_opts_in, input$Scenario_opt_in)
    }
  }
  
  dat_temp_in = dat_temp_in %>%
    rename(factor_lab = selected_col) %>%
    mutate(rank = rank(desc(factor_lab))) #%>%
  if(nrow(dat_temp_in)>0){
    ln_select_in <- international_base %>%
      select(GEOID, NAME) %>%
      inner_join(dat_temp_in,by = "GEOID") %>%
      mutate(tranp=ifelse(rank <= input$n_top_in, 1,.25))
  } else {ln_select_in=NULL}
  
  return(ln_select_in)
})

map_update_in <- reactive({
  #req(n_lines_disp$curr)
  req(click_counties_in$curr)
  req(input$dms_mode_opts_in)
  req(input$sctg2_opts_in)
  req(input$Value_opts_in)
  input$Scenario_opt_in
  req(input$OD_opts_in)
  req(input$n_top_in)
  req(input$cors_opts_in)
  paste(click_counties_in$curr,input$dms_mode_opts_in,
        input$sctg2_opts_in,
        input$Value_opts_in,input$Scenario_opt_in, input$OD_opts_in, input$n_top_in, input$cors_opts_in) 
})



#cs map update
observeEvent(eventExpr = map_update_in(), {
  req(click_counties_in$curr,input$dms_mode_opts_in,
      input$sctg2_opts_in, input$Value_opts_in,input$Scenario_opt_in, input$n_top_in)
  pb_temp <- ports_base 
  if(input$dms_mode_opts_in != "All"){
  pb_temp <- pb_temp%>% filter(mode_nm == input$dms_mode_opts_in)
  }
  #do we have to use the entire line file to remove?
  ln_select_in=data_ss_click_in()
  if(is.null(ln_select_in) & input$cors_opts_in=='p2n'){
    leafletProxy(mapId = "odmap_in",session = session) %>%
      removeShape(layerId = paste("data", international_base$GEOID)) %>%
      #removeShape(layerId = paste(all_selected$GEOID)) %>%
      #removeShape(layerId = 'leg') %>%
      removeMarker(layerId = all_selected$GEOID)%>%
      clearControls() %>%
      removeShape(layerId = 'pulsemarker')
  } else if(is.null(ln_select_in)&input$cors_opts_in=='s2n') {
    
    leafletProxy(mapId = "odmap_in",session = session) %>%
      removeShape(layerId = paste("data", international_base$GEOID)) %>%
      #removeShape(layerId = paste(all_selected$GEOID)) %>%
      #removeShape(layerId = 'leg') %>%
      clearControls() %>%
      removeShape(layerId = 'pulsemarker')}
  
  
  
  
  if(!is.null(ln_select_in)) {
    
    leafletProxy(mapId = "odmap_in",session = session) %>%
      removeShape(layerId = paste("data", international_base$GEOID)) %>%
      removeShape(layerId = paste(all_selected$GEOID)) %>%
      #removeShape(layerId = 'leg') %>%
      clearControls() %>%
      removeShape(layerId = 'pulsemarker')
    
    ln_select_in <- ln_select_in %>% 
      arrange(-rank)
    
    #lines_labs <- paste0(input$Value_opts_in,': ',round(ln_select_in$rank,6)) %>% lapply(htmltools::HTML)
    con_name = all_selected$NAME[all_selected$GEOID == click_counties_in$curr]
    
    
    if(input$OD_opts_in == "Both"){
      dir = "Inbound & Outbound to "
    }else if(input$OD_opts_in == "dms_orig"){
      dir = "Outbound from "
    } else {
      dir = "Inbound to "
    }
    
    
    
    if(grepl('tons',input$Value_opts_in)){
      titl = paste0(dir, con_name, "</br>", str_replace(str_to_title(input$Value_opts_in,),'_',' '), " (Thousand tons)")
      pulsecolor='blue'
      
      pal_factor <- colorQuantile(
        palette = "YlOrRd",
        domain = ln_select_in$factor_lab[!duplicated(ln_select_in$factor_lab)],
        probs = seq(0, 1, .2)
      )
      
    } else {
      titl = paste0(dir, con_name, "</br>", str_replace(str_to_title(input$Value_opts_in,),'_',' '), " ($Million)")
      pulsecolor='red'
      pal_factor <- colorQuantile(
        palette = "Blues",
        domain = ln_select_in$factor_lab[!duplicated(ln_select_in$factor_lab)],
        probs = seq(0, 1, .2)
      )
      
    }
    
    
    if(nrow(ln_select_in)== 1){
      pal_factor_labs = round(ln_select_in$factor_lab, 1)
      if(grepl('tons',input$Value_opts_in)){
        pal_factor = function(nada){return("#bd0026")}
        pal_factor_colors = "#bd0026"
      } else {
        pal_factor = function(nada){return('#08306b')}
        pal_factor_colors = '#08306b'
      }
    } else {
      pal_factor_colors <- unique(pal_factor(sort(ln_select_in$factor_lab[!duplicated(ln_select_in$factor_lab)])))
    }
    
    steps = 1/length(pal_factor_colors)
    pal_factor_labs <- round(quantile(round(ln_select_in$factor_lab[!duplicated(ln_select_in$factor_lab)], 1), probs = seq(0, 1, steps)), 1)
    pal_factor_labs <- paste(scales::comma(lag(pal_factor_labs)), scales::comma(pal_factor_labs), sep = " - ")[-1]
    
    
    
    
    
    if(input$cors_opts_in=="p2n"){
      pulse_name = ports_base$NAME[ports_base$GEOID == click_counties_in$curr]
      
      port_pulse=ports_base %>% 
        filter(GEOID==click_counties_in$curr) %>%
        mutate(long = sf::st_coordinates(.)[,1],
               lat = sf::st_coordinates(.)[,2])
      
      leafletProxy(mapId = "odmap_in",session = session) %>%
        addPolygons(data = ln_select_in,
                    layerId = ~paste("data", ln_select_in$GEOID),
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
        addMarkers(data = pb_temp, 
                   #radius = 18000,
                   layerId = ~GEOID,
                   #color = "purple",
                   #weight = .5,
                   #opacity = 1,
                   #fillOpacity = .3,
                   label = ~NAME,
                   labelOptions = labelOptions(
                     style = list("front-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto"),
                   icon = ~map_icons[type]
                   # highlightOptions = highlightOptions(
                   #   weight = 2,
                   #   color = "red",
                   #   fillOpacity = 0,
                   #   bringToFront = TRUE)
                   ) %>%
        addLegend(position = "bottomright",
                  layerId = 'leg',
                  colors = pal_factor_colors,
                  labels = pal_factor_labs, 
                  title = titl) %>%
        addPulseMarkers(
         layerId = 'pulsemarker',
         lng = port_pulse$long, lat = port_pulse$lat,
         label = pulse_name,
         labelOptions = labelOptions(
           style = list("front-weight" = "normal", padding = "3px 8px"),
           textsize = "15px",
           direction = "auto"),
         icon = makePulseIcon(heartbeat = 1,iconSize=10,
                              color=pulsecolor))
    } else if(input$cors_opts_in == "s2n"){
      all_states_centr_sel=all_states_centr %>% 
        filter(GEOID==click_counties_in$curr)
      pulse_name = state_choices$NAME[state_choices$GEOID == click_counties_in$curr]
      
      leafletProxy(mapId = "odmap_in",session = session) %>%
        addPolygons(data = ln_select_in,
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
    }
    
  }
  
})

outputOptions(output, 'odmap_in', suspendWhenHidden = FALSE)

table_titl <- reactiveVal("")
# output$table_title_in <- renderText({
#   req(click_counties_in$curr)
observe({
  if(input$OD_opts_in != "Both"){
    if(input$OD_opts_in == "dms_orig"){
      title = paste0("Origin: ", all_selected$NAME[all_selected$GEOID == click_counties_in$curr])
      
    } else {
      title = paste0("Destination: ", all_selected$NAME[all_selected$GEOID == click_counties_in$curr])
    }
  } else {
    title = paste0("Selected: ", all_selected$NAME[all_selected$GEOID == click_counties_in$curr])
  }
  table_titl(title)
})
output$table_title_in <- renderText({ table_titl() })

output$scenario_title_in <- renderText({
  
  if (input$Scenario_opt_in == '_s1'){
    sencario = paste0("Selected Scenario: ", 'Scenario 1')
  }else if (input$Scenario_opt_in == '_s2'){
    sencario = paste0("Selected Scenario: ", 'Scenario 2')
  }else if (input$Scenario_opt_in == '_s3'){
    sencario = paste0("Selected Scenario: ", 'Scenario 3')
  }else if (input$Scenario_opt_in == '_s4'){
    sencario = paste0("Selected Scenario: ", 'Scenario 4')
  }else if (input$Scenario_opt_in == '_s5'){
    sencario = paste0("Selected Scenario: ", 'Scenario 5')
  }else if (input$Scenario_opt_in == '_s6'){
    sencario = paste0("Selected Scenario: ", 'Scenario 6')
  }else{
    sencario = paste0("Selected Scenario: ", input$Scenario_opt_in)}
  return(sencario)
})

output$subsetSETTS_in<-renderDataTable({#server = FALSE,{
  
  
  if(input$Scenario_opt_in == 'Baseline' &
     grepl('2019',input$Value_opts_in) &
     input$dms_mode_opts_in == 'All' &
     input$sctg2_opts_in == 'All' &
     input$OD_opts_in == 'Both'){
    ln_select_in=ln_select_in_ini
    names(ln_select_in)[names(ln_select_in)=='factor_lab']=input$Value_opts_in}
  else if(input$Scenario_opt_in == 'Baseline' ){
    ln_select_in=data_ss_click_in()
    names(ln_select_in)[names(ln_select_in)=='factor_lab']=input$Value_opts_in
  }else{
    ln_select_in=data_ss_click_in()
    names(ln_select_in)[names(ln_select_in)=='factor_lab']=paste0(input$Value_opts_in, input$Scenario_opt_in)
  }
  
  
  SETTS_ss_in<-ln_select_in %>% 
    st_drop_geometry() 
  
  if(input$OD_opts_in == "Both"){
  SETTS_ss_in<-SETTS_ss_in %>% 
    left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("dms_imp_exp" = "GEOID"))
  } else if(input$OD_opts_in == "dms_orig"){
    SETTS_ss_in<-SETTS_ss_in %>%
      left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("destination" = "GEOID"))
  } else if(input$OD_opts_in == "dms_dest"){
    SETTS_ss_in<-SETTS_ss_in %>%
      left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("origin" = "GEOID"))
  }
  
  SETTS_ss_in<-SETTS_ss_in %>%
    arrange(rank) %>% 
    select('NAME',starts_with('tons_'), starts_with('value_'))
  
  SETTS_ss_in_r$SETTS_ss_in=SETTS_ss_in
  
  SETTS_ss_in<-SETTS_ss_in %>%
    mutate_at(vars(contains('tons_'),contains('value_')),~round(.,1)) %>% 
    rename('Tons 2019</br>(Thousand Tons)'='tons_2019',
           'Tons 2021</br>(Thousand Tons)'='tons_2021',
           'Value 2019</br>($Million)'='value_2019',
           'Value 2021</br>($Million)'='value_2021')
  names(SETTS_ss_in)[grepl('_',names(SETTS_ss_in))] <- str_to_title(gsub("_"," ",names(SETTS_ss_in)[grepl('_',names(SETTS_ss_in))]))
  
  #rename_all(~str_replace_all(.,'_',' ') %>% str_to_title(.)) 
  
  #Qi: not sure why this crash the tool, 
  # if(input$cors_opts_in=="p2n"){
  #   SETTS_ss_in<-SETTS_ss_in %>%
  #     rename('International Region'='NAME')#%>%
  #   #filter(rank <= input$n_top_in)
  # } else if(input$cors_opts_in=="s2n"){
  #   SETTS_ss_in<-SETTS_ss_in %>%
  #     rename('International Region'='NAME') #%>%
  #   #filter(rank <= input$n_top_in)
  # }
  
  
  
  
  SETTS_tbl_in=datatable(SETTS_ss_in,
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
    DT::formatRound(grepl("20", colnames(SETTS_ss_in)),digits = 1,mark = ",")
  
  return(SETTS_tbl_in)
  
})

proxy_cty2state_tbl = dataTableProxy('subsetSETTS_in')


observe({
  
  req(click_counties_in$curr,input$dms_mode_opts_in,
      input$county_opts_in,input$n_top_in,
      input$OD_opts_in, input$sctg2_opts_in, input$Value_opts_in,input$Scenario_opt_in)
  
  ln_select_in=data_ss_click_in()
  
  # validate(need(nrow(ln_select_in)>0,
  #               'There is no data for the selected subset.'))
  
  if(!is.null(ln_select_in)){
    
    #names(ln_select_in)[names(ln_select_in)=='factor_lab']=input$Value_opts_in
    
    if(input$Scenario_opt_in == 'Baseline' &
       grepl('2019',input$Value_opts_in) &
       input$dms_mode_opts_in == 'All' &
       input$sctg2_opts_in == 'All' &
       input$OD_opts_in == 'Both'){
      names(ln_select_in)[names(ln_select_in)=='factor_lab']=input$Value_opts_in}
    else if(input$Scenario_opt_in == 'Baseline' ){
      names(ln_select_in)[names(ln_select_in)=='factor_lab']=input$Value_opts_in
    }else{
      names(ln_select_in)[names(ln_select_in)=='factor_lab']=paste0(input$Value_opts_in, input$Scenario_opt_in)
    }

    SETTS_ss_in<-ln_select_in %>%
      st_drop_geometry()
    
    if(input$OD_opts_in == "Both"){

      SETTS_ss_in<-SETTS_ss_in %>%
        left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("dms_imp_exp" = "GEOID"))

    } else if(input$OD_opts_in == "dms_orig"){
      SETTS_ss_in<-SETTS_ss_in %>%
        left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("destination" = "GEOID"))
    } else if(input$OD_opts_in == "dms_dest"){
      SETTS_ss_in<-SETTS_ss_in %>%
        left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("origin" = "GEOID"))
    }
    
    SETTS_ss_in<-SETTS_ss_in %>%
      arrange(rank) %>%
      filter(rank <= input$n_top_in) %>%
      select(contains('NAME'),starts_with('tons_'), starts_with('value_'))
    
    SETTS_ss_in_r$SETTS_ss_in=SETTS_ss_in
    
    SETTS_ss_in<-SETTS_ss_in %>%
      mutate_at(vars(contains('tons_'),contains('value_')),~round(.,1)) %>%
      rename('Tons 2019</br>(Thousand Tons)'='tons_2019',
             'Tons 2021</br>(Thousand Tons)'='tons_2021',
             'Value 2019</br>($Million)'='value_2019',
             'Value 2021</br>($Million)'='value_2021')
    #rename_all(~str_replace_all(.,'_',' ') %>% str_to_title(.))
    SETTS_ss_in_r$SETTS_ss_in=SETTS_ss_in
    
    replaceData(proxy_cty2state_tbl, SETTS_ss_in, rownames = FALSE)
    
  } else {
    SETTS_ss_in <- data.frame('EmptyData'=c(),
                              'Tons 2019</br>(Thousand Tons)'=c(),
                              'Tons 2021</br>(Thousand Tons)'=c(),
                              'Value 2019</br>($Million)'=c(),
                              'Value 2021</br>($Million)'=c())
    SETTS_ss_in_r$SETTS_ss_in=SETTS_ss_in
    replaceData(proxy_cty2state_tbl, SETTS_ss_in, rownames = FALSE)
    
  }
})



outputOptions(output, 'subsetSETTS_in', suspendWhenHidden = FALSE)


output$download_in <- downloadHandler(
  filename = function(){
    paste("data_export",Sys.Date(), ".csv", sep="")},
  content = function(file) {
    tbl_out=SETTS_ss_in_r$SETTS_ss_in %>% 
      rename()
    write.csv(tbl_out, file,row.names = F)
  })



observe({
  req(click_counties_in$curr,input$dms_mode_opts_in,input$county_opts_in,input$n_top_in,
      input$OD_opts_in, input$sctg2_opts_in, input$Value_opts_in, input$Scenario_opt_in)
  
  if(input$cors_opts_in == "p2n"){
    dat_in <- dat_pin
  } else if(input$cors_opts_in == "s2n"){
    dat_in <- dat_sin
  }
  
  if (input$OD_opts_in == 'Both'){
    dat_in <- dat_in %>% filter(origin %in% input$county_opts_in | destination %in% input$county_opts_in)
  } else if (input$OD_opts_in == 'dms_orig'){
    dat_in <- dat_in %>% filter(origin %in% input$county_opts_in)
  }else if (input$OD_opts_in == 'dms_dest'){
    dat_in <- dat_in %>% filter(destination %in% input$county_opts_in)}
  
  #filter for mode
  if(input$dms_mode_opts_in != "All" & nrow(dat_in) >=1) {
    dat_in = dat_in %>%
      filter(dms_mode == input$dms_mode_opts_in)}
  #filter for commodity
  if(input$sctg2_opts_in != "All" & nrow(dat_in) >=1) {
    dat_in = dat_in %>%
      filter(Grouped_sctg2==input$sctg2_opts_in)}
  
  selected_value_in = input$Value_opts_in
  
  # if scenario applied
  if (input$Scenario_opt_in != 'Baseline'){
    dat_in = process_scenario_in(dat_in,
                              input$Value_opts_in,
                              input$Scenario_opt_in,
                              click_counties_in$curr,
                              c('origin', 'destination','Grouped_sctg2','dms_mode'),
                              1)
    selected_value_in = paste0(input$Value_opts_in,input$Scenario_opt_in)
  }
  #str_to_title(gsub("_"," ",paste0(input$Value_opts, input$Scenario_opt)))
  
  
  output$in_flowDirection <- renderPlotly({
    direction_pie_graph_countyselected(dat_in,
                                       county = input$county_opts_in,
                                       tons_value_selection = selected_value_in,
                                       commcolors = init_commcolors,
                                       sourceName = "in_flowDirection")
  })
  
  output$in_mode <- renderPlotly({
    mode_pie_graph(dat_in,
                   #county = input$county_opts,
                   tons_value_selection = selected_value_in,
                   ini_modecolors = ini_modecolors,
                   sourceName = "in_mode")
  })
  
  output$in_cf_commodity <- renderPlotly({
    tile_graph(dat_in,
               tons_value_selection = selected_value_in,
               sourceName = "in_cf_commodity")
  })
  
  output$in_cf_topInbound <- renderPlotly({
    top_importing_all(dat_in,
                      tons_value_selection = selected_value_in,
                      ton_color = "#66c2a5",
                      value_color = "#3288bd",
                      location = click_counties_in$curr, 
                      sourceName = "in_cf_topInbound")
    
  })
  
  output$in_cf_topOutbound <- renderPlotly({
    top_exporting_all(dat_in,
                      tons_value_selection = selected_value_in,
                      ton_color = "#66c2a5",
                      value_color = "#3288bd",
                      location = click_counties_in$curr, 
                      sourceName = "in_cf_topOutbound")
  })
})
