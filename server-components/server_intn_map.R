click_counties_in <- reactiveValues(curr=NULL,prev=NULL)

#initialization---- 
#make a version for ports and internationals
print('international map initialization')
all_states_centr_sel_ini_in=all_states_centr %>% 
  filter(GEOID=='48')

port_pulse_pini=ports_base %>% 
  filter(GEOID=='4035') %>%
  mutate(long = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])
# all_state_centr_sel_ini_in=all_state_centr %>% 
#   filter(GEOID=='48453')
pulse_name_pini = ports_base$NAME[ports_base$GEOID == '4035']

dat_ini_in <- dat_sin %>%
  filter(origin == '48'|destination == '48') %>%
  mutate(dms_imp_exp = if_else(origin == '48', destination, origin),
         GEOID = dms_imp_exp) %>% 
  group_by(dms_imp_exp, GEOID)%>% 
  summarise(tons_2022 = sum(tons_2022),
            value_2022 = sum(value_2022)) %>%
  ungroup() %>%
  mutate(rank = rank(desc(value_2022)))

dat_pini_in <- dat_pin %>%
  filter(origin == '4035'|destination == '4035') %>%
  mutate(dms_imp_exp = if_else(origin == '4035', destination, origin),
         GEOID = dms_imp_exp) %>% 
  group_by(dms_imp_exp, GEOID)%>% 
  summarise(tons_2022 = sum(tons_2022),
            value_2022 = sum(value_2022)) %>%
  ungroup() %>%
  mutate(rank = rank(desc(value_2022)))

ln_select_in_ini <- international_base %>%
  select(GEOID, NAME) %>%
  inner_join(dat_ini_in,by = "GEOID")

ln_select_in_pini <- international_base %>%
  select(GEOID, NAME) %>%
  inner_join(dat_pini_in, by = "GEOID")

output$odmap_in <- renderLeaflet({

  pal_factor_ini_in <- colorQuantile(palette = "Blues",domain = ln_select_in_ini$value_2022,probs = seq(0, 1, .2))
  pulsecolor_ini_in='red'

  pal_factor_colors_ini_in <- unique(pal_factor_ini_in(sort(ln_select_in_ini$value_2022)))
  pal_factor_labs_ini_in <- round(quantile(round(ln_select_in_ini$value_2022, 1), probs = seq(0, 1, .2)), 1)
  pal_factor_labs_ini_in <- paste(scales::comma(lag(pal_factor_labs_ini_in)), scales::comma(pal_factor_labs_ini_in), sep = " - ")[-1]

  con_name_ini_in=all_selected$NAME[all_selected$GEOID == '48']
  titl_ini_in = paste0("Inbound & Outbound to </br>", con_name_ini_in, "</br>", str_replace(str_to_title('Value_2022'),'_',' '), " (K tons)")
  #print('why arent i browsing?')
  #browser()
  m_in %>%
    addPolygons(data = ln_select_in_ini,
                layerId = ~paste("data", ln_select_in_ini$GEOID),
                fillColor = ~pal_factor_ini_in(ln_select_in_ini$value_2022),
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
#update select inputs
observeEvent(input$cors_opts_in, {
  print('cors_opts_in change')
  #browser()
  req(input$cors_opts_in)

  if(input$cors_opts_in=="p2n"){
    updateSelectizeInput(session, 'county_opts_in', label = "Port", choices = port_ch, selected = c("Dallas-Fort Worth Airport", value = "4035"), server = TRUE)
    updateSelectInput(session, 'dms_mode_opts_in',label = 'Mode Type',selected = "All", choices =c("All" = "All",modes_int))
    click_counties_in$curr <- "4035"

  } else if(input$cors_opts_in=="s2n"){
    updateSelectizeInput(session, 'county_opts_in', label = "State", choices = state_ch, selected = c("Texas", value = "48"), server = TRUE)
    updateSelectInput(session, 'dms_mode_opts_in',label = 'Mode Type',selected = "All", choices =c("All" = "All",modes_int))
    click_counties_in$curr <- "48"

  }

})

#update click county
observeEvent(input$county_opts_in, {
  req(input$county_opts_in)
  cnty_in = all_selected$GEOID[all_selected$GEOID == input$county_opts_in]
  click_counties_in$curr <- cnty_in
})

#update select inputs when mode changes
observeEvent(input$dms_mode_opts_in, {
  #browser()
  if(input$cors_opts_in == "p2n"&input$dms_mode_opts_in != "All"){
    
    pb_temp<- ports_base[ports_base$mode_nm == input$dms_mode_opts_in,]
    port_ch_tmp = pb_temp$GEOID
    names(port_ch_tmp) = pb_temp$NAME
    
    if(!(input$county_opts_in %in% pb_temp$GEOID)){
      
      p_val <- int_ports_mode_no_select$use[int_ports_mode_no_select$mode == input$dms_mode_opts_in]
      p_nm <- pb_temp$NAME[pb_temp$GEOID == p_val]
      
      updateSelectizeInput(session, 'county_opts_in', choices = port_ch_tmp, selected = c(p_nm, value = p_val), server = TRUE)
      
    } else {
      updateSelectizeInput(session, 'county_opts_in', choices = port_ch_tmp, server = TRUE)
    }
  }
  
})

#update when map is clicked
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
  print('here11231')
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

# observeEvent(input$Value_opts_in,{
#   print('here222')
#   #browser()
#   if(grepl('2019',input$Value_opts_in)){
#     updateSelectizeInput(session, 'Scenario_opt_in', label = 'Scenario Options', choices = c('Baseline'), selected = 'Baseline',server = TRUE)
#   } else if (grepl(c('2022'),input$Value_opts_in)) {
#     updateSelectizeInput(session, 'Scenario_opt_in', label = 'Scenario Options', choices = c('Baseline' = 'Baseline',
#                                                                                           'Scenario 1: Respond to Heightened Supply Chain Risks' = '_s1',
#                                                                                           'Scenario 2: Leverage Multi-State Strength' = '_s2',
#                                                                                           'Scenario 3: Embrace Technology Transformations' = '_s3'),
#                          selected = 'Baseline', server = FALSE)
# 
#   }
# })

SETTS_ss_in_r <- reactiveValues(SETTS_ss_in=ln_select_in_ini %>% st_drop_geometry())


data_ss_click_in<- reactive({
  req(click_counties_in$curr)
  req(input$dms_mode_opts_in)
  req(input$sctg2_opts_in)
  req(input$Value_opts_in)
  req(input$OD_opts_in)
  req(input$county_opts_in)
  req(input$n_top_in)
  req(input$cors_opts_in)
  
  
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
    if(input$dms_mode_opts_in != "All" & nrow(dat_temp_in) >=1) {
      dat_temp_in = dat_temp_in %>%
        filter(dms_mode == input$dms_mode_opts_in)
      
    }
    if(input$sctg2_opts_in != "All" & nrow(dat_temp_in) >=1) {
      dat_temp_in = dat_temp_in %>%
        filter(Grouped_sctg2==input$sctg2_opts_in)}
    
    if(input$sctg2_opts_in == "All"| input$dms_mode_opts_in == "All"){
      dat_temp_in = dat_temp_in %>%
        group_by(origin, destination, GEOID)%>%
        summarise(tons_2022 = sum(tons_2022),
                  value_2022 = sum(value_2022)) %>%
        ungroup()
      
    } else {
      dat_temp_in = dat_temp_in %>%
        select(origin, destination, GEOID, contains('Tons_'),contains('Value_'))
    }
  } else {
    dat_temp_in <- dat_temp_in %>%
      filter(origin == click_counties_in$curr|destination == click_counties_in$curr)
    if(input$dms_mode_opts_in != "All" & nrow(dat_temp_in) >=1) {
      dat_temp_in = dat_temp_in %>%
        filter(dms_mode == input$dms_mode_opts_in)}
    if(input$sctg2_opts_in != "All" & nrow(dat_temp_in) >=1) {
      dat_temp_in = dat_temp_in %>%
        filter(Grouped_sctg2==input$sctg2_opts_in)}}
  
  dat_temp_in = dat_temp_in %>%
    mutate(dms_imp_exp = ifelse(origin == click_counties_in$curr, destination, origin),
           GEOID = dms_imp_exp) %>% 
    group_by(dms_imp_exp, GEOID)%>%
    summarise(tons_2022 = sum(tons_2022),
              value_2022 = sum(value_2022)) %>%
    ungroup()
  
  selected_col = input$Value_opts_in
  
  dat_temp_in = dat_temp_in %>%
    rename(factor_lab = selected_col) %>%
    mutate(rank = rank(desc(factor_lab)))
  
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
  #input$Scenario_opt_in
  req(input$OD_opts_in)
  req(input$n_top_in)
  req(input$cors_opts_in)
  paste(click_counties_in$curr,input$dms_mode_opts_in,
        input$sctg2_opts_in,
        input$Value_opts_in,input$OD_opts_in, input$n_top_in, input$cors_opts_in) 
})

#cs map update
observeEvent(eventExpr = map_update_in(), {
  
  req(click_counties_in$curr,input$dms_mode_opts_in,
      input$sctg2_opts_in, input$Value_opts_in, input$n_top_in)
  
  #browser()
  pb_temp <- ports_base 
  
  if(input$dms_mode_opts_in != "All"){
    pb_temp <- pb_temp%>% filter(mode_nm == input$dms_mode_opts_in)
  }
  
  ln_select_in=data_ss_click_in()
  
  if(is.null(ln_select_in) & input$cors_opts_in=='p2n'){
    
    leafletProxy(mapId = "odmap_in",session = session) %>%
      removeShape(layerId = paste("data", international_base$GEOID)) %>%
      #removeShape(layerId = paste(all_selected$GEOID)) %>%
      #removeShape(layerId = 'leg') %>%
      removeMarker(layerId = all_selected$GEOID)%>%
      clearControls() %>%
      removeShape(layerId = 'pulsemarker')
    
    warning = HTML("Your selections did not include any freight flows. <br/> 
                    Please change your selection to continue.")
    
    showNotification(HTML(warning), type = "warning")
    
  } else if(is.null(ln_select_in)&input$cors_opts_in=='s2n') {
    
    leafletProxy(mapId = "odmap_in",session = session) %>%
      removeShape(layerId = paste("data", international_base$GEOID)) %>%
      removeShape(layerId = all_selected$GEOID) %>%
      #removeShape(layerId = 'leg') %>%
      clearControls() %>%
      removeShape(layerId = 'pulsemarker')
    
    warning = HTML("Your selections did not include any freight flows. <br/> 
                    Please change your selection to continue. ")
    
    showNotification(HTML(warning), type = "warning")
    }
  

  
  if(!is.null(ln_select_in)) {
    
    leafletProxy(mapId = "odmap_in",session = session) %>%
      removeShape(layerId = paste("data", international_base$GEOID)) %>%
      removeShape(layerId = (all_selected$GEOID)) %>%
      removeMarker(layerId = all_selected$GEOID) %>%
      #removeShape(layerId = 'leg') %>%
      clearControls() %>%
      removeShape(layerId = 'pulsemarker')
    
    ln_select_in <- ln_select_in %>% 
      arrange(-rank)
    
    con_name = all_selected$NAME[all_selected$GEOID == click_counties_in$curr]
    
    
    if(input$OD_opts_in == "Both"){
      dir = "Inbound & Outbound to "
    }else if(input$OD_opts_in == "dms_orig"){
      dir = "Outbound from "
    } else {
      dir = "Inbound to "
    }
    
    
    
    if(grepl('tons',input$Value_opts_in)){
      titl = paste0(dir, con_name, "</br>", str_replace(str_to_title(input$Value_opts_in,),'_',' '), " (K tons)")
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
      print("RENDERING: Leaflet Port Map")
      #browser()
      pulse_name = ports_base$NAME[ports_base$GEOID == click_counties_in$curr]
      
      port_pulse=ports_base %>% 
        filter(GEOID==click_counties_in$curr) %>%
        mutate(long = sf::st_coordinates(.)[,1],
               lat = sf::st_coordinates(.)[,2])

      leafletProxy(mapId = "odmap_in",session = session) %>%
        addPolygons(data = ln_select_in,
                    layerId = ~paste("data", ln_select_in$GEOID),
                    fillColor = ~pal_factor(ln_select_in$factor_lab),
                    stroke = TRUE,
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
        #these are teh ports
        addMarkers(data = pb_temp, 
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
         
         icon = makePulseIcon(heartbeat = 1,
                              iconSize=10,
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

output$subsetSETTS_in<-renderDataTable({#server = FALSE,{
  #browser()
  
  ln_select_in=data_ss_click_in()
  if(!is.null(ln_select_in)){
    names(ln_select_in)[names(ln_select_in)=='factor_lab']=input$Value_opts_in
    
    SETTS_ss_in<-ln_select_in %>% st_drop_geometry() 
  
  SETTS_ss_in<-SETTS_ss_in %>% 
    left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("dms_imp_exp" = "GEOID"))
  
  SETTS_ss_in<-SETTS_ss_in %>%
    arrange(rank) %>% 
    select('NAME',starts_with('tons_'), starts_with('value_'))
  
  SETTS_ss_in_r$SETTS_ss_in=SETTS_ss_in
  
  SETTS_ss_in<-SETTS_ss_in %>%
    mutate_at(vars(contains('tons_'),contains('value_')),~round(.,1)) %>% 
    rename('Tons 2022</br>(K tons)'='tons_2022',
           'Value 2022</br>($Million)'='value_2022')
  
  names(SETTS_ss_in)[grepl('_',names(SETTS_ss_in))] <- str_to_title(gsub("_"," ",names(SETTS_ss_in)[grepl('_',names(SETTS_ss_in))]))
  
  } else {
    
    #if it's null
    SETTS_ss_in<-ln_select_in_ini[0,]|>st_drop_geometry() |>
      select('NAME',starts_with('tons_'), starts_with('value_'))|>
      rename('Tons 2022</br>(K tons)'='tons_2022',
             'Value 2022</br>($Million)'='value_2022')
    
    }
  
  SETTS_tbl_in=datatable(SETTS_ss_in,
                         filter = list(position = 'top', clear = FALSE),
                         extensions = 'Buttons',
                         options = list(lengthMenu=c(5,10,100),
                                        pageLength=10, 
                                        scrollX = TRUE,
                                        dom = 'lftp' 
                         ), 
                         rownames=FALSE,
                         escape = FALSE
  ) %>% 
    DT::formatRound(grepl("20", colnames(SETTS_ss_in)),digits = 1,mark = ",")
  
  return(SETTS_tbl_in)
  #browser()
})

proxy_cty2state_tbl = dataTableProxy('subsetSETTS_in')


observe({
  
  req(click_counties_in$curr,input$dms_mode_opts_in,
      input$county_opts_in,input$n_top_in,
      input$OD_opts_in, input$sctg2_opts_in, input$Value_opts_in)
  
  ln_select_in=data_ss_click_in()
  
  
  if(!is.null(ln_select_in)){
    
    names(ln_select_in)[names(ln_select_in)=='factor_lab']=input$Value_opts_in
    
    SETTS_ss_in<-ln_select_in %>%
      st_drop_geometry()
    #browser()
  
    if(input$OD_opts == "Both"){
      SETTS_ss_in<-SETTS_ss_in %>% 
        left_join(st_drop_geometry(select(county_choices, GEOID)), by = c("dms_imp_exp" = "GEOID"))
    } else {
      SETTS_ss_in<-SETTS_ss_in %>% 
        left_join(st_drop_geometry(select(county_choices, GEOID)), by = c("GEOID" = "GEOID"))
    }
    
    SETTS_ss_in<-SETTS_ss_in %>%
      arrange(rank) %>%
      filter(rank <= input$n_top_in) %>%
      select(contains('NAME'),starts_with('tons_'), starts_with('value_'))
    
    SETTS_ss_in_r$SETTS_ss_in=SETTS_ss_in
    
    SETTS_ss_in<-SETTS_ss_in %>%
      mutate_at(vars(contains('tons_'),contains('value_')),~round(.,1)) %>%
      rename('Tons 2022</br>(K tons)'='tons_2022',
             'Value 2022</br>($Million)'='value_2022')
    
    replaceData(proxy_cty2state_tbl, SETTS_ss_in, rownames = FALSE)
    
  } else {
    SETTS_ss_in <- data.frame('EmptyData'=c(),
                              'Tons 2022</br>(K tons)'=c(),
                              'Value 2022</br>($Million)'=c())
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
      input$OD_opts_in, input$sctg2_opts_in, input$Value_opts_in)
  
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
  
  # # if scenario applied
  # if (input$Scenario_opt_in != 'Baseline'){
  #   dat_in = process_scenario_in(dat_in,
  #                             input$Value_opts_in,
  #                             input$Scenario_opt_in,
  #                             click_counties_in$curr,
  #                             c('origin', 'destination','Grouped_sctg2','dms_mode'),
  #                             1)
  #   selected_value_in = paste0(input$Value_opts_in,input$Scenario_opt_in)
  # }
  # #str_to_title(gsub("_"," ",paste0(input$Value_opts, input$Scenario_opt)))
  
  
  output$in_flowDirection <- renderPlotly({
    direction_pie_graph_countyselected(dat_in,
                                       county = input$county_opts_in,
                                       tons_value_selection = selected_value_in,
                                       commcolors = init_commcolors,
                                       sourceName = "in_flowDirection")
  })
  
  output$in_mode <- renderPlotly({
    mode_pie_graph_v2(dat_in,
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
