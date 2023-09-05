updateSelectizeInput(session, 'county_opts_cs', choices = cty_ch, selected = '48453', server = TRUE)

debounced_slider_n_top_cs <- debounce(reactive(input$n_top_cs), millis = 2000)


click_counties_cs <- reactiveValues(curr=NULL,prev=NULL)

all_counties_centr_sel_ini_cs=all_counties_centr %>% 
  filter(GEOID=='48453')

dat_ini_cs <- dat_cs %>%
  filter(origin == '48453'|destination == '48453') %>%
  mutate(dms_imp_exp = if_else(origin == '48453', destination, origin),
         GEOID = dms_imp_exp) %>% 
  group_by(dms_imp_exp, GEOID)%>% 
  summarise(tons_2017 = sum(tons_2017), 
            tons_2020 = sum(tons_2020),
            tons_2050 = sum(tons_2050),
            value_2017 = sum(value_2017),
            value_2020 = sum(value_2020),
            value_2050 = sum(value_2050)) %>%
  ungroup() %>%
  mutate(rank = rank(desc(value_2017)))

ln_select_cs_ini <- state_base %>%
  select(GEOID, NAME) %>%
  inner_join(dat_ini_cs,by = "GEOID")

output$odmap_cs <- renderLeaflet({
  pal_factor_ini_cs <- colorQuantile(palette = "Blues",domain = ln_select_cs_ini$value_2017,probs = seq(0, 1, .2))
  pulsecolor_ini_cs='red'
  
  pal_factor_colors_ini_cs <- unique(pal_factor_ini_cs(sort(ln_select_cs_ini$value_2017)))
  pal_factor_labs_ini_cs <- round(quantile(round(ln_select_cs_ini$value_2017, 1), probs = seq(0, 1, .2)), 1)
  pal_factor_labs_ini_cs <- paste(scales::comma(lag(pal_factor_labs_ini_cs)), scales::comma(pal_factor_labs_ini_cs), sep = " - ")[-1]
  
  con_name_ini_cs=all_selected$NAME[all_selected$GEOID == '48453']
  titl_ini_cs = paste0("Inbound & Outbound to </br>", con_name_ini_cs, " county </br>", str_replace(str_to_title('value_2017'),'_',' '), " (Thousand tons)")
  
  m_cs %>%
    addPolygons(data = ln_select_cs_ini,
                layerId = ~paste("data", ln_select_cs_ini$GEOID),
                fillColor = ~pal_factor_ini_cs(value_2017),
                stroke=TRUE,
                smoothFactor = 0.3,
                color = '#5a5a5a',
                weight = 1,
                label = ~NAME,
                labelOptions = labelOptions(
                  style = list("front-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"),
                fillOpacity  = 1) %>%
    addPolygons(data = county_selected,
                layerId = ~GEOID,
                color = cty_border_color,
                weight = .5,
                smoothFactor = 0.3,
                opacity = .4,
                fillOpacity = 0,
                label = cty_labels,
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
      lng = all_counties_centr_sel_ini$long, lat = all_counties_centr_sel_ini$lat,
      label = con_name_ini_cs,
      labelOptions = labelOptions(
        style = list("front-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"),
      icon = makePulseIcon(heartbeat = 1,iconSize=10,
                           color=pulsecolor_ini_cs))
})
outputOptions(output, 'odmap_cs', suspendWhenHidden = FALSE)

observeEvent(input$cors_opts, ignoreInit=T, {
  
  req(input$cors_opts)
  
  if(input$cors_opts=="c2c"){
    updateSelectizeInput(session, 'county_opts_cs', label = "County", choices = cty_ch, selected = c("Travis County, TX", value = "48453"), server = TRUE)
    click_counties_cs$curr <- "48453"
    leafletProxy(mapId = "odmap_cs",session = session) %>%
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
                    bringToFront = TRUE))
  } else if(input$cors_opts=="s2s"){
    updateSelectizeInput(session, 'county_opts_cs', label = "State", choices = state_ch, selected = c("Texas", value = "48"), server = TRUE)
    click_counties_cs$curr <- "48"
  }
  
})



observeEvent(input$county_opts_cs, {
  req(input$county_opts_cs)
  cnty_cs = all_selected$GEOID[all_selected$GEOID == input$county_opts_cs]
  click_counties_cs$curr <- cnty_cs
})


observeEvent(input$odmap_cs_shape_click, {
  req(input$odmap_cs_shape_click)
  
  if(!is.null(input$odmap_cs_shape_click$id)){
    if(input$odmap_cs_shape_click$id %in% all_selected$GEOID){
      sel = all_selected$NAME[all_selected$GEOID == input$odmap_cs_shape_click$id]
      if(input$cors_opts=="c2c"){
        updateSelectizeInput(session, 'county_opts_cs', choices = cty_ch, selected = c(sel, value = input$odmap_cs_shape_click$id), server = TRUE)
      } else if(input$cors_opts=="s2s"){
        updateSelectizeInput(session, 'county_opts_cs', choices = state_ch, selected = c(sel, value = input$odmap_cs_shape_click$id), server = TRUE)
      }
      click_counties_cs$prev=click_counties_cs$curr
      click_counties_cs$curr=input$odmap_cs_shape_click$id
    }}
}) 



SETTS_ss_cs_r <- reactiveValues(SETTS_ss_cs=ln_select_cs_ini %>% st_drop_geometry())


data_ss_click_cs<- reactive({
  req(click_counties_cs$curr)
  req(input$dms_mode_opts_cs)
  req(input$sctg2_opts_cs)
  req(input$Value_opts_cs)
  req(input$OD_opts_cs)
  req(input$county_opts_cs)
  req(debounced_slider_n_top_cs())
  req(input$cors_opts)
  
  if(input$cors_opts == "c2c"){
    dat_temp_cs <- dat_cs
  } else if(input$cors_opts == "s2s"){
    dat_temp_cs <- dat_ss
  }
  
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
    if(input$dms_mode_opts_cs != "All" & nrow(dat_temp_cs) >=1) {
      dat_temp_cs = dat_temp_cs %>%
        filter(dms_mode == input$dms_mode_opts_cs)}
    if(input$sctg2_opts_cs != "All" & nrow(dat_temp_cs) >=1) {
      dat_temp_cs = dat_temp_cs %>%
        filter(Grouped_sctg2==input$sctg2_opts_cs)}
    
    if(input$sctg2_opts_cs == "All" | input$dms_mode_opts_cs == "All"){
      dat_temp_cs = dat_temp_cs %>%
        group_by(origin, destination, GEOID)%>%
        summarise(tons_2017 = sum(tons_2017), 
                  tons_2020 = sum(tons_2020),
                  tons_2050 = sum(tons_2050),
                  value_2017 = sum(value_2017),
                  value_2020 = sum(value_2020),
                  value_2050 = sum(value_2050)) %>%
        ungroup()
    } else {
      dat_temp_cs = dat_temp_cs %>%
        select(origin, destination, GEOID, contains('tons_'),contains('value_'))
    }
  } else {
    dat_temp_cs <- dat_temp_cs %>%
      filter(origin == click_counties_cs$curr|destination == click_counties_cs$curr)
    if(input$dms_mode_opts_cs != "All" & nrow(dat_temp_cs) >=1) {
      dat_temp_cs = dat_temp_cs %>%
        filter(dms_mode == input$dms_mode_opts_cs)}
    if(input$sctg2_opts_cs != "All" & nrow(dat_temp_cs) >=1) {
      dat_temp_cs = dat_temp_cs %>%
        filter(Grouped_sctg2==input$sctg2_opts_cs)}
    
    dat_temp_cs = dat_temp_cs %>%
      mutate(dms_imp_exp = ifelse(origin == click_counties_cs$curr, destination, origin),
             GEOID = dms_imp_exp) %>% 
      group_by(dms_imp_exp, GEOID)%>%
      summarise(tons_2017 = sum(tons_2017), 
                tons_2020 = sum(tons_2020),
                tons_2050 = sum(tons_2050),
                value_2017 = sum(value_2017),
                value_2020 = sum(value_2020),
                value_2050 = sum(value_2050)) %>%
      ungroup()
  }
  
  dat_temp_cs = dat_temp_cs %>%
    rename(factor_lab = input$Value_opts_cs) %>%
    mutate(rank = rank(desc(factor_lab))) 
  if(nrow(dat_temp_cs)>0){
    ln_select_cs <- state_base %>%
      select(GEOID, NAME) %>%
      inner_join(dat_temp_cs,by = "GEOID") %>%
      mutate(tranp=ifelse(rank <= debounced_slider_n_top_cs(), 1,.25))
  } else {ln_select_cs=NULL}
  
  return(ln_select_cs)
})

map_update_cs <- reactive({
  req(click_counties_cs$curr)
  req(input$dms_mode_opts_cs)
  req(input$sctg2_opts_cs)
  req(input$Value_opts_cs)
  req(input$OD_opts_cs)
  req(debounced_slider_n_top_cs())
  req(input$cors_opts)
  paste(click_counties_cs$curr,input$dms_mode_opts_cs,input$sctg2_opts_cs,
        input$Value_opts_cs, input$OD_opts_cs, debounced_slider_n_top_cs(), input$cors_opts) 
})

observeEvent(eventExpr = map_update_cs(), ignoreInit=T, {
  req(click_counties_cs$curr,input$dms_mode_opts_cs,
      input$sctg2_opts_cs, input$Value_opts_cs, debounced_slider_n_top_cs())
  show_waiter_message()
  ln_select_cs=data_ss_click_cs()
  if(is.null(ln_select_cs)&input$cors_opts=='c2c'){
    leafletProxy(mapId = "odmap_cs",session = session) %>%
      removeShape(layerId = paste("data", state_base$GEOID)) %>%
      clearControls() %>%
      removeShape(layerId = 'pulsemarker')
  } else if(is.null(ln_select_cs)&input$cors_opts=='s2s') {
    
    leafletProxy(mapId = "odmap_cs",session = session) %>%
      removeShape(layerId = paste("data", state_base$GEOID)) %>%
      clearControls() %>%
      removeShape(layerId = 'pulsemarker')}
  
  
  
  
  if(!is.null(ln_select_cs)) {
    
    leafletProxy(mapId = "odmap_cs",session = session) %>%
      removeShape(layerId = paste("data", state_base$GEOID)) %>%
      removeShape(layerId = paste(all_selected$GEOID)) %>%
      clearControls() %>%
      removeShape(layerId = 'pulsemarker')
    
    ln_select_cs <- ln_select_cs %>% 
      arrange(-rank)
    
    con_name = all_selected$NAME[all_selected$GEOID == click_counties_cs$curr]
    
    
    if(input$OD_opts_cs == "Both"){
      dir = "Inbound & Outbound to "
    }else if(input$OD_opts_cs == "origin"){
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
                    fillOpacity  =  ~tranp
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
                    fillOpacity  =  ~tranp,
                    label = ~NAME,
                    labelOptions = labelOptions(
                      style = list("front-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")
        ) %>%
        addPolygons(data = all_selected[all_selected$GEOID %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"),],
                    layerId = ~GEOID,
                    stroke=TRUE,
                    color = st_border_color,
                    weight = 1,
                    fillOpacity  = 0,
                    label = ~NAME,
                    labelOptions = labelOptions(
                      style = list("front-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"),
                    highlightOptions = highlightOptions(
                      weight = 2,
                      color = "red",
                      fillOpacity = 0)#,
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
  onFlushed(function() {
    flush_waiter_message()
  })
})





table_titl <- reactiveVal("")
observe({
  if(input$OD_opts_cs != "Both"){
    if(input$OD_opts_cs == "origin"){
      title = paste0("Origin: ", all_selected$NAME[all_selected$GEOID == click_counties_cs$curr])
      
    } else {
      title = paste0("Destination: ", all_selected$NAME[all_selected$GEOID == click_counties_cs$curr])
    }
  } else {
    title = paste0("Selected: ", all_selected$NAME[all_selected$GEOID == click_counties_cs$curr])
  }
  table_titl(title)
})
output$table_title_cs <- renderText({ table_titl() })

output$subsetSETTS_cs<-renderDataTable({
  
  
  ln_select_cs=ln_select_cs_ini
  
  names(ln_select_cs)[names(ln_select_cs)=='factor_lab']=input$Value_opts_cs
  
  SETTS_ss_cs<-ln_select_cs %>% 
    st_drop_geometry() 
  
  SETTS_ss_cs<-SETTS_ss_cs %>% 
    left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("dms_imp_exp" = "GEOID"))
  
  SETTS_ss_cs<-SETTS_ss_cs %>%
    arrange(rank) %>% 
    select('NAME',starts_with('tons_'), starts_with('value_'))
  
  SETTS_ss_cs_r$SETTS_ss_cs=SETTS_ss_cs
  
  SETTS_ss_cs<-SETTS_ss_cs %>%
    mutate_at(vars(contains('tons_'),contains('value_')),~round(.,1)) %>% 
    rename('Tons 2017</br>(Thousand Tons)'='tons_2017',
           'Tons 2020</br>(Thousand Tons)'='tons_2020',
           'Tons 2050</br>(Thousand Tons)'='tons_2050',
           'Value 2017</br>($Million)'='value_2017',
           'Value 2020</br>($Million)'='value_2020',
           'Value 2050</br>($Million)'='value_2050')
  
  
  if(input$cors_opts=="c2c"){
    SETTS_ss_cs<-SETTS_ss_cs %>%
      rename('State'='NAME')
  } else if(input$cors_opts=="s2s"){
    SETTS_ss_cs<-SETTS_ss_cs %>%
      rename('State'='NAME') 
  }
  
  
  
  
  SETTS_tbl_cs=datatable(SETTS_ss_cs,
                         filter = list(position = 'top', clear = FALSE),
                         extensions = 'Buttons',
                         options = list(lengthMenu=c(5,10,100),
                                        pageLength=10, 
                                        scrollX = TRUE,
                                        dom = 'lftp'#, 
                         ), 
                         rownames=FALSE,
                         escape = FALSE
  ) %>% 
    DT::formatRound(grepl("20", colnames(SETTS_ss_cs)),digits = 1,mark = ",")
  
  return(SETTS_tbl_cs)
  
})

proxy_cty2state_tbl = dataTableProxy('subsetSETTS_cs')


observe({
  
  req(click_counties_cs$curr,input$dms_mode_opts_cs,input$county_opts_cs,debounced_slider_n_top_cs(),
      input$OD_opts_cs, input$sctg2_opts_cs, input$Value_opts_cs)
  
  ln_select_cs=data_ss_click_cs()
  
  
  if(!is.null(ln_select_cs)){
    
    names(ln_select_cs)[names(ln_select_cs)=='factor_lab']=input$Value_opts_cs
    
    SETTS_ss_cs<-ln_select_cs %>%
      st_drop_geometry()
    
    if(input$OD_opts_cs == "Both"){
      SETTS_ss_cs<-SETTS_ss_cs %>%
        left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("dms_imp_exp" = "GEOID"))
    } else if(input$OD_opts_cs == "dms_orig"){
      SETTS_ss_cs<-SETTS_ss_cs %>%
        left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("destination" = "GEOID"))
    } else if(input$OD_opts_cs == "dms_dest"){
      SETTS_ss_cs<-SETTS_ss_cs %>%
        left_join(st_drop_geometry(select(all_selected, GEOID)), by = c("origin" = "GEOID"))
    }
    
    SETTS_ss_cs<-SETTS_ss_cs %>%
      arrange(rank) %>%
      filter(rank <= debounced_slider_n_top_cs()) %>%
      select(contains('NAME'),starts_with('tons_'), starts_with('value_'))
    
    SETTS_ss_cs_r$SETTS_ss_cs=SETTS_ss_cs
    
    SETTS_ss_cs<-SETTS_ss_cs %>%
      mutate_at(vars(contains('tons_'),contains('value_')),~round(.,1)) %>%
      rename('Tons 2017</br>(Thousand Tons)'='tons_2017',
             'Tons 2020</br>(Thousand Tons)'='tons_2020',
             'Tons 2050</br>(Thousand Tons)'='tons_2050',
             'Value 2017</br>($Million)'='value_2017',
             'Value 2020</br>($Million)'='value_2020',
             'Value 2050</br>($Million)'='value_2050')
    
    replaceData(proxy_cty2state_tbl, SETTS_ss_cs, rownames = FALSE)
    
  } else {
    SETTS_ss_cs <- data.frame('EmptyData'=c(),
                              'Tons 2017</br>(Thousand Tons)'=c(),
                              'Tons 2020</br>(Thousand Tons)'=c(),
                              'Tons 2050</br>(Thousand Tons)'=c(),
                              'Value 2017</br>($Million)'=c(),
                              'Value 2020</br>($Million)'=c(),
                              'Value 2050</br>($Million)'=c())
    SETTS_ss_cs_r$SETTS_ss_cs=SETTS_ss_cs
    replaceData(proxy_cty2state_tbl, SETTS_ss_cs, rownames = FALSE)
    
  }
})


outputOptions(output, 'subsetSETTS_cs', suspendWhenHidden = FALSE)

output$download_cs <- downloadHandler(
  filename = function(){
    paste("data_export",Sys.Date(), ".csv", sep="")},
  content = function(file) {
    tbl_out=SETTS_ss_cs_r$SETTS_ss_cs %>% 
      rename('Tons 2017 (Thousand Tons)'='tons_2017',
             'Tons 2020 (Thousand Tons)'='tons_2020',
             'Tons 2050 (Thousand Tons)'='tons_2050',
             'Value 2017 ($Million)'='value_2017',
             'Value 2020 ($Million)'='value_2020',
             'Value 2050 ($Million)'='value_2050')
    write.csv(tbl_out, file,row.names = F)
  })

#NAME	tons_2017	tons_2020	tons_2050	value_2017	value_2020	value_2050
