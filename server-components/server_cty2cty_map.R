updateSelectizeInput(session, 'county_opts', choices = cty_ch, selected = '48453', server = TRUE)

debounced_slider_n_top <- debounce(reactive(input$n_top), millis = 2000)


click_counties <- reactiveValues(curr=NULL,prev=NULL)


all_counties_centr_sel_ini=all_counties_centr %>% 
  filter(GEOID=='48453')

dat_ini <- dat %>%
  filter(dms_orig == '48453'|dms_dest == '48453') %>%
  mutate(dms_imp_exp = if_else(dms_orig == '48453', dms_dest, dms_orig),
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

ln_select_ini <- county_selected %>%
  select(GEOID,county_lab) %>%
  inner_join(dat_ini,by = "GEOID")



observeEvent(input$county_opts, {
  req(input$county_opts)
  cnty = county_selected$GEOID[county_selected$GEOID == input$county_opts]
  
  click_counties$curr <- cnty
  
})

observeEvent(input$odmap_shape_click, {
  req(input$odmap_shape_click)
  
  click_counties$prev=click_counties$curr
  click_counties$curr=input$odmap_shape_click$id
  
  cnty = county_selected$county_lab[county_selected$GEOID == click_counties$curr]
  val = click_counties$curr
  
  
  
  updateSelectizeInput(session, 'county_opts', choices = cty_ch, selected = c(cnty, value = val), server = TRUE)
  
  
})


output$odmap <- renderLeaflet({
  
  pal_factor_ini <- colorQuantile(palette = "Blues",domain = ln_select_ini$value_2017,probs = seq(0, 1, .2))
  pulsecolor_ini='red'
  
  pal_factor_colors_ini <- unique(pal_factor_ini(sort(ln_select_ini$value_2017)))
  pal_factor_labs_ini <- round(quantile(round(ln_select_ini$value_2017, 1), probs = seq(0, 1, .2)), 1)
  pal_factor_labs_ini <- paste(scales::comma(lag(pal_factor_labs_ini)), scales::comma(pal_factor_labs_ini), sep = " - ")[-1]
  
  con_name_ini=county_selected$county_lab[county_selected$GEOID == '48453']
  titl_ini = paste0("Inbound & Outbound to </br>", con_name_ini, " county </br>", str_replace(str_to_title('value_2017'),'_',' '), " ($Million)")
  
  
  
  m %>%
    addPolygons(data = ln_select_ini,
                layerId = ~paste(ln_select_ini$GEOID),
                fillColor = ~pal_factor_ini(value_2017),
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
                           color=pulsecolor_ini))
})
outputOptions(output, 'odmap', suspendWhenHidden = FALSE)


SETTS_ss_r <- reactiveValues(SETTS_ss=ln_select_ini %>% st_drop_geometry())

data_ss_click<- reactive({
  req(click_counties$curr)
  req(input$dms_mode_opts)
  req(input$sctg2_opts)
  req(input$Value_opts)
  req(input$OD_opts)
  req(debounced_slider_n_top())
  
  
  if(input$OD_opts != "Both"){
    if(input$OD_opts == "dms_orig"){
      dat_temp <- dat %>%
        filter(dms_orig == click_counties$curr) %>%
        mutate(GEOID = dms_dest)
    } else {
      dat_temp <- dat %>%
        filter(dms_dest == click_counties$curr) %>%
        mutate(GEOID = dms_orig)
    }
    if(input$dms_mode_opts != "All" & nrow(dat_temp) >=1) {
      dat_temp = dat_temp %>%
        filter(dms_mode == input$dms_mode_opts)}
    if(input$sctg2_opts != "All" & nrow(dat_temp) >=1) {
      dat_temp = dat_temp %>%
        filter(Grouped_sctg2==input$sctg2_opts)}
    
    if(input$sctg2_opts == "All" | input$dms_mode_opts == "All"){
      dat_temp = dat_temp %>%
        group_by(dms_orig, dms_dest, GEOID)%>%
        summarise(tons_2017 = sum(tons_2017), 
                  tons_2020 = sum(tons_2020),
                  tons_2050 = sum(tons_2050),
                  value_2017 = sum(value_2017),
                  value_2020 = sum(value_2020),
                  value_2050 = sum(value_2050)) %>%
        ungroup()
    } else {
      dat_temp = dat_temp %>%
        select(dms_orig, dms_dest, GEOID, contains('tons_'),contains('value_'))
    }
  } else {
    dat_temp <- dat %>%
      filter(dms_orig == click_counties$curr|dms_dest == click_counties$curr)
    if(input$dms_mode_opts != "All" & nrow(dat_temp) >=1) {
      dat_temp = dat_temp %>%
        filter(dms_mode == input$dms_mode_opts)}
    if(input$sctg2_opts != "All" & nrow(dat_temp) >=1) {
      dat_temp = dat_temp %>%
        filter(Grouped_sctg2==input$sctg2_opts)}
    
    dat_temp = dat_temp %>%
      mutate(dms_imp_exp = ifelse(dms_orig == click_counties$curr, dms_dest, dms_orig),
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
  
  dat_temp = dat_temp %>%
    rename(factor_lab = input$Value_opts) %>%
    mutate(rank = rank(desc(factor_lab))) 
  
  if(nrow(dat_temp)>0){
    ln_select <- county_selected %>%
      select(GEOID,county_lab) %>%
      inner_join(dat_temp,by = "GEOID") %>%
      mutate(tranp=ifelse(rank <= debounced_slider_n_top(), 1,.25))
  } else {ln_select=NULL}
  
  return(ln_select)
})

map_update <- reactive({
  req(click_counties$curr)
  req(input$dms_mode_opts)
  req(input$sctg2_opts)
  req(input$Value_opts)
  req(input$OD_opts)
  req(debounced_slider_n_top())
  paste(click_counties$curr,input$dms_mode_opts,input$sctg2_opts,input$Value_opts, input$OD_opts, debounced_slider_n_top()) 
})

observeEvent(eventExpr = map_update(), ignoreInit=T, {
  req(click_counties$curr,input$dms_mode_opts,
      input$sctg2_opts, input$Value_opts, debounced_slider_n_top())
  show_waiter_message()
  leafletProxy(mapId = "odmap",session = session) %>%
    removeShape(layerId = paste(county_selected$GEOID)) %>%
    removeShape(layerId = paste("data",county_selected$GEOID)) %>%
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
    ln_select <- ln_select %>% 
      arrange(-rank)
    
    if(grepl('tons',input$Value_opts)){
      titl = paste0(dir, con_name, "</br>", str_replace(str_to_title(input$Value_opts_cs,),'_',' '), " (Thousand tons)")
      pal_factor <- colorQuantile(
        palette = "YlOrRd",
        domain = ln_select$factor_lab[!duplicated(ln_select$factor_lab)],
        probs = seq(0, 1, .2)
      )
      pulsecolor='blue'
    } else {
      titl = paste0(dir, con_name, "</br>", str_replace(str_to_title(input$Value_opts_cs,),'_',' '), " ($Million)")
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
    
    
    
    
    leafletProxy(mapId = "odmap",session = session) %>%
      addPolygons(data = ln_select,
                  layerId = ~paste("data",ln_select$GEOID),
                  fillColor = ~pal_factor(factor_lab),
                  stroke=TRUE,
                  smoothFactor = 0.3,
                  color = cty_border_color,
                  weight = 1,
                  fillOpacity  = ~tranp,
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
                             color=pulsecolor))
    
  } else {
    if(grepl('tons',input$Value_opts)){
      titl = paste0(dir, con_name, "</br>", str_replace(str_to_title(input$Value_opts_cs,),'_',' '), " (Thousand tons)")
      pulsecolor='blue'
    } else {
      titl = paste0(dir, con_name, "</br>", str_replace(str_to_title(input$Value_opts_cs,),'_',' '), " ($Million)")
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




output$subsetSETTS<-renderDataTable({
  
  
  ln_select=ln_select_ini
  
  
  SETTS_ss<-ln_select %>% 
    st_drop_geometry() 
  
  SETTS_ss<-SETTS_ss %>% 
    left_join(st_drop_geometry(select(county_selected, GEOID)), by = c("dms_imp_exp" = "GEOID"))
  
  SETTS_ss<-SETTS_ss %>%
    arrange(rank) %>% 
    select(contains('county'),starts_with('tons_'), starts_with('value_')) %>% 
    rename('County' = 'county_lab')
  
  SETTS_ss_r$SETTS_ss=SETTS_ss
  
  SETTS_ss<-SETTS_ss %>%
    mutate_at(vars(contains('tons_'),contains('value_')),~round(.,1)) %>% 
    rename('Tons 2017</br>(Thousand Tons)'='tons_2017',
           'Tons 2020</br>(Thousand Tons)'='tons_2020',
           'Tons 2050</br>(Thousand Tons)'='tons_2050',
           'Value 2017</br>($Million)'='value_2017',
           'Value 2020</br>($Million)'='value_2020',
           'Value 2050</br>($Million)'='value_2050')
  
  
  
  SETTS_tbl=datatable(SETTS_ss,
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
    DT::formatRound(grepl("20", colnames(SETTS_ss)),digits = 1,mark = ",")
  
  return(SETTS_tbl)
  
})




proxy_cty2cty_tbl = dataTableProxy('subsetSETTS')


observe({
  
  req(click_counties$curr,input$dms_mode_opts,input$county_opts,debounced_slider_n_top(),
      input$OD_opts, input$sctg2_opts, input$Value_opts)
  
  ln_select=data_ss_click()
  
  if(!is.null(ln_select)){
    names(ln_select)[names(ln_select)=='factor_lab']=input$Value_opts
    
    SETTS_ss<-ln_select %>% 
      st_drop_geometry() 
    
    if(input$OD_opts == "Both"){
      SETTS_ss<-SETTS_ss %>% 
        left_join(st_drop_geometry(select(county_selected, GEOID)), by = c("dms_imp_exp" = "GEOID"))
    } else if(input$OD_opts == "dms_orig"){
      SETTS_ss<-SETTS_ss %>%
        left_join(st_drop_geometry(select(county_selected, GEOID)), by = c("dms_dest" = "GEOID"))
    } else if(input$OD_opts == "dms_dest"){
      SETTS_ss<-SETTS_ss %>%
        left_join(st_drop_geometry(select(county_selected, GEOID)), by = c("dms_orig" = "GEOID"))
    }
    
    SETTS_ss<-SETTS_ss %>%
      arrange(rank) %>% 
      filter(rank <= debounced_slider_n_top()) %>% 
      select(contains('county'),starts_with('tons_'), starts_with('value_')) %>% 
      rename('County' = 'county_lab')
    
    SETTS_ss_r$SETTS_ss=SETTS_ss
    
    SETTS_ss<-SETTS_ss %>%
      mutate_at(vars(contains('tons_'),contains('value_')),~round(.,1)) %>% 
      rename('Tons 2017</br>(Thousand Tons)'='tons_2017',
             'Tons 2020</br>(Thousand Tons)'='tons_2020',
             'Tons 2050</br>(Thousand Tons)'='tons_2050',
             'Value 2017</br>($Million)'='value_2017',
             'Value 2020</br>($Million)'='value_2020',
             'Value 2050</br>($Million)'='value_2050')
    
    replaceData(proxy_cty2cty_tbl, SETTS_ss, rownames = FALSE)} else {
      SETTS_ss <- data.frame('County'=c(),
                             'Tons 2017</br>(Thousand Tons)'=c(),
                             'Tons 2020</br>(Thousand Tons)'=c(),
                             'Tons 2050</br>(Thousand Tons)'=c(),
                             'Value 2017</br>($Million)'=c(),
                             'Value 2020</br>($Million)'=c(),
                             'Value 2050</br>($Million)'=c())
      SETTS_ss_r$SETTS_ss=SETTS_ss
      replaceData(proxy_cty2cty_tbl, SETTS_ss, rownames = FALSE)
    }
})

outputOptions(output, 'subsetSETTS', suspendWhenHidden = FALSE)  

output$download_cc <- downloadHandler(
  filename = function(){
    paste("data_export",Sys.Date(), ".csv", sep="")},
  content = function(file) {
    tbl_out=SETTS_ss_r$SETTS_ss %>% 
      rename('Tons 2017 (Thousand Tons)'='tons_2017',
             'Tons 2020 (Thousand Tons)'='tons_2020',
             'Tons 2050 (Thousand Tons)'='tons_2050',
             'Value 2017 ($Million)'='value_2017',
             'Value 2020 ($Million)'='value_2020',
             'Value 2050 ($Million)'='value_2050')
    write.csv(tbl_out, file,row.names = F)
  })

