################################
#BASELINE SUMMARY
################################

#load---------------------------
county_base <- st_read("data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")  %>%
  filter(STATEFP %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))

county_selected <- county_base %>%
  left_join(y=tigris::fips_codes %>% 
              mutate(GEOID=paste0(state_code,county_code)) %>% 
              select(GEOID,state_abb=state)) %>%
  mutate(county_lab=paste0(NAME,', ',state_abb)) %>% 
  select(GEOID, county_lab) %>% 
  st_drop_geometry()


#define colors for the maps below

ini_modecolors <- data.frame(
  dms_mode = c("1","2","3","4","5","6","7"),
  mode_group = c("Truck", "Rail", "Water", "Air (Includes truck-air)", "Mutliple Modes and Mail", "Pipeline", "Other and Unknown"),  
  color = c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#66c2a5"))

ini_commcolors <- data.frame(commodities = c("Agriculture and Fish",
                                                        "Energy Products", 
                                                        "Food, Alcohol and Tobacco",
                                                        "Machinery, Electric, and Precision Instruments",
                                                        "Mixed Freight",
                                                        "Waste and Scrap",
                                                        "Nonmetallic Mineral and Base Metal Products",
                                                        "Raw and Finished Wood Products",
                                                        "Chemicals, Pharmaceuticals, Plastics, and Rubber",
                                                        "Vehicles and Transportation Equipment",
                                                        "Textiles and Leather",
                                                        "Aggregates"),
                         color = c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4",
                                   "#66c2a5","#3288bd", "#ec4500", "#195c99","#f69081", "#a3171c"))


dircolors <- data.frame(Direction = c("Inbound", "Outbound"), 
                        color = c("#66c2a5","#3288bd"))

#necessary for function testing-----
# df_in <- dat
# df_in <- dat_cs
# tons_value_selection <- "tons_2017"
# county_test <- county <- "01001"
# sourceName <- 'test'
# county_test <- county <- "48031"
# ton_color = "#66c2a5"
# value_color = "#3288bd"

# test<-top_import_partners(dat, county  = county_test)
# test <- top_export_partners(dat, county = county_test)
#import graph

top_import_partners <- function(df_in, county, tons_value_selection = "tons_2017",  
                                ton_color = "#66c2a5", 
                                value_color = "#3288bd", 
                                sourceName = sourceName){

  #first: filter, group, and summarize Transearch data based on user selections
  df_temp <- df_in %>%
    filter(dms_dest == county) %>%
    rename(factor_lab = tons_value_selection) %>%
    group_by(dms_orig) %>%
    summarise(factor_lab = sum(factor_lab, na.rm= T)) %>%
    ungroup() %>%
    mutate(rank = rank(desc(factor_lab))) %>% 
    filter(rank <= 10) %>%
    left_join(county_selected, by=c("dms_orig"="GEOID")) 
    
  
  #rank_keep = df_temp$rank[df_temp$dms_dest == county]
  #ranks_keep = c(seq(rank_keep-5, rank_keep), seq(rank_keep+1,rank_keep+4))
  #df_temp <- df_temp %>% filter(rank %in% ranks_keep)
  
  #then, make graph using filtered data. Graph differs for tons vs. value 
    import_plot <- df_temp %>% 
      plot_ly(source = sourceName, 
              type = "bar", color= I(ton_color), 
              x = ~reorder(county_lab, desc(factor_lab)), y = ~factor_lab, 
              hovertemplate = ~paste("%{label} <br>:", round(factor_lab,digits=0), "<extra></extra>"), 
              text = ~reorder(county_lab, desc(factor_lab)), 
              textposition = "none") %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = paste0(str_to_title(str_replace(tons_value_selection,"_", " ")))), autosize = T) %>% 
      config(displaylogo = FALSE, 
             modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
             # toImageButtonOptions= list(filename = saveName,
             #                            width = saveWidth,
             #                            height =  saveHeight)
             )
    

  return(import_plot)
  
}

top_export_partners <- function(df_in, county, tons_value_selection = "tons_2017",  
                                ton_color = "#66c2a5", 
                                value_color = "#3288bd", 
                                sourceName = sourceName){
  
  #first: filter, group, and summarize Transearch data based on user selections
  df_temp <- df_in %>%
    filter(dms_orig == county) %>%
    rename(factor_lab = tons_value_selection) %>%
    group_by(dms_dest) %>%
    summarise(factor_lab = sum(factor_lab, na.rm= T)) %>%
    ungroup() %>%
    mutate(rank = rank(desc(factor_lab))) %>% 
    filter(rank <= 10) %>%
    left_join(county_selected, by=c("dms_dest"="GEOID")) 
  
  
  #rank_keep = df_temp$rank[df_temp$dms_dest == county]
  #ranks_keep = c(seq(rank_keep-5, rank_keep), seq(rank_keep+1,rank_keep+4))
  #df_temp <- df_temp %>% filter(rank %in% ranks_keep)
  
  #then, make graph using filtered data. Graph differs for tons vs. value 
  export_plot <- df_temp %>% 
    plot_ly(source = sourceName, 
            type = "bar", color= I(ton_color), 
            x = ~reorder(county_lab, desc(factor_lab)), y = ~factor_lab, 
            hovertemplate = ~paste("%{label} <br>Tons:", round(factor_lab,digits=0), "<extra></extra>"), 
            text = ~reorder(county_lab, desc(factor_lab)), 
            textposition = "none") %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = paste0(str_to_title(str_replace(tons_value_selection,"_", " ")))), autosize = T) %>% 
    config(displaylogo = FALSE, 
           modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
           # toImageButtonOptions= list(filename = saveName,
           #                            width = saveWidth,
           #                            height =  saveHeight)
    )
  
  
  return(export_plot)
  
}

mode_pie_graph <- function(df_in, county, tons_value_selection = "tons_2017",
                           ini_modecolors = ini_modecolors,
                           sourceName = sourceName){
  #arguments: cf_db = Transearch database, yrSelection = Year, flowUnit = Tons or Value,
  #counties = counties selected by the user;
  #modecolors = data frame with two columns ("Mode_Group" and "color") that crosswalks between modes and their color on the graph
  #saveName, saveWidth, saveHeight = arguments passed to the Plotly configu function to customize file saving

  
  mode_df <- df_in %>% dplyr::filter(dms_orig %in% county | dms_dest %in% county) %>%
    dplyr::rename(factor_lab = tons_value_selection) %>% 
    select(factor_lab, dms_mode) %>% 
    #rbind(data.frame(dms_mode = c("1","2","3","4","5","6","7"), factor_lab = rep(0,7))) %>% #this fills any zero modes
    dplyr::group_by(dms_mode) %>% 
    dplyr::summarise(factor_lab = sum(factor_lab, na.rm = TRUE)) %>% ungroup() %>%
    left_join(ini_modecolors)
  
    mode_plot <- mode_df %>% plot_ly(source = sourceName) %>% 
      add_pie(values = ~factor_lab, labels = ~mode_group, automargin = TRUE, marker = list(colors = ~color, line = list(color = "#595959", width = 1)),
                                                                 hovertemplate = ~paste("%{label} <br>: ", round(factor_lab, digits = 1), "</br> %{percent} <extra></extra>"), 
                                                                 text = ~mode_group, key=~mode_group, hole = 0.6, #textfont = list(family = "Arial"), 
                                                                 textposition = "outside") %>%
      layout(#font = list(family = "Arial, "Source Sans Pro", \"Helvetica Neue\", Helvetica, sans-serif", color = "#333"),
        showlegend = FALSE, autosize = T, 
        annotations = list(text = HTML(paste0(str_to_title(str_replace(tons_value_selection,"_", " ")), "</i>")), "showarrow"=F)) %>% 
      config(displaylogo = FALSE, 
             modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
             # toImageButtonOptions= list(filename = saveName,
             #                            width = saveWidth,
             #                            height =  saveHeight)
             )

  return(mode_plot)
}

commodity_pie_graph <- function(df_in, county, tons_value_selection = "tons_2017", 
                                commcolors = init_commcolors, 
                                sourceName = sourceName){
  
  comm_temp <- df_in %>% ungroup() %>% 
    dplyr::filter(dms_orig %in% county | dms_dest %in% county) %>% 
    rename(factor_lab = tons_value_selection) %>%  
    dplyr::group_by(Grouped_sctg2) %>% 
    dplyr::summarise(factor_lab = sum(factor_lab, na.rm = TRUE)) %>% ungroup()
  
  a <- c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4",
         "#66c2a5","#3288bd", "#ec4500", "#195c99","#f69081", "#a3171c")
  
  comm_temp$color <- a[1:nrow(comm_temp)]
  comm_temp$wrapped.Comm.Grp <- sapply(comm_temp$Grouped_sctg2, 
                                     FUN = function (x){paste(strwrap(x, width = 30), collapse = "<br>")}) #taken from https://stackoverflow.com/questions/35637404/wrapping-long-axis-labels
  
  

    comm_plot <- comm_temp %>% plot_ly(source = sourceName) %>% add_pie(labels = ~wrapped.Comm.Grp, values = ~factor_lab, automargin = TRUE, #text = ~Aggregate.Commodity.Group, 
                                                                 key = ~wrapped.Comm.Grp, hole = 0.6, sort = TRUE, direction = "clockwise",
                                                                 hovertemplate = ~paste("%{label} <br>: ", round(factor_lab, digits = 1), "</br> %{percent} <extra></extra>"), 
                                                                 marker = list(colors = ~color, line = list(color = "#595959", width = 1)), #textfont = list(family = "Arial", size = 10), 
                                                                 textposition = "none") %>%
      layout(
        showlegend = TRUE, autosize = T, 
        annotations = list(text = HTML(paste0( tons_value_selection, "</i>")), "showarrow"=F)) %>% 
      config(displaylogo = FALSE, 
             modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
             # toImageButtonOptions= list(filename = saveName,
             #                            width = saveWidth,
             #                            height =  saveHeight)
             )

  return(comm_plot)
  
}

direction_pie_graph <- function(df_in, county, tons_value_selection = "tons_2017", 
                                commcolors = init_commcolors, 
                                sourceName = sourceName){
  
  dir_temp <- df_in %>% ungroup() %>% 
    dplyr::filter(dms_orig %in% county | dms_dest %in% county) 
  dir_temp$direction[dir_temp$dms_orig == county & dir_temp$dms_orig == dir_temp$dms_dest] <- "Within"
  dir_temp$direction[dir_temp$dms_orig == county & dir_temp$dms_orig != dir_temp$dms_dest] <- "Export"
  dir_temp$direction[dir_temp$dms_dest == county & dir_temp$dms_orig != dir_temp$dms_dest] <- "Import"
  dir_temp <- dir_temp %>% 
    rename(factor_lab = tons_value_selection) %>%  
    dplyr::group_by(direction) %>% 
    dplyr::summarise(factor_lab = sum(factor_lab, na.rm = TRUE)) %>% ungroup()
  
  a <- c("#d53e4f","#e6f598","#f69081")
  
  dir_temp$color <- a[1:nrow(dir_temp)]

  dir_plot <- dir_temp %>% plot_ly(source = sourceName) %>% add_pie(labels = ~direction, values = ~factor_lab, automargin = TRUE, #text = ~Aggregate.Commodity.Group, 
                                                                      key = ~direction, hole = 0.6, sort = TRUE, direction = "clockwise",
                                                                      hovertemplate = ~paste("%{label} <br>: ", round(factor_lab, digits = 1), "</br> %{percent} <extra></extra>"), 
                                                                      marker = list(colors = ~color, line = list(color = "#595959", width = 1)), #textfont = list(family = "Arial", size = 10), 
                                                                      textposition = "none") %>%
    layout(
      #showlegend = TRUE, autosize = T, 
      annotations = list(text = HTML(paste0( str_to_title(str_replace(tons_value_selection,"_", " ")), "</i>")), "showarrow"=F)) %>% 
    config(displaylogo = FALSE, 
           modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
           # toImageButtonOptions= list(filename = saveName,
           #                            width = saveWidth,
           #                            height =  saveHeight)
    )
  
  return(dir_plot)
  
}

#function for displaying total tons or value
total_tons_or_value <- function(df_in, county, tons_value_selection){
  
  text_temp <- df_in %>% 
    rename(factor_lab = tons_value_selection) %>%  
    filter(dms_orig == county|dms_dest == county) %>%
    dplyr::group_by() %>% 
    dplyr::summarise(factor_lab = sum(factor_lab, na.rm = TRUE)) %>% ungroup()
  
  text = paste0(str_replace(tons_value_selection,"_", " ") %>% str_to_title(), ":", text_temp$factor_lab)
  
  return(text)
}

#functino for bubble graph
state_bubbles <- function(df_in, county, tons_value_selection, sourceName){
  
  #browser()
  
  state_join <- data.frame(state = c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", 
                                     "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27",
                                     "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
                                     "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53",
                                     "54", "55", "56", "60", "66", "69","72", "78"),
                           state_lab =c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
                           "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
                           "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                           "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
                           "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", 
                           "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", 
                           "American Samoa", "Guam", "Northern Mariana Islands", "Puerto Rico", "U.S. Virgin Islands"))
  df_temp <- df_in %>% 
    filter(dms_orig == county|dms_dest == county) %>%
    mutate(state = ifelse(dms_orig == county, dms_dest, dms_orig)) %>%
    rename(factor_lab = tons_value_selection) %>% 
    mutate(inbound_factor = ifelse(dms_orig == state, factor_lab, 0)) %>%
    mutate(outbound_factor = ifelse(dms_dest == state, factor_lab, 0)) %>%
    group_by(state) %>%
    summarise(inbound_factor = round(sum(inbound_factor),digits = 1),
              outbound_factor = round(sum(outbound_factor), digits = 1),
              factor_lab = round(sum(factor_lab), digits=1)) %>%
    ungroup() %>% 
    left_join(state_join) %>% 
    filter(state != str_sub(county, 1,2)) 

  temp_bub<- plot_ly(df_temp, x = ~inbound_factor, y = ~outbound_factor, text = ~state_lab, type = 'scatter', 
                     mode = 'markers', size = ~factor_lab, color = ~state_lab, colors = 'Paired',
                     #Choosing the range of the bubbles' sizes:
                     sizes = c(10, 50),
                     marker = list(opacity = 0.5, sizemode = 'diameter'))
  temp_bub <- temp_bub %>% layout(title = 'Trade with US States',
                                                 xaxis = list(title = paste0("Inbound Trade (", str_to_title(str_replace(tons_value_selection,"_", " ")), ")"), showgrid = FALSE),
                                                 yaxis = list(title = paste0("Outbound Trade (", str_to_title(str_replace(tons_value_selection,"_", " ")), ")"), showgrid = FALSE),
                                                 showlegend = TRUE)
  return(temp_bub)
}

#function for displaying total tons or value
state_bar <- function(df_in, county, tons_value_selection){
  
  df_temp <- df_in %>% 
    filter(dms_orig == county|dms_dest == county) %>%
    mutate(state = ifelse(dms_orig == county, dms_dest, dms_orig)) %>%
    filter(state == str_sub(county, 1,2)) %>%
    rename(factor_lab = tons_value_selection) %>% 
    mutate(inbound_factor = ifelse(dms_orig == state, factor_lab, 0)) %>%
    mutate(outbound_factor = ifelse(dms_dest == state, factor_lab, 0)) %>%
    group_by(state) %>%
    summarise(inbound_factor = round(sum(inbound_factor),digits = 1),
              outbound_factor = round(sum(outbound_factor), digits = 1),
              factor_lab = round(sum(factor_lab), digits=1)) %>%
    ungroup() %>% 
    left_join(state_join)
    statenm <- df_temp$state_lab[1]
    fig <- plot_ly(df_temp, y = df_temp$state_lab, x = df_temp$inbound_factor, 
                   type = 'bar', width=1, 
                   orientation = 'h', name = 'Inbound Trade') %>%
      add_trace(x = df_temp$outbound_factor, y= df_temp$state_lab, name = "Outbound Trade") %>%
      layout(barmode = 'stack', 
             xaxis = list(title = "",
                                 showgrid = FALSE,
                                 showline = FALSE,
                                 showticklabels = FALSE,
                                 zeroline = FALSE),
             yaxis = list(
                                 showgrid = FALSE,
                                 showline = FALSE,
                                 showticklabels = FALSE,
                                 zeroline = FALSE))
    #fig <- fig %>% add_trace(y = ~outbound_factor, name = 'Outbound Trade')
    #fig <- fig %>% layout(yaxis = list(title = paste0('Trade with', statenm), barmode = 'stack'))
  

  return(fig)
}

#observe events
observe({
  req(input$county_opts, input$Value_opts)
  output$cf_flowDirection <- renderPlotly({
    validate(need(try(nrow(dat %>% filter((dms_orig %in% input$county_opts | dms_dest %in% input$county_opts))) > 0), 
                  message = "Please define a geography on the user selection tab to generate a figure."))
    {
      direction_pie_graph(dat, county = input$county_opts, 
                          tons_value_selection = input$Value_opts, 
                          commcolors = init_commcolors, 
                          sourceName = "direction_pie_graph")
    }
    
  })
})

observe({
  #browser()
  req(input$county_opts, input$Value_opts)
  output$cf_mode <- renderPlotly({
    validate(need(try(nrow(dat %>% filter((dms_orig %in% input$county_opts | dms_dest %in% input$county_opts))) > 0), 
                  message = "Please define a geography on the user selection tab to generate a figure."))
    {
      mode_pie_graph(dat, county = input$county_opts, 
                     tons_value_selection = input$Value_opts, 
                     ini_modecolors = ini_modecolors, 
                     sourceName = "mode_pie_graph")
    }
  })
})

observe({
  req(input$county_opts, input$Value_opts)
  output$cf_commodity <- renderPlotly({
    validate(need(try(nrow(dat %>% filter((dms_orig %in% input$county_opts | dms_dest %in% input$county_opts))) > 0), 
                  message = "Please define a geography on the user selection tab to generate a figure."))
    {
      commodity_pie_graph(dat, county = input$county_opts, 
                     tons_value_selection = input$Value_opts, 
                     commcolors = init_commcolors,
                     sourceName = "commodity_pie_graph")
    }
    
  })
})

observe({
  req(input$county_opts, input$Value_opts)
  output$cf_topInbound <- renderPlotly({
    validate(need(try(nrow(dat %>% filter((dms_orig %in% input$county_opts | dms_dest %in% input$county_opts))) > 0), 
                  message = "Please define a geography on the user selection tab to generate a figure."))
    {
      top_import_partners(dat, county = input$county_opts, 
                          tons_value_selection = input$Value_opts, 
                          ton_color = "#66c2a5", 
                          value_color = "#3288bd", 
                          sourceName = "import_graph")
    }
    
  })
})

observe({
  req(input$county_opts, input$Value_opts)
  output$cf_topOutbound <- renderPlotly({
    validate(need(try(nrow(dat %>% filter((dms_orig %in% input$county_opts | dms_dest %in% input$county_opts))) > 0), 
                  message = "Please define a geography on the user selection tab to generate a figure."))
    {
      top_export_partners(dat, county = input$county_opts, 
                          tons_value_selection = input$Value_opts, 
                          ton_color = "#66c2a5", 
                          value_color = "#3288bd", 
                          sourceName = "top_export_partners")
    }
    
  })
})

observe({
  req(input$county_opts, input$Value_opts)
  output$totalcounty_text <- renderText({
    validate(need(try(nrow(dat %>% filter((dms_orig %in% input$county_opts | dms_dest %in% input$county_opts))) > 0), 
                  message = "Please define a geography on the user selection tab to generate a figure."))
    {
      total_tons_or_value(dat, county = input$county_opts, 
                          tons_value_selection = input$Value_opts)
    }
    
  })
})

observe({
  req(input$county_opts, input$Value_opts)
  output$cf_state_trade_bubble_plot <- renderPlotly({
    validate(need(try(nrow(dat %>% filter((dms_orig %in% input$county_opts | dms_dest %in% input$county_opts))) > 0), 
                  message = "Please define a geography on the user selection tab to generate a figure."))
    {
      state_bubbles(dat_cs, county = input$county_opts, tons_value_selection = input$Value_opts)
    }
    
  })
})

observe({
  req(input$county_opts, input$Value_opts)
  output$cf_state_trade_bar <- renderPlotly({
    validate(need(try(nrow(dat %>% filter((dms_orig %in% input$county_opts | dms_dest %in% input$county_opts))) > 0), 
                  message = "Please define a geography on the user selection tab to generate a figure."))
    {
      state_bar(dat_cs, county = input$county_opts, tons_value_selection = input$Value_opts)
    }
    
  })
})
