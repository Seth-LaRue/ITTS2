################################
#BASELINE SUMMARY
################################

#load---------------------------
# county_base <- st_read("data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")  %>%
#   dplyr::filter(STATEFP %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))
# 
# county_selected <- county_base %>%
#   left_join(y=tigris::fips_codes %>% 
#               dplyr::mutate(GEOID=paste0(state_code,county_code)) %>% 
#               dplyr::select(GEOID,state_abb=state)) %>%
#   dplyr::mutate(county_lab=paste0(NAME,', ',state_abb)) %>% 
#   dplyr::select(GEOID, county_lab) %>% 
#   st_drop_geometry()


#define colors for the maps below

# geoinput <- reactiveVal(value = c(county_base$COUNTYFP))

# ini_modecolors2 <- data.frame(
#   dms_mode = c("1","2","3","4","5","6","7","8"),
#   mode_group = c("Truck", "Rail", "Water", "Air (Includes truck-air)", "Mutliple Modes and Mail", "Pipeline", "Other and Unknown","Non-Domestic Mode"),
#   color = c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#66c2a5","#E11111"))
# 
# ini_commcolors <- data.frame(commodities = c("Agriculture and Fish",
#                                                         "Energy Products", 
#                                                         "Food, Alcohol and Tobacco",
#                                                         "Machinery, Electric, and Precision Instruments",
#                                                         "Mixed Freight",
#                                                         "Waste and Scrap",
#                                                         "Nonmetallic Mineral and Base Metal Products",
#                                                         "Raw and Finished Wood Products",
#                                                         "Chemicals, Pharmaceuticals, Plastics, and Rubber",
#                                                         "Vehicles and Transportation Equipment",
#                                                         "Textiles and Leather",
#                                                         "Aggregates"),
#                          color = c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4",
#                                    "#66c2a5","#3288bd", "#ec4500", "#195c99","#f69081", "#a3171c"))
# 
# 
# dircolors <- data.frame(Direction = c("Inbound", "Outbound"), 
#                         color = c("#66c2a5","#3288bd"))

#necessary for function testing-----
# df_in <- dat
# df_in <- dat_cs
# tons_value_selection <- "tons_2022"
# county_test <- county <- "01001"
# sourceName <- 'test'
# county_test <- county <- "48031"
# ton_color = "#66c2a5"
# value_color = "#3288bd"

# test<-top_import_partners(dat, county  = county_test)
# test <- top_export_partners(dat, county = county_test)
#import graph

top_importing_county <- function(df_in, tons_value_selection = "tons_2022",  
                                ton_color = "#66c2a5", 
                                value_color = "#3288bd", 
                                sourceName = sourceName){

  #first: filter, group, and summarize Transearch data based on user selections
  df_temp <- df_in %>%
    #filter(destination %in% county) %>%
    rename(factor_lab = tons_value_selection) %>%
    group_by(origin) %>%
    summarise(factor_lab = sum(factor_lab, na.rm= T)) %>%
    ungroup() %>%
    mutate(rank = rank(desc(factor_lab))) %>% 
    filter(rank <= 10) %>%
    left_join(county_selected, by=c("origin"="GEOID")) # Qi: changed from county_selected to al_selected, to apply for all geographic level.
    
  
  #rank_keep = df_temp$rank[df_temp$destination == county]
  #ranks_keep = c(seq(rank_keep-5, rank_keep), seq(rank_keep+1,rank_keep+4))
  #df_temp <- df_temp %>% filter(rank %in% ranks_keep)
  
  #then, make graph using filtered data. Graph differs for tons vs. value 
    import_plot <- df_temp %>% 
      plot_ly(source = sourceName, 
              type = "bar", color= I(ton_color), 
              x = ~reorder(NAME, desc(factor_lab)), y = ~factor_lab,    # change from county_lab to NAME column
              hovertemplate = ~paste("%{label} <br>:", round(factor_lab,digits=0), "<extra></extra>"), 
              text = ~reorder(NAME, desc(factor_lab)), 
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


## exclude the internal flows within the selected geography
top_importing_all <- function(df_in, tons_value_selection = "tons_2022",  
                                 ton_color = "#66c2a5", 
                                 value_color = "#3288bd",
                              location,
                                 sourceName = sourceName){
  
  #first: filter, group, and summarize Transearch data based on user selections
  df_temp <- df_in %>%
    filter(destination != origin,
           destination == location) %>% # exclude internal flows. 
    rename(factor_lab = tons_value_selection) %>%
    group_by(origin) %>%
    summarise(factor_lab = sum(factor_lab, na.rm= T)) %>%
    ungroup() %>%
    mutate(rank = rank(desc(factor_lab))) %>% 
    filter(rank <= 10) %>%
    left_join(all_selected, by=c("origin"="GEOID")) # Qi: changed from county_selected to al_selected, to apply for all geographic level.
  
  if(grepl("value",tons_value_selection)){
    unit = ""
    unit_pre = "$"
  } else {
    unit = " Thousand tons"
    unit_pre = ""
  }
  #then, make graph using filtered data. Graph differs for tons vs. value 
  import_plot <- df_temp %>% 
    plot_ly(source = sourceName, 
            type = "bar", color= I(ton_color), 
            x = ~reorder(NAME, desc(factor_lab)), y = ~factor_lab,    # change from county_lab to NAME column
            hovertemplate = ~paste0("%{label}:<br>", 
                                   unit_pre, formatC(factor_lab,digits=1,format="f",big.mark = ","),unit, "<extra></extra>"), 
            text = ~reorder(NAME, desc(factor_lab)), 
            textposition = "none") %>%
    layout(
      xaxis = list(title = "",tickfont = list(size = 15)),
      yaxis = list(title = paste0(str_to_title(str_replace(tons_value_selection,"_", " "))," (Thousand Tons)"),tickfont = list(size = 15))) %>% 
    config(displaylogo = FALSE, 
           modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
           # toImageButtonOptions= list(filename = saveName,
           #                            width = saveWidth,
           #                            height =  saveHeight)
    )
  if(grepl("value",tons_value_selection)){
    import_plot<-import_plot %>% layout(yaxis = list(title = paste0(str_to_title(str_replace(tons_value_selection,"_", " "))), tickformat = "$~s"))
  }
  
  return(import_plot)
  
}



top_exporting_county <- function(df_in, tons_value_selection = "tons_2022",  
                                ton_color = "#66c2a5", 
                                value_color = "#3288bd", 
                                sourceName = sourceName){
  
  #first: filter, group, and summarize Transearch data based on user selections
  df_temp <- df_in %>%
    #filter(origin == county) %>%
    rename(factor_lab = tons_value_selection) %>%
    group_by(destination) %>%
    summarise(factor_lab = sum(factor_lab, na.rm= T)) %>%
    ungroup() %>%
    mutate(rank = rank(desc(factor_lab))) %>% 
    filter(rank <= 10) %>%
    left_join(county_selected, by=c("destination"="GEOID")) 
  
  if(grepl("value",tons_value_selection)){
    unit = ""
    unit_pre = "$"
  } else {
    unit = " Thousand tons"
    unit_pre = ""
  }
  
  #rank_keep = df_temp$rank[df_temp$destination == county]
  #ranks_keep = c(seq(rank_keep-5, rank_keep), seq(rank_keep+1,rank_keep+4))
  #df_temp <- df_temp %>% filter(rank %in% ranks_keep)
  
  #then, make graph using filtered data. Graph differs for tons vs. value 
  export_plot <- df_temp %>% 
    plot_ly(source = sourceName, 
            type = "bar", color= I(ton_color), 
            x = ~reorder(NAME, desc(factor_lab)), y = ~factor_lab, 
            hovertemplate = ~paste("%{label}:<br>", 
                                   unit_pre, round(factor_lab,digits=0),
                                   unit, "<extra></extra>"), 
            text = ~reorder(NAME, desc(factor_lab)), 
            textposition = "none") %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = paste0(str_to_title(str_replace(tons_value_selection,"_", " ")), " (Thousand tons)")), autosize = T) %>% 
    config(displaylogo = FALSE, 
           modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
           # toImageButtonOptions= list(filename = saveName,
           #                            width = saveWidth,
           #                            height =  saveHeight)
    )
  
  if(grepl("value",tons_value_selection)){
    export_plot<-export_plot %>% layout(yaxis = list(title = paste0(str_to_title(str_replace(tons_value_selection,"_", " "))), tickformat = "$~s"))
  }
  
  
  return(export_plot)
  
}

top_exporting_all <- function(df_in, tons_value_selection = "tons_2022",  
                                 ton_color = "#66c2a5", 
                                 value_color = "#3288bd", 
                                 location,
                                 sourceName = sourceName){
  
  if(grepl("value",tons_value_selection)){
    unit = ""
    unit_pre = "$"
  } else {
    unit = " Thousand tons"
    unit_pre = ""
  }
  #first: filter, group, and summarize Transearch data based on user selections
  df_temp <- df_in %>%
    filter(origin != destination,
           origin == location) %>%
    rename(factor_lab = tons_value_selection) %>%
    group_by(destination) %>%
    summarise(factor_lab = sum(factor_lab, na.rm= T)) %>%
    ungroup() %>%
    mutate(rank = rank(desc(factor_lab))) %>% 
    filter(rank <= 10) %>%
    left_join(all_selected, by=c("destination"="GEOID")) 
  
  
  #rank_keep = df_temp$rank[df_temp$destination == county]
  #ranks_keep = c(seq(rank_keep-5, rank_keep), seq(rank_keep+1,rank_keep+4))
  #df_temp <- df_temp %>% filter(rank %in% ranks_keep)
  
  #then, make graph using filtered data. Graph differs for tons vs. value 
  export_plot <- df_temp %>% 
    plot_ly(source = sourceName, 
            type = "bar", color= I(ton_color), 
            x = ~reorder(NAME, desc(factor_lab)), y = ~factor_lab, 
            hovertemplate = ~paste0("%{label}:<br>", 
                                   unit_pre, formatC(factor_lab,digits=1,format="f",big.mark = ","),
                                   unit, "<extra></extra>"), 
            text = ~reorder(NAME, desc(factor_lab)), 
            textposition = "none") %>%
    layout(
      xaxis = list(title = "",tickfont = list(size = 15)),
      yaxis = list(title = paste0(str_to_title(str_replace(tons_value_selection,"_", " ")), " (Thousand tons)"),tickfont = list(size = 15))) %>% 
    config(displaylogo = FALSE, 
           modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", 
                                      "zoomOut2d", "resetScale2d", "toggleSpikelines", 
                                      "hoverCompareCartesian", "hoverClosestGeo", 
                                      "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", 
                                      "hoverClosestPie", "toggleHover", "hoverClosestCartesian")
    )
  if(grepl("value",tons_value_selection)){
    export_plot<-export_plot %>% layout(yaxis = list(title = paste0(str_to_title(str_replace(tons_value_selection,"_", " "))), tickformat = "$~s"))
  }
  
  return(export_plot)
  
}

mode_pie_graph <- function(df_in, tons_value_selection = "tons_2022",
                           ini_modecolors = ini_modecolors,
                           sourceName = sourceName){
  #browser()
  #arguments: cf_db = Transearch database, yrSelection = Year, flowUnit = Tons or Value,
  #counties = counties selected by the user;
  #modecolors = data frame with two columns ("Mode_Group" and "color") that crosswalks between modes and their color on the graph
  #saveName, saveWidth, saveHeight = arguments passed to the Plotly configu function to customize file saving

  mode_df <- df_in %>% 
    #dplyr::filter(origin %in% county | destination %in% county) %>%
    dplyr::rename(factor_lab = tons_value_selection) %>% 
    #select(factor_lab, dms_mode) %>% 
    #rbind(data.frame(dms_mode = c("1","2","3","4","5","6","7"), factor_lab = rep(0,7))) %>% #this fills any zero modes
    dplyr::group_by(dms_mode) %>% 
    dplyr::summarise(factor_lab = sum(factor_lab, na.rm = TRUE)) %>% ungroup() %>%
    left_join(ini_modecolors %>% mutate(dms_mode = as.numeric(dms_mode)))

      mode_plot <- mode_df %>% plot_ly(source = sourceName) %>% 
      add_pie(values = ~factor_lab, 
              #textinfo='none', 
              labels = ~mode_group, 
              automargin = TRUE, 
              marker = list(colors = ~color, 
                            line = list(color = "#595959", width = 1)),
              hovertemplate = ~paste("%{label} <br> : ", formatC(factor_lab, digits = 1, big.mark = ",",format="f"), "</br> %{percent} <extra></extra>"),
              text = ~mode_group, key=~mode_group, hole = 0.6, #textfont = list(family = "Arial"),
              textposition = "outside")
  
    mode_plot <- mode_df %>% plot_ly(source = sourceName) %>% 
      add_pie(values = ~factor_lab,  textinfo='none', labels = ~str_wrap(mode_group,14), automargin = TRUE, marker = list(colors = ~color, line = list(color = "#595959", width = 1)),
                                                                 hovertemplate = ~paste("%{label} <br> : ", formatC(factor_lab, digits = 1, big.mark = ",",format="f"), "</br> %{percent} <extra></extra>"), 
                                                                 text = ~mode_group, key=~mode_group, hole = 0.6, #textfont = list(family = "Arial"), 
                                                                 textposition = "outside") %>%
      layout(#font = list(family = "Arial, "Source Sans Pro", \"Helvetica Neue\", Helvetica, sans-serif", color = "#333"),
        #showlegend = T, autosize = T, 
        annotations = list(text = HTML(paste0(str_to_title(str_replace(tons_value_selection,"_", " ")), "</i>")), "showarrow"=F,font=list(size = 20))) %>% 
      layout(legend = list(font = list(size = 16))) %>%
      config(displaylogo = FALSE, 
             modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
             # toImageButtonOptions= list(filename = saveName,
             #                            width = saveWidth,
             #                            height =  saveHeight)
             )
  return(mode_plot)
}



mode_pie_graph_v2 <- function(df_in, tons_value_selection = "Value_2022)",
                           ini_modecolors = ini_modecolors,
                           sourceName = sourceName){
  #arguments: cf_db = Transearch database, yrSelection = Year, flowUnit = Tons or Value,
  #counties = counties selected by the user;
  #modecolors = data frame with two columns ("Mode_Group" and "color") that crosswalks between modes and their color on the graph
  #saveName, saveWidth, saveHeight = arguments passed to the Plotly configu function to customize file saving
  
  
  mode_df <- df_in %>% 
    #dplyr::filter(origin %in% county | destination %in% county) %>%
    dplyr::rename(factor_lab = tons_value_selection) %>% 
    #select(factor_lab, dms_mode) %>% 
    #rbind(data.frame(dms_mode = c("1","2","3","4","5","6","7"), factor_lab = rep(0,7))) %>% #this fills any zero modes
    dplyr::group_by(dms_mode) %>% 
    dplyr::summarise(factor_lab = sum(factor_lab, na.rm = TRUE)) %>% ungroup() %>%
    left_join(ini_modecolors %>% mutate(dms_mode = as.numeric(dms_mode)))
  

  if (length(strsplit(tons_value_selection, "_")[[1]]) == 1) {
    formatted_label <- str_to_title(str_replace(tons_value_selection,"_", " "))
  } else {
    formatted_label <- str_to_title(str_replace_all(tons_value_selection, "_", " "))
  }
  
  if(grepl("Value",formatted_label)){
    unit = ""
    unit_pre = "$"
  } else {
    unit = " Thousand tons"
    unit_pre = ""
  }
  
  mode_plot <- mode_df %>% plot_ly(source = sourceName) %>% 
    add_pie(values = ~factor_lab, 
            #textinfo='none', 
            labels = ~mode_group, 
            automargin = TRUE, 
            marker = list(colors = ~color, 
                          line = list(color = "#595959", width = 1)),
            hovertemplate = ~paste0("%{label}:<br>", 
                                   unit_pre, formatC(factor_lab, digits = 1, big.mark = ",",format="f"), unit, 
                                   "<br> %{percent} <extra></extra>"),
            text = ~mode_group, key=~mode_group, hole = 0.6, #textfont = list(family = "Arial"),
            textposition = "outside") %>%
    layout(#font = list(family = "Arial, "Source Sans Pro", \"Helvetica Neue\", Helvetica, sans-serif", color = "#333"),
      #showlegend = T, autosize = T, 
      annotations = list(text = HTML(paste0(formatted_label, "</i>")), "showarrow"=F,font=list(size = 20))) %>% 
    layout(legend = list(font = list(size = 20))) %>%
    config(displaylogo = FALSE, 
           modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
           # toImageButtonOptions= list(filename = saveName,
           #                            width = saveWidth,
           #                            height =  saveHeight)
    )
  
  return(mode_plot)
}




commodity_pie_graph <- function(df_in, tons_value_selection = "tons_2022", 
                                commcolors = init_commcolors, 
                                sourceName = sourceName){
  
  comm_temp <- df_in %>% ungroup() %>% 
    #dplyr::filter(origin %in% county | destination %in% county) %>% 
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

direction_pie_graph_countyselected <- function(df_in, county, tons_value_selection = "tons_2022", 
                                commcolors = init_commcolors, 
                                sourceName = sourceName){
  #browser()
  dir_temp <- df_in %>% dplyr::filter(origin %in% county | destination %in% county) 
  dir_temp$direction[dir_temp$origin == county & dir_temp$origin == dir_temp$destination] <- "Within"
  dir_temp$direction[dir_temp$origin == county & dir_temp$origin != dir_temp$destination] <- "Outbound"
  dir_temp$direction[dir_temp$destination == county & dir_temp$origin != dir_temp$destination] <- "Inbound"
  dir_temp <- dir_temp %>% 
    rename(factor_lab = tons_value_selection) %>%  
    dplyr::group_by(direction) %>% 
    dplyr::summarise(factor_lab = sum(factor_lab, na.rm = TRUE)) %>% ungroup()
  
  a <- c("#d53e4f","#e6f598","#f69081")
  
  dir_temp$color <- a[1:nrow(dir_temp)]
  
  if (length(strsplit(tons_value_selection, "_")[[1]]) == 1) {
    formatted_label <- str_to_title(str_replace(tons_value_selection,"_", " "))
  } else {
    formatted_label <- str_to_title(str_replace_all(tons_value_selection, "_", " "))
  }
  
  if(grepl("Value",formatted_label)){
    unit = ""
    unit_pre = "$"
  } else {
    unit = " Thousand tons"
    unit_pre = ""
  }

  dir_plot <- dir_temp %>% plot_ly(source = sourceName, 
                                   labels = ~direction, 
                                   values = ~factor_lab) %>% 
    add_pie(automargin = TRUE, #text = ~Aggregate.Commodity.Group, 
            key = ~direction, 
            hole = 0.6, 
            sort = TRUE, 
            direction = "clockwise",hovertemplate = ~paste0("%{label}:<br>", 
                                                            unit_pre, formatC(factor_lab, digits = 1, big.mark = ",",format="f"), unit, 
                                                            "<br> %{percent} <extra></extra>"), 
            marker = list(colors = ~color, 
                          line = list(color = "#595959", 
                                      width = 1)), 
            #textfont = list(family = "Arial", size = 10), 
            textposition = "bottom center") %>%
    layout(
      #showlegend = TRUE, autosize = T, 
      annotations = list(text = HTML(paste0( formatted_label, "</i>")), "showarrow"=F),font=list(size = 20)) #%>% 
    # config(displaylogo = FALSE, 
    #        modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
    #        # toImageButtonOptions= list(filename = saveName,
    #        #                            width = saveWidth,
    #        #                            height =  saveHeight)
    # )
  
  return(dir_plot)
  
}


direction_pie_graph <- function(df_in, tons_value_selection = "tons_2022", 
                                               commcolors = init_commcolors, 
                                               sourceName = sourceName){
  
  dir_temp <- df_in %>% 
    rename(factor_lab = tons_value_selection) %>%  
    dplyr::group_by(direction) %>% 
    dplyr::summarise(factor_lab = sum(factor_lab, na.rm = TRUE)) %>% ungroup()
  
  a <- c("#d53e4f","#e6f598","#f69081")
  
  dir_temp$color <- a[1:nrow(dir_temp)]
  
  dir_plot <- dir_temp %>% plot_ly(source = sourceName) %>% add_pie(labels = ~direction, values = ~factor_lab, automargin = TRUE, #text = ~Aggregate.Commodity.Group, 
                                                                    key = ~direction, hole = 0.6, sort = TRUE, direction = "clockwise",
                                                                    hovertemplate = ~paste("%{label} <br>: ", round(factor_lab, digits = 1), "</br> %{percent} <extra></extra>"), 
                                                                    marker = list(colors = ~color, line = list(color = "#595959", width = 1)), #textfont = list(family = "Arial", size = 10), 
                                                                    #textposition = "none"
                                                                    ) %>%
    layout(
      #showlegend = TRUE, autosize = T, 
      annotations = list(text = HTML(paste0( str_to_title(str_replace(tons_value_selection,"_", " ")), "</i>")), "showarrow"=F,font=list(size = 20))) %>% 
    config(displaylogo = FALSE, 
           modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
           # toImageButtonOptions= list(filename = saveName,
           #                            width = saveWidth,
           #                            height =  saveHeight)
    )
  
  return(dir_plot)
  
}

#function for displaying total tons or value
total_tons_or_value <- function(df_in, tons_value_selection){
  
  text_temp <- df_in %>% 
    rename(factor_lab = tons_value_selection) %>%  
    dplyr::group_by() %>% 
    dplyr::summarise(factor_lab = round(sum(factor_lab, na.rm = TRUE),digits = 0)) %>% ungroup()
  
  text = paste0(str_replace(tons_value_selection,"_", " ") %>% str_to_title(), ":", text_temp$factor_lab)
  
  return(text)
}

#functino for bubble graph
state_bubbles <- function(df_in, tons_value_selection, sourceName){
  

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
    #filter(origin == county|destination == county) %>%
    #mutate(state = ifelse(origin == county, destination, origin)) %>%
    rename(factor_lab = tons_value_selection) %>% 
    mutate(inbound_factor = ifelse(direction == "Import", factor_lab, 0)) %>%
    mutate(outbound_factor = ifelse(direction == "Export", factor_lab, 0)) %>%
    mutate(within_factor = ifelse(direction == "Within", factor_lab, 0)) %>% 
    group_by(state) %>%
    summarise(inbound_factor = round(sum(inbound_factor, na.rm =T),digits = 1),
              outbound_factor = round(sum(outbound_factor, na.rm =T), digits = 1),
              within_factor = round(sum(within_factor, na.rm =T), digits=1)) %>%
    ungroup() %>% 
    left_join(state_join) 

  temp_bub<- plot_ly(df_temp, x = ~inbound_factor, y = ~outbound_factor, text = ~state_lab, type = 'scatter', 
                     mode = 'markers', size = ~within_factor, color = ~state_lab, colors = 'Paired',
                     #Choosing the range of the bubbles' sizes:
                     sizes = c(10, 50),
                     marker = list(opacity = 0.5, sizemode = 'diameter'))
  temp_bub <- temp_bub %>% layout(title = 'Trade with US States',
                                                 xaxis = list(title = paste0("Inbound Trade (", str_to_title(str_replace(tons_value_selection,"_", " ")), ")"), showgrid = FALSE),
                                                 yaxis = list(title = paste0("Outbound Trade (", str_to_title(str_replace(tons_value_selection,"_", " ")), ")"), showgrid = FALSE),
                                                 showlegend = TRUE)
  return(temp_bub)
}

#function for tile graph 
tile_graph <- function(df_in, tons_value_selection, sourceName){

  coms_temp<-df_in %>% 
    rename(factor_lab = tons_value_selection) %>% 
    group_by(Grouped_sctg2) %>% 
    summarise(factor_lab = sum(factor_lab)) %>%
    ungroup() %>% mutate(parents = '') %>%
    mutate(percent = factor_lab/sum(factor_lab))

  if(grepl("value",tons_value_selection)){
    unit = ""
    unit_pre = "$"
  } else {
    unit = " Thousand tons"
    unit_pre = ""
  }
  
    tile_graph <- plot_ly(
    data = coms_temp,
    type = "treemap",
    ids = ~Grouped_sctg2,
    labels = ~Grouped_sctg2 %>% str_wrap(width = 20),
    branchvalues = 'total',
    values = ~factor_lab,
    parents = ~parents,
    text = ~paste0(unit_pre, format(round(factor_lab),big.mark = ","), unit, "<br>", round(percent*100, 1), "%"),
    hovertemplate = ~paste0("%{label}:<br>", unit_pre, formatC(factor_lab,digits=1,format="f",big.mark = ","), unit, "<extra></extra>")
  ) %>%
    layout(uniformtext=list(minsize= 20 #,mode='hide'
                            ))
  tile_graph
  return(tile_graph)
}
#observe events
#PLEASE CONFIRM THIS IS NECCESARY?
# observe({
#   req(input$stab1_value_opts)
#   dir_temp <- dat_cs
#   dir_temp$direction[(nchar(dir_temp$origin) == 5 & str_sub(dir_temp$origin,1,2) %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))|(nchar(dir_temp$destination) == 5 & str_sub(dir_temp$destination,1,2) %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))] <- "Within ITTS"
#   dir_temp$direction[nchar(dir_temp$origin) == 5 & !(dir_temp$destination %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))] <- "Export"
#   dir_temp$direction[nchar(dir_temp$destination) == 5  & !(dir_temp$origin %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))] <- "Import"
#   
#   output$cf_flowDirection <- renderPlotly({
#     #validate(need(try(nrow(dat %>% filter((origin %in% input$stab1_county_opts | destination %in% input$stab1_county_opts))) > 0), 
#     #              message = "Please define a geography on the user selection tab to generate a figure."))
#     #{
#       direction_pie_graph(dir_temp, #county = input$stab1_county_opts, 
#                           tons_value_selection = input$stab1_value_opts, 
#                           commcolors = init_commcolors, 
#                           sourceName = "direction_pie_graph")
#     #}
#   })
# })
# 
# observe({
#   req(input$stab1_value_opts)
#   output$cf_mode <- renderPlotly({
# 
#       mode_pie_graph(dat_cs,
#                      tons_value_selection = input$stab1_value_opts, 
#                      ini_modecolors = ini_modecolors, 
#                      sourceName = "mode_pie_graph")
#     
#   })
# })
# 
# observe({
#   req(input$stab1_value_opts)
#   output$cf_commodity <- renderPlotly({
#       commodity_pie_graph(dat_cs %>% filter(nchar(destination)==5),
#                      tons_value_selection = input$stab1_value_opts, 
#                      commcolors = init_commcolors,
#                      sourceName = "commodity_pie_graph")
#     })
# })
# 
# observe({
#   req(input$stab1_value_opts)
#   output$cf_topInbound <- renderPlotly({
#       top_importing_county(dat_cs %>% filter(nchar(origin) == 5),
#                           tons_value_selection = input$stab1_value_opts, 
#                           ton_color = "#66c2a5", 
#                           value_color = "#3288bd", 
#                           sourceName = "import_graph")
#   })
# })
# 
# observe({
#   req(input$stab1_value_opts)
#   output$cf_topOutbound <- renderPlotly({
#       top_exporting_county(dat_cs %>% filter(nchar(destination) == 5), 
#                           tons_value_selection = input$stab1_value_opts, 
#                           ton_color = "#66c2a5", 
#                           value_color = "#3288bd", 
#                           sourceName = "top_export_partners")
#   })
# })
# 
# observe({
#   req(input$stab1_value_opts)
#   output$totalcounty_text <- renderText({
#       total_tons_or_value(dat_cs, tons_value_selection = input$stab1_value_opts)
#   })
# })
# 
# observe({
#   req(input$stab1_value_opts)
#   
#   bub_temp <- dat_cs %>%
#     mutate(state = ifelse(nchar(origin)==5, str_sub(origin,1,2),str_sub(destination,1,2)))
#   bub_temp$direction[(nchar(bub_temp$origin) == 5 & str_sub(bub_temp$origin,1,2)== bub_temp$destination)|(nchar(bub_temp$destination) == 5 & str_sub(bub_temp$destination,1,2)== bub_temp$origin)] <- "Within"
#   bub_temp$direction[nchar(bub_temp$origin) == 5 & (bub_temp$destination != str_sub(bub_temp$origin,1,2))] <- "Export"
#   bub_temp$direction[nchar(bub_temp$destination) == 5 & (bub_temp$origin != str_sub(bub_temp$destination,1,2))] <- "Import"
#   View(bub_temp %>% filter(is.na(direction)))
#   
#   output$cf_state_trade_bubble_plot <- renderPlotly({
# 
#       state_bubbles(bub_temp, tons_value_selection = input$stab1_value_opts)
#     
#     
#   })
# })
# 
# observe({
#   req(input$stab1_value_opts)
# output$cf_tile_graph <- renderPlotly({
#   #validate(need(try(nrow(dat_cs %>% filter((origin %in% input$stab1_county_opts | destination %in% input$stab1_county_opts))) > 0), 
#   #              message = "Please define a geography on the user selection tab to generate a figure."))
#   #{
#     tile_graph(dat_cs, tons_value_selection = input$stab1_value_opts)
#   #}
#   })
# })
