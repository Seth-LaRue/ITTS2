

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
    left_join(county_choices, by=c("origin"="GEOID")) # Qi: changed from county_choices to al_selected, to apply for all geographic level.
    
  
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
    left_join(all_selected, by=c("origin"="GEOID")) # Qi: changed from county_choices to al_selected, to apply for all geographic level.
  
  if(grepl("value",tons_value_selection)){
    unit = " $Million"
    unit_pre = ""
  } else {
    unit = " K tons"
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
      yaxis = list(title = paste0(str_to_title(str_replace(tons_value_selection,"_", " "))," (K tons)"),tickfont = list(size = 15))) %>% 
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
    left_join(county_choices, by=c("destination"="GEOID")) 
  
  if(grepl("value",tons_value_selection)){
    unit = " $Million"
    unit_pre = ""
  } else {
    unit = " K tons"
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
      yaxis = list(title = paste0(str_to_title(str_replace(tons_value_selection,"_", " ")), " (K tons)")), autosize = T) %>% 
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
    unit = " $Million"
    unit_pre = ""
  } else {
    unit = " K tons"
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
      yaxis = list(title = paste0(str_to_title(str_replace(tons_value_selection,"_", " ")), " (K tons)"),tickfont = list(size = 15))) %>% 
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
    left_join(ini_modecolors %>% mutate(dms_mode = as.character(dms_mode)))
  

  if (length(strsplit(tons_value_selection, "_")[[1]]) == 1) {
    formatted_label <- str_to_title(str_replace(tons_value_selection,"_", " "))
  } else {
    formatted_label <- str_to_title(str_replace_all(tons_value_selection, "_", " "))
  }
  
  if(grepl("Value",formatted_label)){
    unit = " $Million"
    unit_pre = ""
  } else {
    unit = " K tons"
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
    layout(legend = list(font = list(size = 10))) %>%
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
    unit = " $Million"
    unit_pre = ""
  } else {
    unit = " K tons"
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
    unit = " $Million"
    unit_pre = ""
  } else {
    unit = " K tons"
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

