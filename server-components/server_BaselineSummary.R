# mod_summary <- function(id, main_tables){
#   moduleServer(
#     id,
#     ## Below is the module function
#     function(input, output, session) {
#       output$cf_modeShareChange <- datatable(main_tables$table)
#     
#       }
#   )
# }

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

modes <- c("Truck"="1",
           "Rail" ="2",
           "Water"="3",
           "Air (Includes truck-air)"="4",
           "Multiple Modes and Mail"="5",
           "Pipeline"="6",
           "Other and Unknown"="7")

modecolors <- data.frame(mode_group = c("Truck", "Rail", "Water", "Air (Includes truck-air)", "Mutliple Modes and Mail", "Pipeline", "Other and Unknown"),  
                         color = c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#66c2a5"))

commcolors <- data.frame(commodities = c("Agriculture and Fish",
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

# ctc_df <- dat
# cts_df <- dat_cs
# tons_value_selection <- "tons_2017"
# county <- "01001"
# ton_color = "#66c2a5"
# value_color = "#3288bd"

#import graph
top_import_partners <- function(ctc_df, cts_df, tons_value_selection, county, ton_color, value_color, saveName, saveWidth, saveHeight, sourceName){

  #first: filter, group, and summarize Transearch data based on user selections
  ctc_temp <- ctc_df %>%
    filter(dms_dest == county) %>%
    rename(factor_lab = tons_value_selection) %>%
    group_by(dms_orig, dms_dest) %>%
    summarise(factor_lab = sum(factor_lab, na.rm= T)) %>%
    left_join(county_selected, by=c("dms_orig"="GEOID"))

  cts_temp <- cts_df %>%
    filter(destination == county) %>%
    rename(factor_lab = tons_value_selection) %>%
    group_by(origin, destination) %>%
    summarise(factor_lab = sum(factor_lab, na.rm= T))
  
  #then, make graph using filtered data. Graph differs for tons vs. value 
    ctc_import_plot <- ctc_temp %>% plot_ly(type = "bar", color= I(ton_color), x = ~reorder(dms_orig, desc(factor_lab)), y = ~factor_lab, 
                              
                              hovertemplate = ~paste("%{label} <br>Tons:", round(factor_lab/1000, digits = 1), "Thousand <extra></extra>"), 
                              text = ~reorder(dms_orig, desc(factor_lab)), 
                              textposition = "none") %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = paste0(tons_value_selection)), autosize = T) %>% 
      config(displaylogo = FALSE, 
             modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
             # toImageButtonOptions= list(filename = saveName,
             #                            width = saveWidth,
             #                            height =  saveHeight)
             )
    
    cts_import_plot <- cts_temp %>% plot_ly(type = "bar", color= I(ton_color), x = ~reorder(origin, desc(factor_lab)), y = ~factor_lab, 
                                            
                                            hovertemplate = ~paste("%{label} <br>Tons:", round(factor_lab/10000, digits = 1), "Thousand <extra></extra>"), 
                                            text = ~reorder(origin, desc(factor_lab)), 
                                            textposition = "none") %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = paste0(tons_value_selection)), autosize = T) %>% 
      config(displaylogo = FALSE, 
             modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
             # toImageButtonOptions= list(filename = saveName,
             #                            width = saveWidth,
             #                            height =  saveHeight)
             )
  
  return(ctc_import_plot, cts_import_plot)
  
}

top_export_partners <- function(ctc_df, cts_df, tons_value_selection, county, ton_color, value_color, saveName, saveWidth, saveHeight, sourceName){
  
  ctc_temp <- ctc_df %>%
    filter(dms_orig == county) %>%
    rename(factor_lab = tons_value_selection) %>%
    group_by(dms_orig, dms_dest) %>%
    summarise(factor_lab = sum(factor_lab, na.rm= T))
  
  cts_temp <- cts_df %>%
    filter(origin == county) %>%
    rename(factor_lab = tons_value_selection) %>%
    group_by(origin, destination) %>%
    summarise(factor_lab = sum(factor_lab, na.rm= T))
  
  #then, make graph using filtered data. Graph differs for tons vs. value 
  ctc_export_plot <- ctc_temp %>% plot_ly(type = "bar", color= I(ton_color), x = ~reorder(dms_dest, desc(factor_lab)), y = ~factor_lab, 
                                          
                                          hovertemplate = ~paste("%{label} <br>Tons:", round(factor_lab/1000, digits = 1), "Thousand <extra></extra>"), 
                                          text = ~reorder(dms_dest, desc(factor_lab)), 
                                          textposition = "none") %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = paste0(tons_value_selection)), autosize = T) %>% 
    config(displaylogo = FALSE, 
           modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
           # toImageButtonOptions= list(filename = saveName,
           #                            width = saveWidth,
           #                            height =  saveHeight)
    )
  
  cts_export_plot <- cts_temp %>% plot_ly(type = "bar", color= I(ton_color), x = ~reorder(destination, desc(factor_lab)), y = ~factor_lab, 
                                          
                                          hovertemplate = ~paste("%{label} <br>Tons:", round(factor_lab/10000, digits = 1), "Thousand <extra></extra>"), 
                                          text = ~reorder(destination, desc(factor_lab)), 
                                          textposition = "none") %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = paste0(tons_value_selection)), autosize = T) %>% 
    config(displaylogo = FALSE, 
           modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
           # toImageButtonOptions= list(filename = saveName,
           #                            width = saveWidth,
           #                            height =  saveHeight)
    )
  
  return(ctc_export_plot, cts_export_plot)
}

mode_pie_graph <- function(ctc_df, cts_df, tons_value_selection, county, modecolors, saveName, saveWidth, saveHeight, sourceName){
  #arguments: cf_db = Transearch database, yrSelection = Year, flowUnit = Tons or Value,
  #counties = counties selected by the user;
  #modecolors = data frame with two columns ("Mode_Group" and "color") that crosswalks between modes and their color on the graph
  #saveName, saveWidth, saveHeight = arguments passed to the Plotly configu function to customize file saving
  
  
  mode_df <- ctc_df %>% dplyr::filter(dms_orig %in% county | dms_dest %in% county) %>%
    dplyr::rename(factor_lab = tons_value_selection) %>% 
    dplyr::group_by(dms_mode) %>% dplyr::summarise(factor_lab = sum(factor_lab, na.rm = TRUE)) %>%
    rbind(data.frame(dms_mode = c("1","2","3","4","5","6","7"), factor_lab = rep(0,7))) %>% 
    dplyr::mutate(dms_mode = factor(dms_mode,labels = modes))
  
  #assign colors - sample data base retained in commented-out code below. 
  # modecolors <- data.frame(Mode_Group = c("Air", "Rail Carload", "Rail Intermodal", "Other", "Pipeline", "Rail (Not Elsewhere Classified)", "Truck", "Water"), 
  # color = c("#f2663d", "#81CAF8", "#A3171C", "#FFFFFF", "#F2ABAF", "#EC4500", "#002236", "#195C99"))
  mode_df <- merge(mode_df, modecolors, all.x = TRUE, all.y = FALSE)
  
    mode <- mode_df %>% plot_ly(source = "sourceName") %>% 
      add_pie(values = ~factor_lab, labels = ~mode_group, automargin = TRUE, marker = list(colors = ~color, line = list(color = "#595959", width = 1)),
                                                                 hovertemplate = ~paste("%{label} <br>: ", round(factor_lab, digits = 1), "</br> %{percent} <extra></extra>"), 
                                                                 text = ~mode_group, key=~mode_group, hole = 0.6, #textfont = list(family = "Arial"), 
                                                                 textposition = "outside") %>%
      layout(#font = list(family = "Arial, \"Source Sans Pro\", \"Helvetica Neue\", Helvetica, sans-serif", color = "#333"),
        showlegend = FALSE, autosize = T, 
        annotations = list(text = HTML(paste0(tons_value_selection, "</i>")), "showarrow"=F)) %>% 
      config(displaylogo = FALSE, 
             modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
             # toImageButtonOptions= list(filename = saveName,
             #                            width = saveWidth,
             #                            height =  saveHeight)
             )

  return(mode)
}

commodity_pie_graph <- function(ctc_df, cts_df, tons_value_selection, county,  commcolors, saveName, saveWidth, saveHeight, sourceName){
  #arguments: cf_db = Transearch database, yrSelection = Year, flowUnit = Tons or Value,
  #counties = counties selected by the user; commcolors = data.frame with two columns (named "color" and "Aggregate.Commodity.Group")
  #that crosswalks between the commodity groups and their color on the graph.
  #saveName, saveWidth, saveHeight = arguments passed to the Plotly configu function to customize file saving
  
  #need to treat direction, mode, and commodity graphs as special if the user is looking at this from statewide.
  #if counties includes all missouri counties, these datasets will be filtered to include all regions in Transearch, thus including Through flows
  #if fewer than all counties are selected, this will eliminate all through flows
  # if(all(sort(unique(cf_db$Origin.Region.Name[cf_db$Origin.State == "Texas"])) %in% counties)){
  #   counties <- sort(unique(cf_db$Destination.Region.Name))
  #   cf_db$Direction[cf_db$Direction == "Outside or Through"] <- "Through"
  # } 
  # OLD VERSION
  # comm_db <- cf_db %>% dplyr::filter(Origin.Region.Name %in% counties | Destination.Region.Name %in% counties) %>% 
  #   dplyr::filter(Year == as.numeric(yrSelection )) %>%  dplyr::group_by(Aggregate.Commodity.Group) %>% dplyr::summarise(tons = sum(Tons, na.rm = TRUE), value = sum(Value, na.rm = TRUE)) 
  # 
  comm_db <- ctc_df %>% ungroup() %>% 
    dplyr::filter(dms_orig %in% county | dms_dest %in% county) %>% 
    rename(factor_lab = tons_value_selection) %>%  
    dplyr::group_by(Grouped_sctg2) %>% 
    dplyr::summarise(factor_lab = sum(factor_lab, na.rm = TRUE)) %>% ungroup()
  
  a <- c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4",
         "#66c2a5","#3288bd", "#ec4500", "#195c99","#f69081", "#a3171c")
  #comm_db <- merge(comm_db, commcolors, all.x = TRUE, all.y = TRUE)
  
  comm_db$color <- a[1:nrow(comm_db)]
  comm_db$wrapped.Comm.Grp <- sapply(comm_db$Grouped_sctg2, 
                                     FUN = function (x){paste(strwrap(x, width = 30), collapse = "<br>")}) #taken from https://stackoverflow.com/questions/35637404/wrapping-long-axis-labels
  
  

    comm <- comm_db %>% plot_ly(source = "sourceName") %>% add_pie(labels = ~wrapped.Comm.Grp, values = ~factor_lab, automargin = TRUE, #text = ~Aggregate.Commodity.Group, 
                                                                 key = ~wrapped.Comm.Grp, hole = 0.6, sort = TRUE, direction = "clockwise",
                                                                 hovertemplate = ~paste("%{label} <br>: ", round(factor_lab/1000000, digits = 1), "Million </br> %{percent} <extra></extra>"), 
                                                                 marker = list(colors = ~color, line = list(color = "#595959", width = 1)), #textfont = list(family = "Arial", size = 10), 
                                                                 textposition = "none") %>%
      layout(#font = list(family = "Arial, \"Source Sans Pro\", \"Helvetica Neue\", Helvetica, sans-serif", color = "#333"),
        showlegend = TRUE, autosize = T, 
        annotations = list(text = HTML(paste0( tons_value_selection, "</i>")), "showarrow"=F)) %>% 
      config(displaylogo = FALSE, 
             modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
             # toImageButtonOptions= list(filename = saveName,
             #                            width = saveWidth,
             #                            height =  saveHeight)
             )

  return(comm)
  
}

#function for displaying total tons or value
total_tons_or_value <- function(cf_db, yrSelection, flowUnit, counties){
  #arguments: cf_db = Transearch database, yrSelection = Year, flowUnit = Tons or Value,
  #counties = counties selected by the user; 
  
  #need to treat direction, mode, and commodity graphs as special if the user is looking at this from statewide.
  #if counties includes all missouri counties, these datasets will be filtered to include all regions in Transearch, thus including Through flows
  #if fewer than all counties are selected, this will eliminate all through flows
  # if(all(sort(unique(cf_db$Origin.Region.Name[cf_db$Origin.State == "Texas"])) %in% counties)){
  #   counties <- sort(unique(cf_db$Destination.Region.Name))
  #   cf_db$Direction[cf_db$Direction == "Outside or Through"] <- "Through"
  # } 
  
  tot_df <- cf_db %>% dplyr::filter(Origin.Region.Name %in% counties | Destination.Region.Name %in% counties) %>%
    dplyr::filter(Year == as.numeric(yrSelection )) %>% 
    dplyr::summarise(tons = sum(Tons, na.rm = TRUE), value = sum(Value, na.rm = TRUE))
  
  if(flowUnit == "Tons"){
    text <- paste0("Total: ", prettyNum(plyr::round_any(tot_df$tons/1000, accuracy = 1), big.mark = ","), " Thousand Tons ","(", yrSelection,")")
  }else if(flowUnit == "Value"){
    text <- paste0("$",prettyNum(plyr::round_any(tot_df$value/1000000, accuracy = 1), big.mark = ","), " Million (" ,yrSelection,")")
  }
  
  return(text)
}


