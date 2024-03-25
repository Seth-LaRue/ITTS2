#Load Libraries ----
library(shiny)
library(argonR)
library(argonDash)
library(leaflet)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)
library(sf)
library(shinyjs)
library(DT)
#library(shinyalert)
#library(rgdal)
library(leaflet.extras)
library(shinyWidgets)
#library(leafgl)
#library(leaflet.extras2)
library(plotly)
library(data.table)
library(networkD3)
library(waiter)
#Initialize Data ----



#load(file = "ITTS_Initial_Data_03112024_v1_6.Rdata")

# ITTS_base <- state_base %>%
#   mutate(NAME = ifelse(GEOID %in% c("05", "12","13","21","22","28","29","45","48","51"),'ITTS',NAME),
#          GEOID = ifelse(GEOID %in% c("05", "12","13","21","22","28","29","45","48","51"),'ITTS',GEOID)) %>%
#   group_by(NAME, GEOID) %>%
#   summarise(NAME = unique(NAME),
#             GEOID = unique(GEOID))%>%
#   ungroup()

# ITTS_boundary <- ITTS_base %>% filter(GEOID == 'ITTS') %>%
#   select('GEOID','NAME') %>%
#   mutate(type = '',
#          mode_nm = '')
# 
# all_selected = rbind(all_selected,ITTS_boundary)

# SE_base <- state_base %>%
#   mutate(NAME = ifelse(GEOID %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"),'Southeast Region',NAME),
#          GEOID = ifelse(GEOID %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"),'Southeast Region',GEOID)) %>%
#   group_by(NAME, GEOID) %>%
#   summarise(NAME = unique(NAME),
#             GEOID = unique(GEOID))%>%
#   ungroup()

# SE_boundary <- SE_base %>% filter(GEOID == 'Southeast Region') %>%
#   select('GEOID','NAME') %>%
#   mutate(type = '',
#          mode_nm = '')
# 
# all_selected = rbind(all_selected,SE_boundary)
#
# other_states = state_base %>%
#   filter(!(GEOID %in% c("05", "12","13","21","22","28","29","45","48","51","01","47","37"))) %>%
#   select(-'state_lab',-'STATEFP') %>%
#   mutate(type = "",
#          mode_nm = "")
# 
# all_selected <- rbind(all_selected,other_states)  ## add all geographical boundaries to this layer, used in graphs.

# international_base <- international_base %>%
#   mutate(type = "",
#          mode_nm = "")
# all_selected <- rbind(all_selected,international_base)
# all_selected <- st_transform(all_selected, crs = st_crs(4326))
# county_selected <- st_transform(county_selected, crs = st_crs(4326))

# this is for hatch pattern on ITTS and SE_hatch
# ITTS_hatch <- HatchedPolygons::hatched.SpatialPolygons(ITTS_boundary, density = 1, angle = c(45, 135))
# SE_hatch <- HatchedPolygons::hatched.SpatialPolygons(SE_boundary, density  = 1, angle = c(45, 135))
#remove(ITTS_boundary)
#remove(SE_boundary)
#remove(other_states)

#Source scripts ----
source("gral_parameters.R")
source("ini_map_load.R")
source('function/scenario_process_v3.R')
source('function/scenario_process_v2.R')

#Load modules ----
source('ui-components/ui_welcome.R', encoding = "utf8")
source('ui-components/ui_maps.R', encoding = "utf8")
source('ui-components/ui_summary.R', encoding = "utf8")
source('ui-components/ui_resources.R', encoding = "utf8")

#This was missing ----
show_waiter_message <- function(){
  runjs('$("#waiter-message" ).removeClass(\'d-none\');')
  runjs('$("#waiter-inner-content" ).removeClass(\'d-none\');')
  runjs('$("#loading_ongoing" ).removeClass(\'d-none\');')
  runjs('$("#loading_ongoing").show();')
  runjs('$("#waiter-message").show();')
}

flush_waiter_message <- function(){
  runjs('$("#waiter-message").hide()')
}

#UI ----
ui <- fluidPage(
  use_waiter(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    
    #tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))
    tags$style(type="text/css", "#download_cc {background-color:#16b7ee;border: none}"),
    tags$style(type="text/css", "#download_cs {background-color:#16b7ee;border: none}"),
    tags$style(type="text/css", "#download_in {background-color:#16b7ee;border: none}"),
    
    tags$style(
      ".map-container {
          height: 100%;
          width: 100%;
          position: relative;
        }",
      ".map-loading {
          position: absolute;
          display: flex;
          justify-content: center;
          align-items: center;
          width: 100%;
          height: 500px;
          background-color: transparent;
          text-align: center;
        }"
    ),

    tags$script(HTML("
    $(document).ready(function() {
      var counter = 0;
      var interval = setInterval(function() {
        counter++;
        $('#counter').text(counter + ' seconds');
        if (counter >= 60) {
          clearInterval(interval);
        }
      }, 1000);
    });
  ")),
  
  tags$link(rel = "icon", href = "commodity-flow-icon.svg", type = "image/x-icon")
  
  
  ),
  useShinyjs(),
  tags$div(id = "waiter-message", class = "overlay-message d-none",
           div(class = 'inner-content d-none', 
               id = 'waiter-inner-content',
               fluidRow(class = 'start_loader',
                        div(id = 'loading')),
               fluidRow(class = "start_loader mt-1",
                        h1("Loading..."),
               )
           )
  ),
  div(id = "loading-content", class = "overlay-message",
      div(class = 'inner-content',
          fluidRow(class = 'start_loader',
                   div(id = 'loading')),
          fluidRow(class = "start_loader mt-1",
                   h1("Loading the application")
          ),
          fluidRow(class = "start_loader",
                   h3("This should take approximately 20-40 seconds.")
          ),
          fluidRow(class = "start_loader",
                   h4("Elapsed time:  "),
                   h4(id = "counter", "0 seconds")
          )
      )),
  
  
  argonDashPage(
    title = "SETTS data visualization tool",
    author = "Cambridge Systematics",
    
    sidebar = argonDashSidebar(
      vertical = TRUE,
      skin = "light",
      background = "white",
      size = "md",
      side = "left",
      id = "my_sidebar",
      brand_url = "https://camsys.com/",
      
      brand_logo = "https://upload.wikimedia.org/wikipedia/commons/thumb/7/71/CS_LOGO_.png/220px-CS_LOGO_.png",
      
      argonSidebarHeader(title = "SETTS Visualization Tool"),
      argonSidebarMenu(
        argonSidebarItem(
          tabName = "welcome_tab",
          style = "text-align:left",
          icon = icon("handshake"),
          "Welcome"
        ),
        argonSidebarItem(
          tabName = "maps_tabs",
          style = "text-align:left",
          icon = icon("map"),
          "Scenario Explorer"
        ),
        argonSidebarItem(
          tabName = "summary_tab",
          style = "text-align:left",
          icon = icon("atom"),
          "Scenario Analyzer"
        )
      ),
      argonSidebarDivider(),
      argonSidebarHeader(title = "Version 1.0.000")
    ),
    
    header = 
      argonDashHeader(
        gradient = TRUE,
        bottom_padding = 3,
        top_padding = 5,
        color = "primary",
        separator = T,
        separator_color = "secondary",
        height = 30,
        h2('SETTS data visualization tool'),
      ),
    
    body = argonDashBody(
      
      argonTabItems(
        argonTabItem(
          tabName = "welcome_tab",
          mod_welcome_ui("welcome_ui")
        ),
        
        domestic_tab,
        
        argonTabItem(
          tabName = "summary_tab",
          summary_tab
          ),
          
          argonTabItem(
            tabName = "resources_tab",
            mod_resources_ui("resources_ui")
          )
        
      )
    ),
    footer = argonDashFooter(
      copyrights = "SETTS, Cambridge Systematics, 2022",
      src = "https://camsys.com/"
    )
  )
)   


#Server ----
server <- function(input, output, session) {

  #main_tables <- reactiveValues(table = NULL)#What is this for?
  
  
  onFlush(function(){
    runjs('
          $("#tab-maps_tabs").click();
          //$("#tabset_maps-ITTSInternationalTrade-tab").click();
          //$("#tabset_maps-ITTSCountytoCountyTrade-tab").click();
          $("#tabset_maps-ITTSCountyStatetoStateTrade-tab").click();
          
          $("#tabset_maps-ITTSCountytoCountyTrade-tab").click(function(){$("#odmap").trigger("shown");});
          $("#tabset_maps-ITTSCountyStatetoStateTrade-tab").click(function(){$("#odmap_cs").trigger("shown");});
          //$("#tabset_maps-ITTSInternationalTrade-tab").click(function(){$("#odmap_in").trigger("shown");}); #add this back for international trade remove this comment!

          ')
    flush_waiter_message()
  })
  
  onFlushed(function() {
    runjs('
          //document.querySelector("#tabset_maps-ITTSInternationalTrade").classList.remove("active");
          document.querySelector("#tabset_maps-ITTSCountyStatetoStateTrade").classList.remove("active");
          $("#tab-welcome_tab").click();
          $("body").css("overflow", "auto");
          ')
    hide(id = "loading-content", anim = TRUE, animType = "fade")
  })

  
  ##################################################################
  # Server portion for Analysis tab
  source('server-components/server_cty2cty_map.R', local = TRUE)
  source('server-components/server_cty2state_map.R', local = TRUE)
  #source('server-components/server_intn_map.R', local = TRUE)
  source('server-components/server_BaselineSummary.R', local = TRUE)
  source('server-components/server_BaselineScenarioComparison.R', local = TRUE)
  
  
}

#Run Tool ---- 
shinyApp(ui = ui, server = server)
