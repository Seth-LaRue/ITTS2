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
library(rgdal)
library(leaflet.extras)
library(shinyWidgets)
#library(leafgl)
#library(leaflet.extras2)
library(plotly)
library(data.table)

#Initialize Data ----
# 
# dat <- read.csv('data/cnty2cnty_feature_v2.csv', colClass = c("character","character","character",
#                                                       "character",rep("numeric",6)))
# dat_cs <- read.csv('data/cnty2state_feature_v2.csv', colClass = c("character","character","character",
#                                                           "character",
#                                                           rep("numeric",6)))
# dat_ss <- read.csv('data/state2state_feature_v2.csv', colClass = c("character","character","character",
#                                                            "character",
#                                                            rep("numeric",6)))
# dat_pin <- read.csv('data/ports2international_feature.csv', colClass = c("character","character","character",
#                                                                   "character","character",
#                                                                   rep("numeric",4)))
# 
# dat_sin <- read.csv('data/states2international_feature.csv', colClass = c("character","character","character",
#                                                                           "character","character",
#                                                                           rep("numeric",4)))

dat_cs[,5:ncol(dat_cs)] <- lapply(dat_cs[,5:ncol(dat_cs)] ,as.numeric)

# make column names consistent
colnames(dat)[colnames(dat) %in% c("dms_orig", 'dms_dest')] = c("origin",'destination')
colnames(dat_pin)[colnames(dat_pin) %in% c('Tons_2019','Tons_2021','Value_2019','Value_2021')] = c('tons_2019','tons_2021','value_2019','value_2021')
colnames(dat_sin)[colnames(dat_sin) %in% c('Tons_2019','Tons_2021','Value_2019','Value_2021')] = c('tons_2019','tons_2021','value_2019','value_2021')
 
 
source("gral_parameters.R")
source("ini_map_load.R")
source('function/scenario_process.R')
#Load modules ----
source('ui-components/ui_welcome.R', encoding = "utf8")
source('ui-components/ui_maps.R', encoding = "utf8")
source('ui-components/ui_summary.R', encoding = "utf8")
source('ui-components/ui_resources.R', encoding = "utf8")



#UI ----
ui <- fluidPage(
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
    
    # tags$script(
    #   '$("#tabset_maps-ITTSCountytoCountyTrade").on("click", function() { $("#odmap").trigger("shown"); });'
    # )
    
    tags$script(
      '$("#tabset_maps-ITTSCountytoCountyTrade\").click(function(){
                                      $("#odmap").trigger("shown");
                                     });'
    )
    
    # runjs(code = '$("#tabset_maps-ITTSCountytoCountyTrade\").click(function(){
    #                                   $("#odmap").trigger("shown");
    #                                  });')
    
    
  ),
  useShinyjs(),
  
  div(id = "loading-content", class = "overlay-message",
      div(class = 'inner-content',
    fluidRow(class = 'start_loader',
             div(id = 'loading')),
    fluidRow(class = "start_loader",
      h1("Loading the application")
    ),
    fluidRow(class = "start_loader",
      h3("This shouldn't take too long")
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
        ),
        argonSidebarItem(
          tabName = "resources_tab",
          style = "text-align:left",
          icon = img(src='commodity-flow-icon.svg', align = "left", class ="sidebar-icon"),
          "Resources"
        )
      ),
      argonSidebarDivider(),
      argonSidebarHeader(title = "Version 0.0.004")
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

  main_tables <- reactiveValues(table = NULL)
  
  
  onFlushed(function() {
   hide(id = "loading-content", anim = TRUE, animType = "fade")
    runjs('$("body").css("overflow", "auto")')
    runjs('$("#loading-content").css("opacity", .8)')
    
    
    runjs('$("#tab-maps_tabs\").click()')
    runjs('$("#tabset_maps-ITTSCountytoCountyTrade\").click()')
    runjs('$("#tabset_maps-ITTSInternationalTrade\").click()')
    runjs('$("#tab-welcome_tab\").click()')
    

  })
  
  # runjs(code = '$("#tabset_maps-ITTSCountytoCountyTrade\").click(function(){
  #                                     $("#odmap").trigger("shown");
  #                                    });')
  

  
  ##################################################################
  # Server portion for Analysis tab
  source('server-components/server_cty2cty_map.R', local = TRUE)
  source('server-components/server_cty2state_map.R', local = TRUE)
  source('server-components/server_intn_map.R', local = TRUE)
  source('server-components/server_BaselineSummary.R', local = TRUE)
  source('server-components/server_BaselineScenarioComparison.R', local = TRUE)
  
  
}

#Run Tool ---- 
shinyApp(ui = ui, server = server)
