#Initialize Data ----
source("init.R")

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
library(shinyalert)

#Source scripts ----
print("Sourcing scripts")
source("gral_parameters.R")
source("ini_map_load.R")
source('function/scenario_process_v3.R')
source('function/scenario_process_v2.R')

#Load modules ----
print("Loading UI components")
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
  // Counter for overall time
  
  // initiate the counter_analyzer and analyzerInterval
  let counter_analyzer = 0;
  let analyzerInterval = 0;
  
  var counter = 0;
  var counterInterval = setInterval(function() {
    counter++;
    $('#counter').text(counter + ' seconds');
    if (counter >= 60) {
      clearInterval(counterInterval);
    }
  }, 1000);
});

 $(document).on('click', '#stab2_mainbutt', function() {
 //analyzerInterval = 0;
    counter_analyzer = 0; 
    analyzerInterval = setInterval(function() {
      counter_analyzer = counter_analyzer + 1;
      $('#counter_analyzer').text(counter_analyzer + ' seconds');
      if (counter_analyzer >= 120) {
        clearInterval(analyzerInterval);
      }
  }, 1000);
    });
  ")),
  
  tags$link(rel = "icon", href = "commodity-flow-icon.svg", type = "image/x-icon")
  
  
  ),
  useShinyjs(),
  tags$div(id = "waiter-message", class = "overlay-message",
           div(class = 'inner-content', 
               id = 'waiter-inner-content',
               fluidRow(class = 'start_loader',
                        div(id = 'loading')),
               fluidRow(class = "start_loader mt-1",
                        h1("Loading...")
               ),
               fluidRow(class = "start_loader",
                        h3("This processing may take 10 - 180s"))#,
               # fluidRow(class = "start_loader",
               #          h4("Elapsed time:  "),
               #          h4(id = "counter_analyzer", "0 seconds")
               # )
           )
  ),
  # div(id = "loading-content", class = "overlay-message",
  #     div(class = 'inner-content d-none',
  #         fluidRow(class = 'start_loader',
  #                  div(id = 'loading')),
  #         fluidRow(class = "start_loader mt-1",
  #                  h1("Loading the application")
  #         ),
  #         fluidRow(class = "start_loader",
  #                  h3("This should take approximately 20-40 seconds.")
  #         ),
  #         fluidRow(class = "start_loader",
  #                  h4("Elapsed time:  "),
  #                  br(),
  #                  h4(id = "counter", "0 seconds")
  #         )
  #     )),
  
  
  argonDashPage(
    title = "Freight Flow Forecasting and Scenario Planning Tool",
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
          "Trade Explorer"
        ),
        argonSidebarItem(
          tabName = "summary_tab",
          style = "text-align:left",
          icon = icon("atom"),
          "Scenario Analyzer"
        )
      ),
      argonSidebarDivider(),
      argonSidebarHeader(title = "Version 1.2.000")
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
        h2('Freight Flow Forecasting and Scenario Planning Tool'),
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
  onFlushed(function() {
    runjs('
          //document.querySelector("#tabset_maps-ITTSInternationalTrade").classList.remove("active");
          document.querySelector("#tabset_maps-ITTSCountyStatetoStateTrade").classList.remove("active");
          $("#tab-welcome_tab").click();
          $("body").css("overflow", "auto");
          ')
    hide(id = "loading-content", anim = TRUE, animType = "fade")
  })
  
  onFlush(function(){
    runjs('
          //$("#tab-maps_tabs").click();
          //$("#tabset_maps-ITTSInternationalTrade-tab").click();
          //$("#tabset_maps-ITTSCountytoCountyTrade-tab").click();
          //$("#tabset_maps-ITTSCountyStatetoStateTrade-tab").click();
          
          $("#tabset_maps-ITTSCountytoCountyTrade-tab").click(function(){$("#odmap").trigger("shown");});
          $("#tabset_maps-ITTSCountyStatetoStateTrade-tab").click(function(){$("#odmap_cs").trigger("shown");});
          $("#tabset_maps-ITTSInternationalTrade-tab").click(function(){$("#odmap_in").trigger("shown");}); 

          ')
    flush_waiter_message()
  })
  


  
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
