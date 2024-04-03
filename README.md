# Freight Flow Forecasting and Scenario Planning Tool

Version: 1.00.000  
Date: 2023-03-24  
Developer: Cambridge Systematics

## Introduction

This data visualization tool was funded by the Institute for Trade and Transportation Studies (ITTS) during the development of the Southeast Trade and Transportation Study (SETTS). The Freight Flow Forecasting and Scenario Planning Tool provides an accessible way to explore and download commodity flow data for the 10-state ITTS region Arkansas, Florida, Georgia, Kentucky, Louisiana, Mississippi, Missouri, South Carolina, Texas, and Virginia. This platform allows users to better understand intraregional, domestic, and international freight flows to, from, and within the ITTS region, providing detail on volumes, values, commodities, modes, and trading partners.

Live Demo of tool here: [https://camsys.shinyapps.io/ITTS_Data_Tool](https://camsys.shinyapps.io/ITTS_Data_Tool)

## Files contained in the Shiny App

1. ITTS_Data_Tool.Rproj - RStudio project file
2. ITTS_Tool.R - Main R script to run the Freight Flow Forecasting and Scenario Planning Tool Shiny app
3. gral_parameters.R - Global configuration file for the app, containing global settings
4. ini_map_load.R - Script to initialize map data
5. server-componentsserver_cty2cty_map.R - Server-side script for the county-to-county analysis tab
6. server-componentsserver_cty2state_map.R - Server-side script for the county-to-state, or state-to-state analysis tab
7. server-componentsserver_intn_map.R - Server-side script for the international map
8. ui-componentsui_maps.R - UI script for the analyses page
9. ui-componentsui_welcome.R - UI script for the welcome page
10. .RData - File containing all necessary data preloaded in the environment
11. www - Directory containing additional files like images, CSS, and JavaScript files for the app
12. README.md - This file, containing instructions and information about the Freight Flow Forecasting and Scenario Planning Tool

## How to run the Freight Flow Forecasting and Scenario Planning Tool

1. Download and install R 4.1.2 from the following link [https://cloud.r-project.org/](https://cloud.r-project.org/)
2. Download and install the latest version of RStudio from the following link [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)
3. Download and extract the zip file containing the Shiny app.
4. Open the ITTS_Data_Tool.Rproj file.
5. Install the required packages using the following command in the R console: "install.packages(c("shiny", "argonR", "argonDash", "leaflet", "tidyverse", "sf", "shinyjs", "DT", "leaflet.extras", "shinyWidgets"))"
6. Open the ITTS_Tool.R file and click on the "Run App" button on the top right corner of the RStudio window, or execute the `runApp()` command in the console.

For more information or support, please contact Cambridge Systematics at [CLamm@camsys.com](mailto:CLamm@camsys.com),  [CLindsey@camsys.com](mailto:CLindsey@camsys.com), or [PSerigos@Camsys.com](mailto:PSerigos@Camsys.com).

