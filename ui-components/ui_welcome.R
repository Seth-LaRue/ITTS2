# Module UI function
mod_welcome_ui <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  argonPage(
    fluidRow(
      column(12,
             h1("Welcome to the SETTS Data Visualization Tool!"),
             tags$p("This data visualization tool was funded by the Institute for Trade and Transportation Studies (ITTS) during development of the Southeast Trade and Transportation Study (SETTS)."),
             h2("How can you use this tool?"),
             tags$p("The SETTS Data Visualization Tool provides an accessible way to explore and download commodity flow data for the 10-state ITTS region. This platform allows states and their partners to view intraregional, domestic, and international commodity flows to, from, and within the ITTS region with finer granularity."),
             h2("Where did the data come from?"),
             tags$p("The commodity flow data presented in this tool are:"),
             tags$ul(tags$li(p("Based on Federal Highway Administration’s Freight Analysis Framework (FAF) version 5.2.")),
                     tags$li(p("Aggregated into 12 commodity groupings to streamline and simplify navigation and performance.")),
                     tags$li(p("Presented by county within the ITTS region and by state for other domestic trading partners.")),
                     tags$li(p("Supplemented by international trade data from the U.S. Census Bureau through its U.S.A. Trade Online data portal.")),
             ),
             tags$a(#class='left_image',
               img(src = 'ITTS_Logo_Shadow.webp', width='200'),
               href="https://www.ittsresearch.org",
               target="_blank"
             )
             )

      )
    )
}