# Module UI function
mod_resources_ui <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  argonPage(
    fluidRow(
      column(12,
             h1("Resources"),
             tags$p("To be completed....."),
             # tags$div(class='center_image',
             #   img(src = 'www/screen_shot_map.png', width='933')
             # )
             )

      )
    )
}