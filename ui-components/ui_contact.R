# Module UI function
mod_contact_ui <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  argonDashBody(
    h2("Let's talk!"),
    p("There are several ways that you can reach out to the team. If you have general questions about the application or are interested in piloting this software in your community, don't hesitate to get in touch with the project team directly. If you\'re interested in contacting the developers to flag an issue or anything else, please reach out through GitHub."),
    fluidRow(
      argonCard(
        status = "primary",
        width = 6,
        title = h4("Contact the Project Team"),
        hover_lift = FALSE,
        shadow = TRUE,
        # btn_text = "Contact",
        icon = argonIcon("email-83"),
        # src =a(href="mailto:dpatterson@camsys.com"),
        "If you'd like to contact the project team, please send an email to project_team@future_email.com"
      ),
      argonCard(
        status = "primary",
        width = 6,
        title = h3("Contact the Developers"),
        hover_lift = FALSE,
        shadow = TRUE,
        btn_text = "GitHub",
        icon = icon("github"),
        src = "https://github.com/xixiant/FHWA-community",
        "If you like to flag an issue or contact the developers of this application, please reach out through GitHub."
      )
    )
    
  ) 
}
