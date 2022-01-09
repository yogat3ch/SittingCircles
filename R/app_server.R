#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  .GlobalEnv$user_profile <- rv(
    first_name = NULL,
    last_name = NULL,
    email = NULL,
    admin = FALSE
  )
  
   active <<- rv()   
   observe({
     active$ui <- paste0("mod_body_", input$active_tab, "_ui")
     active$server <-  paste0("mod_body_", input$active_tab, "_server")
     active$tab <- input$active_tab
     active$dark_mode <- input$dark_mode
   })
   # Top-level Modules
   mod_navbar_server("navbar")
   mod_sidebar_server("sidebar")
   mod_body_server("body")
}
.GlobalEnv$user_profile <- list(admin = FALSE)