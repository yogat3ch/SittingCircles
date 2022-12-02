#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  google_auth()
  
  .GlobalEnv$db <- purrr::map(rlang::set_names(c("users", "session_ids")), ~googlesheets4::read_sheet(db_id, sheet = .x))
  
   assign("active", rv(), envir = .GlobalEnv)
  
   observe({
     active$ui <- paste0("mod_body_", input$active_tab, "_ui")
     active$server <-  paste0("mod_body_", input$active_tab, "_server")
     active$tab <- input$active_tab
     active$dark_mode <- input$dark_mode
     
     if (!opts$use_login()) {
       .GlobalEnv$user_profile$user <- active$user <- "stephen"
       .GlobalEnv$user_profile$timezone <- "America/New_York"
       times <- user_times(active$user)
       active$times_df <- reactiveVal(
         times
       )
     } else 
       active$user <- NULL
   })
   # Top-level Modules
   mod_navbar_server("navbar")
   mod_sidebar_server("sidebar")
   mod_body_server("body")
}
.GlobalEnv$user_profile <- list(admin = FALSE)