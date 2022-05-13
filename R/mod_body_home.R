#' body_home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    ui_row(
      uiOutput(ns("times"))
    )
    
  )
}
    
#' body_home Server Functions
#'
#' @noRd 
mod_body_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$times <- shiny::renderUI({
      ui_picker_time(ns = ns)
    })
  })
}
    
## To be copied in the UI
# mod_body_home_ui("body_home_1")
    
## To be copied in the server
# mod_body_home_server("body_home_1")
