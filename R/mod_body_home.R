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
    mod_time_picker_ui(ns("times"))
  )
}
    
#' body_home Server Functions
#'
#' @noRd 
mod_body_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_time_picker_server("times")
    
  })
}
    
## To be copied in the UI
# mod_body_home_ui("body_home_1")
    
## To be copied in the server
# mod_body_home_server("body_home_1")
