#' theme UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_theme_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' theme Server Functions
#'
#' @noRd 
mod_theme_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_theme_ui("theme_1")
    
## To be copied in the server
# mod_theme_server("theme_1")

css_path <- file.path("inst", "app", "www", "css")
crssp_sass_bundle <- sass::sass_bundle(
  sass::sass_layer_file(file.path(css_path, "custom.scss"))
)

do_sass <- function (x) {
  sass::sass(
    x,
    options = sass::sass_options(output_style = "compressed"),
    output = file.path(css_path, "custom.min.css")
  )
}

do_sass(crssp_sass_bundle)