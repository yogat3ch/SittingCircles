#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  menu_items <- tibble::tribble(
    ~ text, ~ icon, ~tabName,
    "Home", "house-user", "home",
    "Circles", "users", "circles"
  )
  if (user_profile$admin) {
    menu_items <- tibble::add_row(menu_items, text = "Admin", icon = "user-cog", tabName = "admin")
  }
  bs4Dash::dashboardSidebar(
    skin = "light",
    collapsed = TRUE,
    bs4Dash::sidebarMenu(
      id = "active_tab",
      .list = purrr::pmap(menu_items, ~{
        .x <- rlang::dots_list(..., .named = TRUE)
        .x$icon <- shiny::icon(.x$icon)
        rlang::exec(bs4Dash::bs4SidebarMenuItem, !!!.x)
      })
    )
  )
  
}
    
#' sidebar Server Functions
#'
#' @noRd 
mod_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_1")
    
## To be copied in the server
# mod_sidebar_server("sidebar_1")

