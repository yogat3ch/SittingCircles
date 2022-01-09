#' navbar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_navbar_ui <- function(id){
  ns <- NS(id)
  bs4Dash::bs4DashNavbar(
    title = "SittingCircles",
    rightUi =  bs4Dash::bs4DropdownMenu(
      badgeStatus = "info",
      type = "messages",
      bs4Dash::messageItem(
        from = NULL,
        "My Profile",
        icon = shiny::icon("id-card"),
        inputId = ns("profile")
      ),
      bs4Dash::messageItem(
        from = NULL,
        "My Times",
        icon = shiny::icon("clock"),
        inputId = ns("times")
      )
    )
  )
}
    
#' navbar Server Functions
#'
#' @noRd 
mod_navbar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$profile, {
      shiny::showModal(
        modalDialog(
          title = "My Profile",
          fluidRow(
            bs4Dash::column(6,
                            shiny::textInput("first_name",
                                             "First Name",
                                             width = "100%")),
            bs4Dash::column(6,
                            shiny::textInput("last_name",
                                             "Last Name",
                                             width = "100%"))
          ),
          fluidRow(
            bs4Dash::column(6,
                            shiny::textInput("email",
                                             "Email Address",
                                             width = "100%")),
            bs4Dash::column(6, class = "text-center",
                            bs4Dash::actionButton("profile_save",
                                                  "Save Profile",
                                                  style = "margin-top: 30px;",
                                                  icon = shiny::icon("save"),
                                                  width = "200px",
                                                  status = "primary"
                            ))
            
          )
        )
      )
    })
    golem::invoke_js("clickon", "#navbar-profile")
  })
}
    
## To be copied in the UI
# mod_navbar_ui("navbar_1")
    
## To be copied in the server
# mod_navbar_server("navbar_1")
