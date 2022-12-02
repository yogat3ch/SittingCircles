#' body UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_body_ui <- function(id){
  ns <- NS(id)
  bs4Dash::bs4DashBody(
    shinyVirga::browser_ui(),
    uiOutput(ns("bodyui"))
  )
  
}

toggle_login_screen <- function() {
  shinyjs::toggle("login-panel", anim = TRUE)
}


#' body Server Functions
#'
#' @noRd 
mod_body_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    e <- environment()
    
    output$bodyui <- renderUI({
      req(active$ui)
      message("Tab: ", active$tab)
      if (exists(active$server))
        rlang::exec(active$server, id = paste0("body_", active$tab), .env = e)
      # Render the body UIs here
      if (exists(active$ui)) 
        rlang::exec(active$ui, id = paste0(ns("body_"), active$tab))
      
    })
    
    logout_init <- shinyauthr::logoutServer(
      id = "logout",
      active = reactive(credentials()$user_auth)
    )
    
    credentials <- shinyauthr::loginServer(
      id = "login",
      data = db$users,
      user_col = user,
      pwd_col = password,
      cookie_logins = TRUE,
      sessionid_col = sessionid,
      cookie_getter = db_sessionid_get,
      cookie_setter = db_sessionid_set,
      log_out = reactive(logout_init())
    )

    # Login Dialog ----
    # Sat Jun 18 06:50:16 2022
    login_dialog <- shiny::modalDialog(
      fluidRow(
        shinyauthr::loginUI(
          id = ns("login")
        )
      )
      , fluidRow(
        class = "text-center",
        bs4Dash::actionButton(ns("register"), "New User?", status = "primary", class = "m-auto"),
        bs4Dash::actionButton(ns("recover"), "Forgot Password?", status = "primary", class = "m-auto")
      ),
      shinyjs::hidden(
        shiny::div(id = ns("register-panel"),
                   shiny::fluidRow(
                     shiny::wellPanel(
                       style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                       tags$h2("New User Sign-up", class = "text-center"),
                       shiny::textInput(ns("user"), shiny::tagList(shiny::icon("user"), "Username")),
                       shiny::passwordInput(ns("password"), shiny::tagList(shiny::icon("unlock-alt"), "Password")),
                       shiny::passwordInput(ns("password_confirm"), shiny::tagList(shiny::icon("unlock-alt"), "Confirm Password")),
                       shiny::textInput(ns("email"), shiny::tagList(shiny::icon("envelope"), "Email")),
                       shiny::textInput(ns("email_confirm"), shiny::tagList(shiny::icon("envelope"), "Confirm Email")),
                       ui_picker_tz()
                     )
                   ),
        shiny::fluidRow(
          id = "user_submit",
          bs4Dash::actionButton(
            class = "m-auto float-left",
            ns("back_to_login"),
            "Back to Login",
            status = "primary"
          ),
          bs4Dash::actionButton(
            ns("user_submit"),
            "Submit",
            class = "m-auto float-right",
            status = "primary"
          )
        ),
        fluidRow(
          shinyjs::hidden(
            tags$p(id = "user_added", class = "text-success text-center", "User Successfully Added!")
          )
        )
      )
      ),
      shinyjs::hidden(
        shiny::fluidRow(
          id = "recover",
          shiny::wellPanel(
            style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
            tags$h2("Account Recovery", class = "text-center"),
            shiny::textInput(ns("account_recovery"), shiny::tagList(shiny::icon("user"), "Username/Email")),
            bs4Dash::actionButton(
              inputId = ns("recover_submit"),
              label = "Send email",
              icon = shiny::icon("envelope"),
              status = "primary"
            )
          )
        )
      )
      ,
      size = "xl",
      footer = ""
    )
    
    # Registration ----
    # Sat Jun 18 06:51:23 2022
    observeEvent(c(input$register, input$back_to_login), {
      req(input$register || input$back_to_login)
      toggle_login_screen()
      shinyjs::toggle("register-panel", anim = TRUE)
    })
    
    # Validate registration fields
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("user", shinyvalidate::sv_required())
    iv$add_rule("user", ~ if (stringr::str_detect(., "[^[:alnum:]\\_]")) "Must contain only letters, numbers and underscores")
    iv$add_rule("user",
                ~ if (db_user_exists(input$user)) "User already exists. Try resetting your password?"
                )
    
    iv$add_rule("password", shinyvalidate::sv_required())
    iv$add_rule("password_confirm", shinyvalidate::sv_required())
    iv$add_rule("email", shinyvalidate::sv_required())
    iv$add_rule("email", shinyvalidate::sv_email())
    iv$add_rule("email",
                ~ if (db_user_exists(input$email)) "User already exists. Try resetting your password?"
    )
    iv$add_rule("email_confirm", shinyvalidate::sv_required())
    iv$add_rule("email_confirm", shinyvalidate::sv_email())
    iv$add_rule("email_confirm",
                ~ if (input$email != input$email_confirm) "Email addresses must match")
    iv$add_rule("tz", ~ if (input$tz == "") "Timezone is required")
    
    iv$add_rule("password_confirm",
                ~ if (input$password != input$password_confirm) "Passwords must match"
                )
    # Submit and write to DB
    observeEvent(input$user_submit, {
      iv$enable()
      req(iv$is_valid())
      # TODO Handle existing users password recovery?
      result <- db_user_add(input$user, input$password, input$email, input$tz)
      shinyjs::toggle("user_added", asis = TRUE)
      shinyjs::delay(1500, shiny::removeModal())
    })
    
    if (opts$use_login()) {
      observeEvent(credentials()$user_auth, {
        req(credentials()$user_auth)
        shiny::removeModal()
        active$user <- input$`login-user_name`
        times <- user_times(active$user)
        active$times_df <- reactiveVal(
          times
        )
      })
    } 
    
    
    observeEvent(input$loaded, {
      
      if (isTRUE(credentials()$user_auth == FALSE) && opts$use_login())
        shiny::showModal(login_dialog, session = session)
      # This has to be here because it's otherwise hidden.
      toggle_login_screen()
    })
    
    # Password recovery ----
    # Thu May 12 20:47:17 2022
    recover_iv <- shinyvalidate::InputValidator$new()
    recover_iv$add_rule("account_recovery", shinyvalidate::sv_required())
    recover_iv$add_rule("account_recovery", ~ if (!db_user_exists(.)) "No account found. Have you registered?")

    observeEvent(input$recover, {
      browser()
      toggle_login_screen()
      shinyjs::toggle("recover", asis = TRUE, anim = TRUE)
      # TODO Need to have actions for password recovery.
    })
    # call the logout module with reactive trigger to hide/show
    
    shinyVirga::browser_server()
  })
}

## To be copied in the UI
# mod_body_ui("body_1")

## To be copied in the server
# mod_body_server("body_1")
