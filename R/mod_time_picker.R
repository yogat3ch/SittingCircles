#' time_picker UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_time_picker_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyVirga::browser_ui(),
    ui_row(
      ui_row(
        tags$h4("Preferred times to meditate"),
        tags$em("All times in 24hr format.")
      ,
      box = FALSE),
      ui_row(
        col_10(
          uiOutput(ns("time_picker"))
        ),
        col_2(
          bs4Dash::actionButton(
            ns("add_time"),
            "Add more times",
            icon = shiny::icon("plus")
          )
        )
        , box = FALSE
      ),
      ui_row(
        bs4Dash::actionButton(
          ns("save_time"),
          "Save",
          shiny::icon("floppy-disk"),
          size = "lg",
          width = "100%"
        ),
        box = FALSE
      )
    )
  )
}

ui_picker <- function(id_suffix = 1, day = NULL, begin = NULL, end = NULL, session = shiny::getDefaultReactiveDomain()) {
  ns <- session$ns
  ui_row(
    box = FALSE,
    col_4(
      shinyWidgets::virtualSelectInput(
        inputId = ns(paste0("day", id_suffix)),
        label = "Day of week",
        choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
        selected = day
      )
    ),
    col_4(
      shinyTime::timeInput(ns(paste0("begin", id_suffix)),
                           "Beginning",
                           seconds = FALSE,
                           minute.steps = 5,
                           value = begin
                           )
    ),
    col_4(
      shinyTime::timeInput(ns(paste0("end", id_suffix)),
                           "End",
                           seconds = FALSE,
                           minute.steps = 5,
                           value = end)
    )
  )
}
.time_segs <- c("day", "begin", "end")
ui_picker_gatherer <- function(max_inputs = 20, session = shiny::getDefaultReactiveDomain()) {
  reactive({
    i = 1
    out <- list()
    while (!is.null(session$input[[paste0("day", i)]])) {
      out[[i]] <- tibble::tibble_row(!!!purrr::map(rlang::set_names(paste0(.time_segs, i), .time_segs), ~{
        out <- session$input[[.x]]
        if (lubridate::is.POSIXlt(out))
          out <- lubridate::force_tz(out, tzone = "UTC")
        }))
      i <- i + 1
    }
    if (rlang::is_empty(out)) {
      time <- lubridate::today(tzone = "UTC")
      lubridate::hour(time) <- 4
      lubridate::minute(time) <- 0
      lubridate::second(time) <- 0
      out <- tibble::tibble_row(day = "Monday", begin = time, end = time)
    }
      
    tibble::rownames_to_column(dplyr::bind_rows(out), var = "i")
  })
  
}
#' time_picker Server Functions
#'
#' @noRd 
mod_time_picker_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Load saved times
    
    initial_inputs <- 1
    max_inputs <- 20
    times <- ui_picker_gatherer()
    time_save <- reactiveVal()
    time_pickers <- eventReactive(input$add_time, {
      
      total_inputs <- initial_inputs + (input$add_time %||% 0)
      
      if (total_inputs > 20) {
        shinyWidgets::sendSweetAlert(
          title = "Max times reached",
          type = "error"
        )
        total_inputs = 20
      }
      out <- tagList()
      
      for (i in initial_inputs:total_inputs) {
        out <- tagAppendChild(out,
                              ui_picker(id_suffix = i,
                                        day = times()$day[i] %|try|% NULL,
                                        begin = times()$begin[i] %|try|% NULL,
                                        end = times()$end[i] %|try|% NULL)
        )
      }
      out
    }, ignoreInit = FALSE)
    output$time_picker <- renderUI({
      time_pickers()
    })
    shinyVirga::browser_server()
  })
}
    
## To be copied in the UI
# mod_time_picker_ui("time_picker_1")
    
## To be copied in the server
# mod_time_picker_server("time_picker_1")
