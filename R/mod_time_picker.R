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
      title = "Preferred times to meditate",
      tags$em("All times in 24hr format."),
      ui_row(
        col_12(
          uiOutput(ns("time_picker"))
        ),
        box = FALSE
      ),
      footer = fluidRow(
        col_6(),
        col_2(
          bs4Dash::actionButton(
            ns("subtract_time"),
            "Subtract a time",
            icon = shiny::icon("minus"),
            width = "100%"
          )
        ),
        col_2(
          bs4Dash::actionButton(
            ns("add_time"),
            "Add another time",
            icon = shiny::icon("plus"),
            width = "100%"
          )
        ),
        col_2(
          bs4Dash::actionButton(
            ns("save_time"),
            "Save",
            shiny::icon("floppy-disk"),
            width = "100%"
          ) 
        )
      )
    )
  )
}

ui_picker <- function(id_suffix = 1, day = "Monday", begin = NULL, end = NULL, session = shiny::getDefaultReactiveDomain()) {
  ns <- session$ns
  ui_row(
    box = FALSE,
    col_4(
      shinyWidgets::virtualSelectInput(
        inputId = ns(paste0("day", id_suffix)),
        label = "Day of week",
        choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
        selected = day
      ) |> 
        htmltools::tagAppendAttributes(class = "day_input", .cssSelector = ".vscomp-toggle-button")
    ),
    col_4(
      htmltools::tagAppendAttributes(shinyTime::timeInput(ns(paste0("begin", id_suffix)),
                                                          "Beginning",
                                                          seconds = FALSE,
                                                          minute.steps = 5,
                                                          value = begin
      ), class = "time_input", .cssSelector = ".input-group > input")
    ),
    col_4(
      shinyTime::timeInput(ns(paste0("end", id_suffix)),
                           "End",
                           seconds = FALSE,
                           minute.steps = 5,
                           value = end) |> 
        htmltools::tagAppendAttributes(class = "time_input", .cssSelector = ".input-group > input")
    )
  )
}
#' Convert between tibble and character representations of time
#'
#' @param x \code{tbl/chr} tbl with columns day, begin, end. chr in the format `"Mon_4:0_4:0"`
#'
#' @return \code{tbl/chr}
#' @export
#'

time_handler <- function(x) {
  UseMethod("time_handler")
}
#' @export
time_handler.data.frame <- function(x) {
  out <- purrr::pmap(x, ~{
    .vars <- list(...)
    glue::glue_collapse(c(substr(.vars$day, 0, 3), purrr::map_chr(
      .vars[c("begin",
              "end")], ~
        paste0(lubridate::hour(.x), ":", lubridate::minute(.x))
    )), sep = "_")
  }) |> 
    glue::glue_collapse(sep = "|")
}
#' @export
time_handler.character <- function(x) {
  times <- stringr::str_split(x, "\\|")[[1]] |> 
    stringr::str_split("\\_")
  week_begin <- lubridate::floor_date(lubridate::today(), "week")
  purrr::map_dfr(times, ~{
    dow <- week_begin + lubridate::days(UU::week_factor(.x[1]) - 1)
      tibble::tibble_row(day = .x[1], !!!rlang::set_names(dow + lubridate::hm(.x[2:3]), c("begin", "end")))
  })
}
.time_segs <- c("day", "begin", "end")

time_rows <- function(x = 1) {
  time <- lubridate::today(tzone = "UTC")
  lubridate::hour(time) <- 0
  lubridate::minute(time) <- 0
  lubridate::second(time) <- 1
  out <- tibble::tibble_row(day = "Monday", begin = time, end = time)
  if (x > 1)
    out <- dplyr::bind_rows(
      out,
      out[rep(1, x - 1),]
    )
  return(out)
}

ui_picker_gatherer <- function(max_inputs = 20, session = shiny::getDefaultReactiveDomain(), e = rlang::caller_env()) {
  reactive({
    i = 1
    out <- list()
    while (!is.null(session$input[[paste0("day", i)]])) {
      out[[i]] <- tibble::tibble_row(!!!purrr::map(rlang::set_names(paste0(.time_segs, i), .time_segs), ~{
        out <- session$input[[.x]]
        if (lubridate::is.POSIXlt(out))
          out <- lubridate::force_tz(out, tzone = "UTC")
        else
          out
        }))
      i <- i + 1
    }
    if (rlang::is_empty(out)) {
      out <- time_rows()
    }
      
    dplyr::bind_rows(out)
  }, env = e)
  
}
#' time_picker Server Functions
#'
#' @noRd 
mod_time_picker_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Load saved times
    
    times_df <- reactiveVal(
      time_rows()
    )
    times_max <- 20
    times <- ui_picker_gatherer()
    
    observeEvent(input$save_time, {
      times_df(times())
      browser()
      # TODO Write all times to the spreadsheet
    })
    observeEvent(input$add_time, {
      times_df(
        dplyr::bind_rows(
          times_df(),
          time_rows()
        )
      )
    })
    observeEvent(input$subtract_time, {
      times_df(
        times_df()[-nrow(times_df()),]
      )
    })
    
    output$time_picker <- renderUI({
      tibble::rownames_to_column(times_df(), var = "id_suffix") |> 
        purrr::pmap(~{
          .x <- list(...)
          do.call(ui_picker, .x)
        }) |> 
        tagList()
    })
    
    shinyVirga::browser_server()
  })
}
    
## To be copied in the UI
# mod_time_picker_ui("time_picker_1")
    
## To be copied in the server
# mod_time_picker_server("time_picker_1")
