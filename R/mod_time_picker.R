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
      uiOutput(ns("time_picker")),
      footer = fluidRow(
        col_2(
          sliderInput(
            ns("duration"),
            "Default Duration (min)",
            min = 0,
            max = 3 * 60,
            value = 20,
            step = 5
          )
        ),
        col_2(
          
        ),
        col_2(
          bs4Dash::actionButton(
            ns("add_week"),
            "Add full week of last entered times",
            icon = shiny::icon("calendar"),
            width = "100%"
          )
        ),
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
    col_2(
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
    ),
    col_2(
      bs4Dash::actionButton(
        ns(paste0("rm", id_suffix)),
        "",
        icon = shiny::icon("minus"),
        width = "35px"
      ) |> 
        tagAppendAttributes(style = "width:35px;height:36px;margin-top:24px"),
      bs4Dash::actionButton(
        ns(paste0("add", id_suffix)),
        "",
        icon = shiny::icon("plus"),
        width = "35px"
      ) |> 
        tagAppendAttributes(style = "width:35px;height:36px;margin-top:24px")
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
  slider::slide(x, \(.vars) {
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

time_rows <- function(x = 1, day = "Monday", duration, begin, end) {
  time <- lubridate::today(tzone = "UTC")
  lubridate::hour(time) <- 0
  lubridate::minute(time) <- 0
  lubridate::second(time) <- 1
  if (missing(begin))
    begin <- time
  if (missing(end)) {
    if (!missing(duration))
      time <- time + lubridate::minutes(duration)
    end <- time
  }
    
  
  out <- tibble::tibble(day = day, begin = begin, end = end)
  if (x > 1)
    out <- dplyr::bind_rows(
      out,
      out[rep(1, x - 1),]
    )
  return(out)
}

ui_picker_gatherer <- function(max_inputs = 20, session = shiny::getDefaultReactiveDomain()) {
  
    i = 1
    out <- list()
    while (!is.null(session$input[[paste0("day", i)]]) && i <= max_inputs) {
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
  
  
}
#' time_picker Server Functions
#'
#' @noRd 
mod_time_picker_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Load saved times
    
   
    times_max <- 20
    
    
    observeEvent(input$save_time, {
      virgaUtils::dbg_msg("time_picker: save_time")
      t_df <- active$times_df()
      gathered <- ui_picker_gatherer(max_inputs = nrow(t_df))
      gathered <- dplyr::filter(gathered, lubridate::hour(begin) != 0 & lubridate::hour(end) != 0)
      active$times_df(gathered)
      .row <- db_filter(user = active$user)
      .row$times <- time_handler(active$times_df())
      db_row_update(.row)
      shinyVirga::js_after(
        "save_time",
        tags$span("Times successfully saved to database!"),
        status = "success"
      )
      
    })
    
    observeEvent(input$add_time, {
      active$times_df(
        dplyr::bind_rows(
          ui_picker_gatherer(max_inputs = nrow(active$times_df())),
          time_rows(duration = input$duration)
        )
      )
    }, priority = 2)
    observeEvent(input$add_week, {
      len <- nrow(active$times_df())
      times <- ui_picker_gatherer(max_inputs = len)
      days <- stringr::str_subset(UU::week_factor(), times[len,]$day, negate = TRUE)
      active$times_df(
        dplyr::bind_rows(
          times,
          time_rows(
            day = days,
            begin = times[len,]$begin,
            end = times[len,]$end
          )
        )
      )
    }, priority = 2)
    observeEvent(input$subtract_time, {
      active$times_df(
        ui_picker_gatherer(max_inputs = nrow(active$times_df()) - 1)
      )
    }, priority = 2)
    
    time_env <- environment()
    observeEvent(c(input$add_week, input$add_time, input$subtract_time), {
      len <- nrow(active$times_df())
      input_chr <- purrr::walk(1:len, ~{
        inputs <- paste0("input$", c("rm", "add"), .x)
        observeEvent(rlang::parse_expr(inputs[1]), rlang::expr({
          active$times_df(
            active$times_df()[-!!.x, ]
          )
        }), event.env = time_env, event.quoted = TRUE, handler.env = time_env, handler.quoted = TRUE)
        observeEvent(rlang::parse_expr(inputs[2]), rlang::expr({
          active$times_df(
            dplyr::add_row(
              active$times_df(),
              .after = !!.x,
              active$times_df()[!!.x, ]
            )
          )
        }), event.env = time_env, event.quoted = TRUE, handler.env = time_env, handler.quoted = TRUE)
      })
      
    })
    
    output$time_picker <- renderUI({
      req(active$times_df)
      
      picker <- tibble::rownames_to_column(active$times_df(), var = "id_suffix") |> 
        slider::slide(~do.call(ui_picker, .x)) |> 
        tagList()
      tagList(
        tags$em(HTML(glue::glue("All times in 24hr format in the <strong>{.GlobalEnv$user_profile$timezone}</strong> timezone."))),
        ui_row(
          col_12(
            picker
          ),
          box = FALSE
        )
      )
      
    })
    
    shinyVirga::browser_server()
  })
}
    
## To be copied in the UI
# mod_time_picker_ui("time_picker_1")
    
## To be copied in the server
# mod_time_picker_server("time_picker_1")
