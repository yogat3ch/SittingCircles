
 

ui_picker_tz <- function(ns = rlang::caller_env()$ns) {
  shinyWidgets::pickerInput(ns("tz"),
                        "Timezone",
                        choices = OlsonNames(),
                        selected = "",
                        options = shinyWidgets::pickerOptions(
                          liveSearch = TRUE,
                          liveSearchStyle = "contains"
                        ))
                 
}



#' @title A default full width row box.
#' @inheritParams bs4Dash::box
#' @return A full-width \link[bs4Dash]{box} nested in a row
#' @export
#'
#' @examples
#' ui_row(tags$p("Hi"))
ui_row <- function(...,
                   title = NULL,
                   footer = NULL,
                   status = NULL,
                   solidHeader = FALSE,
                   background = NULL,
                   width = 12,
                   height = NULL,
                   collapsible = TRUE,
                   collapsed = FALSE,
                   closable = FALSE,
                   maximizable = FALSE,
                   icon = NULL,
                   gradient = FALSE,
                   boxToolSize = "sm",
                   elevation = NULL,
                   headerBorder = TRUE,
                   label = NULL,
                   dropdownMenu = NULL,
                   sidebar = NULL,
                   id = NULL,
                   box = TRUE
) {
  .dots <- suppressWarnings(rlang::dots_list(...))
  .args <- list(title = title,
                footer = footer,
                status = status,
                solidHeader = solidHeader,
                background = background,
                width = width,
                height = height,
                collapsible = collapsible,
                collapsed = collapsed,
                closable = closable,
                maximizable = maximizable,
                icon = icon,
                gradient = gradient,
                boxToolSize = "sm",
                elevation = elevation,
                headerBorder = headerBorder,
                label = label,
                dropdownMenu = dropdownMenu,
                sidebar = sidebar,
                id = id)
  
  if (UU::is_legit(.dots)) {
    out <- shiny::fluidRow(class = "ui_row", eval(
      rlang::call2(
        purrr::when(box, . ~ bs4Dash::box, ~ shiny::tagList),
        !!!purrr::when(box,. && UU::is_legit(.dots) ~ append(.args, .dots), . ~ .args,  ~ .dots)
      )
    ))
  } else
    out <- NULL
  
  out
}



use_login <- function(x = getOption("use_login", FALSE)) {
  x
} 