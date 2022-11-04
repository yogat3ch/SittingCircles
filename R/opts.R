opts <-
list(use_login = function (default = FALSE) 
getOption("use_login", default = default), use_reprex = function (default = FALSE) 
{
    if (!interactive()) {
        out <- FALSE
    }
    else {
        out <- getOption("use_reprex", FALSE)
    }
    out
}, use_debug = function (default = FALSE) 
getOption("use_debug", default = default), renv.config.auto.snapshot = function (default = TRUE) 
getOption("renv.config.auto.snapshot", default = default), lubridate.week.start = function (default = 1) 
getOption("lubridate.week.start", default = default))
