
#' Fetch user times or return a default time
#'
#' @param user \code{chr} username
#'
#' @return \code{tbl} with rows of user times
#' @export
#'

user_times <- function(user) {
  times <- db_filter(user = user)$time
  times <- if (UU::is_legit(times)) {
    time_handler(times)
  } else {
    # use blank default otherwise
    time_rows()
  }
}