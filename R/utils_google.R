google_auth <- function(path = "inst/vault/rstudio@adept-eon-198316.iam.gserviceaccount.com.json") {
  googlesheets4::gs4_auth(path = path)
}

google_auth()
db_id <- "https://docs.google.com/spreadsheets/d/1V7Te0zUG-IVRO9bZfZhF2r91Fz_GHdZi2Vn5-eQzQVo/edit#gid=956988236"
# This function must return a data.frame with columns user and sessionid  Other columns are also okay
# and will be made available to the app after log in as columns in credentials()$user_auth

db_sessionid_set <- function(user, sessionid) {
  googlesheets4::sheet_append(db_id,
                              tibble::tibble_row(user = user, sessionid = sessionid, login_time = as.character(Sys.time())),
                              sheet = "session_ids")
}


db_sessionid_get <- function(conn = db$session_ids, user, expiry = 7) {
  conn |> 
    dplyr::filter(login_time > Sys.time() - lubridate::days(expiry) & user == user)
}

db_has_recent_session <- function(user) {
  db_sessionid_get(user = user)
}
#' Which row is the user on in the DB
#'
#' @param email \code{chr} User email address
#' @param out_type \code{obj} of type corresponding to output
#'
#' @return \code{obj} of type similar to `out_type`
#' @export

db_find <- function(x, out_type = logical(), col = c("email", "user")[1], db = virgaUtils::get_global("db")$user) {
  UseMethod("db_find", out_type)
}
#' @export
db_find.default <- function(x, out_type = logical(), col = c("email", "user")[1], db = virgaUtils::get_global("db")$user) {
  db[[col]] %in% x
}
#' @export
db_find.numeric <- function(x, out_type = logical(), col = c("email", "user")[1], db = virgaUtils::get_global("db")$user) {
  which(db[[col]] %in% x)
}
#' @export
db_find.cell_limits <- function(x, out_type = logical(), col = c("email", "user")[1], db = virgaUtils::get_global("db")$user) {
  cellranger::cell_limits(ul = c(which(db[[col]] %in% x) + 1, 1))
}

db_filter <- function(user, email, ..., sheet = "users") {
  x <- rlang::enexprs(...)
  if (!missing(user))
    x <- append(x, rlang::expr(user == !!user))
  if (!missing(email))
    x <- append(x, rlang::expr(email == !!email))
  dplyr::filter(db[[sheet]], !!!x)
}
#' Does a user exist in the DB
#'
#' @inheritParams db_find
#'
#' @return \code{lgl}
#' @export

db_user_exists <- function(x) {
  any(db_find(x, out_type = logical(), col = ifelse(stringr::str_detect(x, "@"), "email", "user")), na.rm = TRUE)
}

db_user_add <- function (user, password, email, tz) {
  .user_id <- tibble::tibble_row(user = user, password = password, email = email, timezone = tz, admin = FALSE, times = NA_character_, circle = NA_character_)
  conds <- c(email = db_user_exists(email), user = db_user_exists(user))
  if (!any(conds)) {
    # if user nor email exists
    googlesheets4::sheet_append(db_id, .user_id, sheet = "users")
  }
  conds
}


#' Update a row in database
#'
#' @param .row \code{tbl} Single row of a tbl
#' @param range \code{cell_limits} to update
#' @inheritParams db_find
#'
#' @return \code{msg}
#' @export
#'

db_row_update <- function(.row, range = NULL, col = "user") {
  if (is.null(range))
    range <- db_find(.row$user, out_type = cellranger::cell_limits(), col = "user")
  googlesheets4::range_write(
    db_id,
    data = .row,
    sheet = "users",
    range = range,
    col_names = FALSE
  )
}