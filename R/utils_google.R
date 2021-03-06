google_auth <- function(path = "inst/vault/rstudio@adept-eon-198316.iam.gserviceaccount.com.json") {
  googlesheets4::gs4_auth(path = path)
}

google_auth()
db_id <- "https://docs.google.com/spreadsheets/d/1V7Te0zUG-IVRO9bZfZhF2r91Fz_GHdZi2Vn5-eQzQVo/edit#gid=956988236"
db <- purrr::map(rlang::set_names(c("users", "session_ids")), ~googlesheets4::read_sheet(db_id, sheet = .x))

db_sessionid_set <- function(user, sessionid) {
  googlesheets4::sheet_append(db_id,
                              tibble::tibble_row(user = user, sessionid = sessionid, login_time = as.character(Sys.time())),
                              sheet = "session_ids")
}

# This function must return a data.frame with columns user and sessionid  Other columns are also okay
# and will be made available to the app after log in as columns in credentials()$user_auth

db_sessionid_get <- function(conn = db$session_ids, expiry = 7) {
  conn |> 
    dplyr::filter(login_time > Sys.time() - lubridate::days(expiry))
}

#' Which row is the user on in the DB
#'
#' @param email \code{chr} User email address
#' @param out_type \code{obj} of type corresponding to output
#'
#' @return \code{obj} of type similar to `out_type`
#' @export

db_find <- function(x, out_type = logical(), col = c("email", "user")[1]) {
  UseMethod("db_find", out_type)
}
#' @export
db_find.default <- function(x, out_type, col) {
  db$users[[col]] %in% x
}
#' @export
db_find.numeric <- function(x, out_type, col) {
  which(db$users[[col]] %in% x)
}
#' @export
db_find.cell_limits <- function(x, out_type, col) {
  cellranger::cell_rows(which(db$users[[col]] %in% x) + 1)
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

db_user_add <- function (user, password, email) {
  .user_id <- tibble::tibble_row(user = user, password = password, email = email, admin = FALSE, times = NA_character_, circle = NA_character_)
  conds <- c(email = db_user_exists(email), user = db_user_exists(user))
  if (!any(conds)) {
    # if user nor email exists
    googlesheets4::sheet_append(db_id, .user_id, sheet = "users")
  }
  conds
}