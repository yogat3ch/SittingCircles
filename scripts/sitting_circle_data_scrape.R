ss <- "https://docs.google.com/spreadsheets/d/1_U-r7yqS1MMBEzU5vwOfUawrDVKfOFcCM3-Xgpe8dUU/edit#gid=0"
googlesheets4::gs4_auth(path = "~/Documents/R/auth_tokens/rstudio@adept-eon-198316.iam.gserviceaccount.com.json")
sign_ups <- googlesheets4::read_sheet(ss, "Sheet1")
emails <- googlesheets4::read_sheet(ss, "First and Last Names")
users <- dplyr::inner_join(dplyr::distinct(emails, Email, .keep_all = TRUE), dplyr::select(sign_ups, `Submitted On`, Email, `UTC +/-`, `earliest start time`, `latest start time not end time`)) |> 
  dplyr::group_by(Email) |> 
  dplyr::slice_max(`Submitted On`) |> 
  dplyr::filter(!is.na(`UTC +/-`)) |> 
  dplyr::select(- `Submitted On`, -`would you like to get invitations to sitheads meetups and qas with guest teachers`) |> 
  tidyr::unnest(`latest start time not end time`) |> 
  dplyr::mutate(
    sign = sign(`UTC +/-`),
    timezone = paste0("Etc/GMT", 
                      dplyr::case_when(sign == -1 ~ "-",
                                       sign == 0 ~ "",
                                       sign == 1 ~ "+"
                                       ), abs(`UTC +/-`)),
    earliest = `earliest start time`,
    latest = `latest start time not end time` - lubridate::hours(1),
    sign = NULL,
    `UTC +/-` = NULL,
    `earliest start time` = NULL,
    `latest start time not end time` = NULL,
    user = Email,
    admin = FALSE,
    password = stringi::stri_rand_strings(length(user), 12)
    ) |> 
  dplyr::rename_with(.fn = snakecase::to_snake_case) |> 
  dplyr::select(user, password, email, first_name, last_name, timezone, admin, earliest, latest)
  
users$times <- slider::slide_chr(users, ~{
  time_handler(dplyr::bind_rows(
    tibble::tibble(day = UU::week_factor(), begin = .x$earliest, end = .x$earliest + lubridate::minutes(30)),
    tibble::tibble(day = UU::week_factor(), begin = .x$latest, end = .x$latest - lubridate::hours(1) + lubridate::minutes(30))
  ))
})

users <- dplyr::mutate(users,
              earliest = NULL,
              latest = NULL,
              circle = NA)
googlesheets4::sheet_append(
  "https://docs.google.com/spreadsheets/d/1V7Te0zUG-IVRO9bZfZhF2r91Fz_GHdZi2Vn5-eQzQVo/edit#gid=0",
  users
)
