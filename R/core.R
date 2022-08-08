#' @importFrom rlang .data
get_schedule <- function(season) {
  url <- "http://ergast.com/api/f1/"
  url <- paste0(url, season, ".json")

  schedule <- jsonlite::fromJSON(httr::content(httr::GET(url), as = "text"))
  schedule <- tibble::as_tibble(schedule$MRData$RaceTable$Races)
  schedule <- tidyr::unnest(schedule, .data$Circuit, names_sep = "_")
  schedule <- dplyr::select(
    schedule,
    season, round_num = round, round_name = .data$raceName,
    circuit = .data$Circuit_circuitName, date
  )
  schedule <- dplyr::mutate(
    schedule,
    season = as.numeric(.data$season),
    round_num = as.numeric(.data$round_num),
    date = as.Date(date)
  )
  schedule
}

get_position <- function(season, round, session) {
  url <- "http://ergast.com/api/f1/"
  if (grepl("race", session, ignore.case = TRUE)) {
    url <- paste0(url, season, "/", round, "/results.json?limit=50")
  }

  results <- jsonlite::fromJSON(httr::content(httr::GET(url), as = "text"))
  results <- tibble::as_tibble(results$MRData$RaceTable$Races$Results[[1]])
  format_position(results, season, round, session)
}

#' @importFrom rlang .data
format_position <- function(results, season, round, session) {
  schedule <- get_schedule(season)
  idx <- which(schedule$season == season & schedule$round_num == round)
  schedule <- schedule[idx, ]

  results <- tidyr::unnest(
    results,
    c(.data$Driver, .data$Constructor, .data$Time), names_sep = "_"
  )
  results <- dplyr::mutate(
    results,
    driver_name = paste(.data$Driver_givenName, .data$Driver_familyName),
    season = schedule$season, round_num = schedule$round_num,
    round_name = schedule$round_name, circuit = schedule$circuit,
    session = session, date = schedule$date,
    Driver_dateOfBirth = as.Date(.data$Driver_dateOfBirth),
    driver_age = lubridate::interval(.data$Driver_dateOfBirth, .data$date),
    driver_age = lubridate::as.period(.data$driver_age),
    time = lubridate::seconds_to_period(
      as.numeric(.data$Time_millis) / 1000
    )
  )
  results <- dplyr::select(
    results,
    season = .data$season, round_num = .data$round_num,
    round_name = .data$round_name, circuit = .data$circuit,
    session = session, position = .data$positionText,
    driver_num = .data$number, driver_code = .data$Driver_code,
    driver_name = .data$driver_name, driver_age = .data$driver_age,
    driver_nationality = .data$Driver_nationality,
    constructor = .data$Constructor_name,
    grid_position = .data$grid, .data$laps, .data$status, time
  )
  results
}
