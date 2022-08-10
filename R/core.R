get_position <- function(season, round, session, detailed = FALSE) {
  url <- "http://ergast.com/api/f1/"
  if (grepl("race", session, ignore.case = TRUE)) {
    url <- paste0(url, season, "/", round, "/results.json?limit=50")
    results <- jsonlite::fromJSON(httr::content(httr::GET(url), as = "text"))
    results <- tibble::as_tibble(results$MRData$RaceTable$Races$Results[[1]])
    format_results_race(results, season, round, session, detailed)
  } else if (grepl("qualifying", session, ignore.case = TRUE)) {
    url <- paste0(url, season, "/", round, "/qualifying.json?limit=50")
    results <- jsonlite::fromJSON(httr::content(httr::GET(url), as = "text"))
    results <- tibble::as_tibble(
      results$MRData$RaceTable$Races$QualifyingResults[[1]]
    )
    format_results_qualifying(results, season, round, session, detailed)
  }
}

#' @importFrom rlang .data
format_results_race <- function(results, season, round, session, detailed) {
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
    grid_position = .data$grid, .data$laps, .data$status, .data$time
  )

  if (isTRUE(detailed)) {
    return(results)
  } else {
    results <- dplyr::select(
      results,
      .data$season, .data$round_num, .data$round_name,
      .data$session, .data$position, .data$driver_code,
      .data$constructor, .data$time
    )
    return(results)
  }
}

#' @importFrom rlang .data
format_results_qualifying <- function(results,
                                      season,
                                      round,
                                      session,
                                      detailed) {
  schedule <- get_schedule(season)
  idx <- which(schedule$season == season & schedule$round_num == round)
  schedule <- schedule[idx, ]

  results <- tidyr::unnest(
    results,
    c(.data$Driver, .data$Constructor), names_sep = "_"
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
    Q1 = lubridate::ms(.data$Q1, quiet = TRUE),
    Q2 = lubridate::ms(.data$Q2, quiet = TRUE),
    Q3 = lubridate::ms(.data$Q3, quiet = TRUE)
  )
  results <- dplyr::select(
    results,
    season = .data$season, round_num = .data$round_num,
    round_name = .data$round_name, circuit = .data$circuit,
    session = session, .data$position,
    driver_num = .data$number, driver_code = .data$Driver_code,
    driver_name = .data$driver_name, driver_age = .data$driver_age,
    driver_nationality = .data$Driver_nationality,
    constructor = .data$Constructor_name,
    .data$Q1, .data$Q2, .data$Q3
  )

  if (isTRUE(detailed)) {
    return(results)
  } else {
    results <- dplyr::select(
      results,
      .data$season, .data$round_num, .data$round_name,
      .data$session, .data$position, .data$driver_code,
      .data$constructor, .data$Q1, .data$Q2, .data$Q3
    )
    return(results)
  }
}
