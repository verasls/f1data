get_session_results <- function(season, round, session, detailed = FALSE) {
  round <- get_round(round)
  if (grepl("race|qualifying|sprint", session, ignore.case = TRUE)) {
    parse_ergast_session_results(season, round, session, detailed)
  } else if (grepl("fp", session, ignore.case = TRUE)) {
    parse_f1_session_results(season, round, session, detailed)
  }
}

parse_ergast_session_results <- function(season, round, session, detailed) {
  if (grepl("race", session, ignore.case = TRUE)) {
    url <- glue::glue(
      "http://ergast.com/api/f1/{season}/{round}/results.json?limit=50"
    )

    results <- jsonlite::fromJSON(httr::content(httr::GET(url), as = "text"))
    results <- tibble::as_tibble(results$MRData$RaceTable$Races$Results[[1]])

    format_results_race(results, season, round, session, detailed)
  } else if (grepl("qualifying", session, ignore.case = TRUE)) {
    url <- glue::glue(
      "http://ergast.com/api/f1/{season}/{round}/qualifying.json?limit=50"
    )

    results <- jsonlite::fromJSON(httr::content(httr::GET(url), as = "text"))
    results <- tibble::as_tibble(
      results$MRData$RaceTable$Races$QualifyingResults[[1]]
    )

    format_results_qualifying(results, season, round, session, detailed)
  } else if (grepl("sprint", session, ignore.case = TRUE)) {
    is_sprint <- is_sprint_weekend(season, round)
    if (isFALSE(is_sprint)) {
      rlang::abort("This round does not have a Sprint session.")
    }

    url <- glue::glue(
      "http://ergast.com/api/f1/{season}/{round}/sprint.json?limit=50"
    )

    results <- jsonlite::fromJSON(httr::content(httr::GET(url), as = "text"))
    results <- tibble::as_tibble(
      results$MRData$RaceTable$Races$SprintResults[[1]]
    )

    format_results_race(results, season, round, session, detailed)
  }
}

parse_f1_session_results <- function(season, round, session, detailed) {
  is_sprint <- is_sprint_weekend(season, round)
  if (isTRUE(is_sprint) && grepl("fp3", session, ignore.case = TRUE)) {
    rlang::abort("This round does not have a free practice 3 session.")
  }
  url <- get_f1_urls(season)
  i <- which(url$round_num == round)
  url <- url[i, "urls"]
  url <- paste0(
    url, "practice-",
    substr(session, nchar(session), nchar(session)),
    ".html"
  )

  results <- rvest::read_html(url)
  results <- rvest::html_element(results, "body")
  results <- rvest::html_table(results)

  format_results_practice(results, season, round, session, detailed)
}

#' @importFrom rlang .data
format_results_race <- function(results, season, round, session, detailed) {
  schedule <- get_schedule(season)
  i <- which(schedule$season == season & schedule$round_num == round)
  schedule <- schedule[i, ]

  results <- tidyr::unnest(
    results,
    c(.data$Driver, .data$Constructor, .data$Time), names_sep = "_"
  )
  if (grepl("race", session, ignore.case = TRUE)) {
    results <- tidyr::unnest(results, .data$FastestLap, names_sep = "_")
    results <- tidyr::unnest(results, .data$FastestLap_Time, names_sep = "_")
  }
  results <- dplyr::mutate(
    results,
    driver_name = paste(.data$Driver_givenName, .data$Driver_familyName),
    season = schedule$season, round_num = schedule$round_num,
    round_name = schedule$round_name, circuit = schedule$circuit,
    session = session, date = schedule$race_date,
    Driver_dateOfBirth = as.Date(.data$Driver_dateOfBirth),
    driver_age = lubridate::interval(.data$Driver_dateOfBirth, .data$date),
    driver_age = lubridate::as.period(.data$driver_age),
    time = lubridate::seconds_to_period(
      as.numeric(.data$Time_millis) / 1000
    )
  )
  if (grepl("race", session, ignore.case = TRUE)) {
    results <- dplyr::mutate(
      results,
      fastest_lap_time = lubridate::ms(.data$FastestLap_Time_time, quiet = TRUE)
    )
  }
  if (grepl("race", session, ignore.case = TRUE)) {
    results <- dplyr::select(
      results,
      season = .data$season, round_num = .data$round_num,
      round_name = .data$round_name, circuit = .data$circuit,
      session = session, position = .data$positionText,
      driver_num = .data$number, driver_code = .data$Driver_code,
      driver_name = .data$driver_name, driver_age = .data$driver_age,
      driver_nationality = .data$Driver_nationality,
      constructor = .data$Constructor_name,
      grid_position = .data$grid, .data$laps, .data$status,
      .data$points, .data$time,
      fastest_lap_rank = .data$FastestLap_rank,
      .data$fastest_lap_time,
      fastest_lap_lap = .data$FastestLap_lap
    )
  } else {
    results <- dplyr::select(
      results,
      season = .data$season, round_num = .data$round_num,
      round_name = .data$round_name, circuit = .data$circuit,
      session = session, position = .data$positionText,
      driver_num = .data$number, driver_code = .data$Driver_code,
      driver_name = .data$driver_name, driver_age = .data$driver_age,
      driver_nationality = .data$Driver_nationality,
      constructor = .data$Constructor_name,
      grid_position = .data$grid, .data$laps, .data$status,
      .data$points, .data$time
    )
  }
  results$constructor <- trimws(
    gsub("f1|team|scuderia|racing", "", results$constructor, ignore.case = TRUE)
  )

  if (isTRUE(detailed)) {
    return(results)
  } else {
    results <- dplyr::select(
      results,
      .data$season, .data$round_num, .data$round_name,
      .data$session, .data$position, .data$driver_code,
      .data$constructor, .data$points, .data$time
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
  i <- which(schedule$season == season & schedule$round_num == round)
  schedule <- schedule[i, ]

  results <- tidyr::unnest(
    results,
    c(.data$Driver, .data$Constructor), names_sep = "_"
  )
  results <- dplyr::mutate(
    results,
    driver_name = paste(.data$Driver_givenName, .data$Driver_familyName),
    season = schedule$season, round_num = schedule$round_num,
    round_name = schedule$round_name, circuit = schedule$circuit,
    session = session, date = schedule$race_date,
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
  results$constructor <- trimws(
    gsub("f1|team|scuderia|racing", "", results$constructor, ignore.case = TRUE)
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

#' @importFrom rlang .data
format_results_practice <- function(results, season, round, session, detailed) {
  results <- dplyr::select(
    results,
    position = .data$Pos, driver_num = .data$No, constructor = .data$Car,
    time = .data$Time, laps = .data$Laps
  )
  results <- dplyr::mutate(
    results,
    time = lubridate::ms(results$time, quiet = TRUE)
  )

  info <- get_drivers_info(season, round)
  schedule <- get_schedule(season)
  i <- which(schedule$season == season & schedule$round_num == round)
  schedule <- schedule[i, ]

  results <- dplyr::left_join(
    results, info, by = "driver_num"
  )
  results <- dplyr::mutate(
    results,
    season = schedule$season, round_num = schedule$round_num,
    round_name = schedule$round_name, circuit = schedule$circuit,
    session = session, date = schedule$race_date,
    driver_age = lubridate::interval(.data$dob, .data$date),
    driver_age = lubridate::as.period(.data$driver_age),
  )
  results <- dplyr::select(
    results,
    season = .data$season, round_num = .data$round_num,
    round_name = .data$round_name, circuit = .data$circuit,
    session = session, .data$position, .data$driver_num,
    .data$driver_code, .data$driver_name, .data$driver_age,
    driver_nationality = .data$nationality, .data$constructor,
    .data$laps, .data$time
  )

  # Remove the last word from constructors with more than one word in their
  # names, as from the official formula 1 api results it usually indicates
  # the power unit supplier.
  i <- stringi::stri_count_regex(results$constructor, "\\W+") + 1
  i <- which(i > 1)
  j <- stringi::stri_locate_last_regex(results$constructor[i], "\\W+")[, 1]
  results$constructor[i] <- substr(results$constructor[i], 1, j - 1)
  results$constructor <- trimws(
    gsub("f1|team|scuderia|racing", "", results$constructor, ignore.case = TRUE)
  )
  # Match constructor names with the ones obtained with the ergast api
  constructors <- get_constructors(2022)
  k <- purrr::map(constructors, ~ grep(.x, results$constructor))
  for (l in seq_along(constructors)) {
    results$constructor[k[[l]]] <- constructors[l]
  }

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
