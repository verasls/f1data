#' @importFrom rlang .data
read_results <- function(season, round, session) {

  url <- urls[which(urls$season == season), ]

  sprint_weekend <- is_sprint_weekend(season, round)

  if (isTRUE(sprint_weekend) && grepl("FP3", session, ignore.case = TRUE)) {
    msg <- glue::glue(
      "This round ({round}) of the {season} season is a sprint weekend \\
      and does not have the specified session ({session}). \\
      Please, try another one."
    )
    rlang::abort(msg)
  }

  if (is.numeric(round)) {
    idx <- which(url$round_num == round)
  } else if (is.character(round)) {
    idx <- which(url$round_name == round)
  }

  round <- paste(url$round_num[idx], "-", url$round_name[idx])
  url <- url$url[idx]

  if (isFALSE(sprint_weekend)) {
    if (grepl("FP", session, ignore.case = TRUE)) {
      session2 <- paste0("practice-", substr(session, 3, 3))
      url2 <- paste0(url, session2, ".html")
    } else if (grepl("quali", session, ignore.case = TRUE)) {
      session <- "Qualifying"
      session2 <- "starting-grid"
      url2 <- paste0(url, session2, ".html")
    } else if (grepl("race", session, ignore.case = TRUE)) {
      session <- "Race"
      session2 <- "race-result"
      url2 <- paste0(url, session2, ".html")
    }
  } else if (isTRUE(sprint_weekend)) {
    if (grepl("quali", session, ignore.case = TRUE)) {
      session <- "Qualifying"
      session2 <- "sprint-grid"
      url2 <- paste0(url, session2, ".html")
    } else if (grepl("sprint", session, ignore.case = TRUE)) {
      session <- "Sprint race"
      session2 <- "sprint-results"
      url2 <- paste0(url, session2, ".html")
    } else if (grepl("race", session, ignore.case = TRUE)) {
      session <- "Race"
      session2 <- "race-result"
      url2 <- paste0(url, session2, ".html")
    }
  }

  results <- rvest::read_html(url2)
  results <- rvest::html_element(results, "body")
  results <- rvest::html_table(results)

  if (grepl("FP", session, ignore.case = TRUE)) {
    results <- dplyr::select(
      results,
      position = .data$Pos, driver = .data$Driver, team = .data$Car,
      time = .data$Time, gap = .data$Gap, laps = .data$Laps
    )
  } else if (grepl("quali", session, ignore.case = TRUE)) {
    results <- dplyr::select(
      results,
      position = .data$Pos, driver = .data$Driver,
      team = .data$Car, time = .data$Time
    )
  } else if (grepl("race", session, ignore.case = TRUE)) {
    results <- dplyr::select(
      results,
      position = .data$Pos, driver = .data$Driver, team = .data$Car,
      laps = .data$Laps, time = .data$`Time/Retired`, points = .data$PTS
    )
  }

  results <- format_results(results)
  results <- dplyr::mutate(
    results,
    season = season,
    round = round,
    session = session,
    position = ifelse(
      .data$time == "DNF", "DNF", .data$position
    ),
    position = ifelse(
      .data$time == "DNS", "DNS", .data$position
    ),
    .before = 1
  )

  results

}

is_sprint_weekend <- function(season, round) {
  sprint <- sprint[which(sprint$season == season), ]

  if (is.numeric(round)) {
    idx <- which(sprint$round_num == round)
  } else if (is.character(round)) {
    idx <- which(sprint$round_name == round)
  }

  !identical(idx, integer(0))
}

format_results <- function(results) {
  results <- format_driver(results)
  results <- format_team(results)
  results
}

format_driver <- function(results) {
  dplyr::mutate(
    results,
    driver = substr(.data$driver, nchar(.data$driver) - 2, nchar(.data$driver))
  )
}

format_team <- function(results) {
  dplyr::mutate(
    results,
    team = dplyr::case_when(
      grepl("^Mercedes$", .data$team)   ~ "Mercedes",
      grepl("Red Bull", .data$team)     ~ "Red Bull Racing",
      grepl("^Ferrari$", .data$team)    ~ "Ferrari",
      grepl("McLaren", .data$team)      ~ "McLaren",
      grepl("Alpine", .data$team)       ~ "Alpine",
      grepl("AlphaTauri", .data$team)   ~ "AlphaTauri",
      grepl("Aston Martin", .data$team) ~ "Aston Martin",
      grepl("Williams", .data$team)     ~ "Williams",
      grepl("Alfa Romeo", .data$team)   ~ "Alfa Romeo",
      grepl("Haas", .data$team)         ~ "Haas"
    )
  )
}
