read_results <- function(url, round, session) {
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

  results <- rvest::read_html(url2)
  results <- rvest::html_element(results, "body")
  results <- rvest::html_table(results)

  if (grepl("FP", session, ignore.case = TRUE)) {
    results <- dplyr::select(
      results,
      position = Pos, driver = Driver, team = Car,
      time = Time, gap = Gap, laps = Laps
    )
  } else if (grepl("quali", session, ignore.case = TRUE)) {
    results <- dplyr::select(
      results,
      position = Pos, driver = Driver, team = Car, time = Time
    )
  } else if (grepl("race", session, ignore.case = TRUE)) {
    results <- dplyr::select(
      results,
      position = Pos, driver = Driver, team = Car,
      laps = Laps, time = `Time/Retired`, points = PTS
    )
  }

  results <- format_results(results)
  results <- dplyr::mutate(
    results,
    season = 2022,
    round = round,
    session = session,
    position = ifelse(
      time == "DNF", "DNF", position
    ),
    .before = 1
  )

  results

}

format_results <- function(results) {
  results <- format_driver(results)
  results <- format_team(results)
  results
}

format_driver <- function(results) {
  dplyr::mutate(
    results,
    driver = substr(driver, nchar(driver) - 2, nchar(driver))
  )
}

format_team <- function(results) {
  dplyr::mutate(
    results,
    team = dplyr::case_when(
      grepl("^Mercedes$", team)   ~ "Mercedes",
      grepl("Red Bull", team)     ~ "Red Bull Racing",
      grepl("^Ferrari$", team)    ~ "Ferrari",
      grepl("McLaren", team)      ~ "McLaren",
      grepl("Alpine", team)       ~ "Alpine",
      grepl("AlphaTauri", team)   ~ "AlphaTauri",
      grepl("Aston Martin", team) ~ "Aston Martin",
      grepl("Williams", team)     ~ "Williams",
      grepl("Alfa Romeo", team)   ~ "Alfa Romeo",
      grepl("Haas", team)         ~ "Haas"
    )
  )
}
