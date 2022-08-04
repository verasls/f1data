read_results <- function(url, round, session) {
  if (grepl("FP", session)) {
    session2 <- paste0("practice-", substr(session, 3, 3))
    url2 <- paste0(url, session2, ".html")

    results <- rvest::read_html(url2) |>
      rvest::html_element("body") |>
      rvest::html_table() |>
      dplyr::select(
        position = Pos, driver = Driver, team = Car,
        time = Time, gap = Gap, laps = Laps
      ) |>
      format_data() |>
      dplyr::mutate(
        season = 2022,
        round = round,
        session = session,
        .before = 1
      )
  } else if (grepl("quali", session)) {
    session <- "Qualifying"
    session2 <- "starting-grid"
    url2 <- paste0(url, session2, ".html")

    results <- rvest::read_html(url2) |>
      rvest::html_element("body") |>
      rvest::html_table() |>
      dplyr::select(
        position = Pos, driver = Driver, team = Car, time = Time
      ) |>
      format_data() |>
      dplyr::mutate(
        season = 2022,
        round = round,
        session = session,
        .before = 1
      )
  } else if (grepl("race", session)) {
    session <- "Race"
    session2 <- "race-result"
    url2 <- paste0(url, session2, ".html")

    results <- rvest::read_html(url2) |>
      rvest::html_element("body") |>
      rvest::html_table() |>
      dplyr::select(
        position = Pos, driver = Driver, team = Car,
        laps = Laps, time = `Time/Retired`, points = PTS
      ) |>
      format_data() |>
      dplyr::mutate(
        season = 2022,
        round = round,
        session = session,
        position = ifelse(
          time == "DNF", "DNF", position
        ),
        .before = 1
      )
  }

  results

}

format_data <- function(data) {
  format_driver(data) |>
    format_team()
}

format_driver <- function(data) {
  dplyr::mutate(
    data,
    driver = substr(driver, nchar(driver) - 2, nchar(driver))
  )
}

format_team <- function(data) {
  dplyr::mutate(
    data,
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
