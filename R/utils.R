#' @importFrom rlang .data
get_schedule <- function(season, detailed = FALSE) {
  url <- glue::glue(
    "http://ergast.com/api/f1/{season}.json"
  )

  schedule <- jsonlite::fromJSON(httr::content(httr::GET(url), as = "text"))
  schedule <- tibble::as_tibble(schedule$MRData$RaceTable$Races)
  schedule <- tidyr::unnest(
    schedule,
    c(
      .data$Circuit, .data$FirstPractice,
      .data$SecondPractice, .data$ThirdPractice,
      .data$Qualifying, .data$Sprint
    ),
    names_sep = "_"
  )
  schedule <- dplyr::select(
    schedule,
    season, round_num = round, round_name = .data$raceName,
    circuit = .data$Circuit_circuitName,
    FP1_date = .data$FirstPractice_date,
    FP2_date = .data$SecondPractice_date,
    FP3_date = .data$ThirdPractice_date,
    qualifying_date = .data$Qualifying_date,
    sprint_date = .data$Sprint_date,
    race_date = .data$date
  )
  schedule <- dplyr::mutate(
    schedule,
    season = as.numeric(.data$season),
    round_num = as.numeric(.data$round_num),
    dplyr::across(dplyr::ends_with("_date"), as.Date)
  )

  if (isTRUE(detailed)) {
    return(schedule)
  } else if (isFALSE(detailed)) {
    schedule <- dplyr::select(
      schedule,
      .data$season, .data$round_num, .data$round_name,
      .data$circuit, .data$race_date
    )
    return(schedule)
  }
}

get_f1_urls <- function(season) {
  home <- "https://www.formula1.com"
  url <- glue::glue(
    "https://www.formula1.com/en/results.html/{season}/races.html"
  )

  urls <- rvest::read_html(url)
  urls <- rvest::html_element(urls, "body")
  urls <- rvest::html_elements(urls, "a")
  urls <- as.character(urls)

  i <- which(grepl("(.*2022)(.*race-result.html)", urls))
  urls <- urls[i]
  urls <- strsplit(urls, split = "\\\"")
  urls <- purrr::map_chr(urls, 2)

  duplicated <- data.frame(table(urls))
  duplicated <- as.character(duplicated$urls[duplicated$Freq > 1])
  to_remove <- purrr::map_dbl(duplicated, ~ which(urls == .x)[2])
  urls <- urls[- to_remove]

  urls <- paste0(home, urls)
  urls <- substr(urls, 1, nchar(urls) - 16)

  urls_df <- tibble::tibble(
    season = season, round_num = seq_along(urls), urls = urls
  )

  check_f1_urls(season, urls_df)
  urls_df
}

check_f1_urls <- function(season, urls_df) {
  url <- glue::glue(
    "https://www.formula1.com/en/results.html/{season}/races.html"
  )

  urls <- rvest::read_html(url)
  urls <- rvest::html_element(urls, "body")
  urls <- as.character(urls)

  i <- stringi::stri_locate_first(urls, fixed = "raceNavigationItems")[1]
  urls <- substr(urls, i, nchar(urls))
  i <- stringi::stri_locate_first(urls, fixed = "]")[1]
  urls <- substr(urls, 1, 6755)

  i <- stringi::stri_locate_all(
    urls,
    regex = "\"meetingKey\":[:digit:]+,"
  )
  start_i <- i[[1]][, 1]
  end_i <- i[[1]][, 2]
  key <- purrr::map2_chr(start_i, end_i, ~ substr(urls, .x, .y))
  key <- substr(key, 14, nchar(key) - 1)

  i <- stringi::stri_locate_all(
    urls,
    regex = "\"meetingNumber\":[:digit:]+,"
  )
  start_i <- i[[1]][, 1]
  end_i <- i[[1]][, 2]
  num <- purrr::map2_chr(start_i, end_i, ~ substr(urls, .x, .y))
  num <- as.numeric(substr(num, 17, nchar(num) - 1))

  check_df <- tibble::tibble(num = num, key = key)
  check_df <- dplyr::arrange(check_df, num)

  if (any(diff(check_df$num) != 1)) {
    rlang::abort("Wrong sequence of round numbers")
  }

  check_df <- tibble::add_column(urls_df, check_df)

  num_ok <- ifelse(check_df$round_num - check_df$num != 0, FALSE, TRUE)

  key_ok <- substr(urls_df$urls, 53, nchar(urls_df$urls))
  i <- stringi::stri_locate_first(key_ok, fixed = "/")[, 1]
  key_ok <- substr(key_ok, 1, i - 1)
  key_ok <- purrr::map_lgl(
    seq_along(key_ok), ~ grepl(key_ok[.x], check_df$urls[.x])
  )

  if (any(isFALSE(num_ok)) || any(isFALSE(key_ok))) {
    rlang::abort("The urls do not match the round numbers")
  } else {
    NULL
  }
}

is_sprint_weekend <- function(season, round) {
  if (season < 2021) {
    FALSE
  } else if (season >= 2021) {
    schedule <- get_schedule(season, detailed = TRUE)
    i <- which(schedule$round_num == round)
    if (is.na(schedule[i, "sprint_date"])) {
      FALSE
    } else {
      TRUE
    }
  }
}

get_drivers_info <- function(season, round) {
  url <- glue::glue(
    "http://ergast.com/api/f1/{season}/{round}/drivers.json?limit=50"
  )

  info <- jsonlite::fromJSON(httr::content(httr::GET(url), as = "text"))
  info <- tibble::as_tibble(info$MRData$DriverTable$Drivers)
  info <- dplyr::mutate(
    info,
    driver_num = as.integer(.data$permanentNumber),
    driver_name = paste(.data$givenName, .data$familyName),
    dob = as.Date(.data$dateOfBirth)
  )
  info <- dplyr::select(
    info,
    .data$driver_num, driver_code = .data$code,
    .data$driver_name, .data$dob, .data$nationality
  )
  info <- dplyr::rowwise(info)
  info <- dplyr::mutate(
    info,
    driver_num = ifelse(
      .data$driver_code == "VER" && season == 2022,
      1, .data$driver_num
    )
  )
  dplyr::ungroup(info)
}

get_constructors <- function(season) {
  url <- glue::glue(
    "http://ergast.com/api/f1/{season}/constructors.json?limit=50"
  )
  constructors <- jsonlite::fromJSON(httr::content(httr::GET(url), as = "text"))
  constructors <- constructors$MRData$ConstructorTable$Constructors$name
  constructors <- trimws(
    gsub("f1|team|scuderia|racing", "", constructors, ignore.case = TRUE)
  )
  constructors
}
