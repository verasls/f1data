url <- "http://ergast.com/api/f1/circuits.json?limit=99"

circuits <- jsonlite::fromJSON(httr::content(httr::GET(url), as = "text"))
circuits <- tibble::as_tibble(circuits$MRData$CircuitTable$Circuits)
circuits <- tidyr::unnest(circuits, Location)
circuits <- dplyr::select(
  circuits,
  circuit = .data$circuitName,
  country = .data$country
)
# Deal with country abbreviations
circuits <- dplyr::mutate(
  circuits,
  country = ifelse(
    grepl("UK", country), paste(country, "United Kingdom"),
    ifelse(
      grepl("USA", country), paste(country, "United States of America"),
      ifelse(
        grepl("UAE", country), paste(country, "United Arab Emirates"),
        country
      )
    )
  )
)

rounds <- get_schedule(2022)
rounds <- dplyr::left_join(rounds, circuits, by = "circuit")
rounds <- dplyr::transmute(
  rounds,
  season = season,
  round_num = round_num,
  names = tolower(
    paste(
      trimws(gsub("Grand Prix", "", round_name)),
      circuit, country
    )
  )
)

usethis::use_data(rounds, internal = TRUE)
