# urls dataset
urls <- readr::read_csv("data-raw/urls.csv")
usethis::use_data(urls, overwrite = TRUE, internal = TRUE)

# sprint dataset
sprint <- readr::read_csv("data-raw/sprint.csv")
usethis::use_data(sprint, overwrite = TRUE, internal = TRUE)
