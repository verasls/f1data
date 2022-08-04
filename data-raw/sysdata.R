# urls dataset
urls <- readr::read_csv("data-raw/urls.csv")
# sprint dataset
sprint <- readr::read_csv("data-raw/sprint.csv")

usethis::use_data(urls, sprint, overwrite = TRUE, internal = TRUE)
