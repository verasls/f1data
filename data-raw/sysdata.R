# urls dataset
urls <- readr::read_csv("data-raw/urls.csv")
# sprint dataset
sprint <- readr::read_csv("data-raw/sprint.csv")
# schedule dataset
schedule <- readr::read_csv("data-raw/schedule.csv")

usethis::use_data(urls, sprint, schedule, overwrite = TRUE, internal = TRUE)
