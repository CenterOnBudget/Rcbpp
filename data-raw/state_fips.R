
library(dplyr)

if (!file.exists("data-raw/state_fips.txt")) {
  download.file(
    url = "http://www2.census.gov/geo/docs/reference/state.txt",
    destfile = "data-raw/state_fips.txt"
  )
}

state_fips_raw <- readr::read_delim("data-raw/state_fips.txt", delim = "|")

state_fips <- state_fips_raw |>
  rename_with(tolower) |>
  select(state_fips = state, state_abbrv = stusab, state_name) |>
  mutate(
    state_fips = as.numeric(state_fips),
    across(state_abbrv:state_name, forcats::as_factor)
  )

usethis::use_data(state_fips, overwrite = TRUE)
