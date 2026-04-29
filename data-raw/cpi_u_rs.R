library(dplyr)

user_email <- paste0(fs::path_file(Sys.getenv("USERPROFILE")), "@cbpp.org")

download.file(
  url = "https://www.bls.gov/cpi/research-series/r-cpi-u-rs-allitems.xlsx",
  destfile = "data-raw/cpi_u_rs.xlsx",
  mode = "wb",
  # BLS site requirement for automated downloads
  headers = c("User-Agent" = user_email)
)

raw_cpi_u_rs <- readxl::read_xlsx("data-raw/cpi_u_rs.xlsx", skip = 5)

cpi_u_rs <- raw_cpi_u_rs |>
  rename_with(tolower) |>
  select(year, cpi_u_rs = avg) |>
  filter(year >= 1978, !is.na(cpi_u_rs))

usethis::use_data(cpi_u_rs, overwrite = TRUE)
