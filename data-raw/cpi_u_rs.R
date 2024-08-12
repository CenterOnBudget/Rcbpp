

library(readxl)
library(dplyr)

download.file(
  url = "https://www.bls.gov/cpi/research-series/r-cpi-u-rs-allitems.xlsx",
  destfile = "data-raw/cpi_u_rs.xlsx",
  mode = "wb",
  # BLS site requirement for automated downloads
  headers = c("User-Agent" = paste0(Sys.getenv()[["USERNAME"]], "@cbpp.org"))
)

raw_cpi_u_rs <- read_xlsx("data-raw/cpi_u_rs.xlsx", skip = 5)

cpi_u_rs <- raw_cpi_u_rs %>%
  rename_with(tolower) %>%
  select(year, cpi_u_rs = avg) %>%
  filter(year >= 1978)

usethis::use_data(cpi_u_rs, overwrite = TRUE)
