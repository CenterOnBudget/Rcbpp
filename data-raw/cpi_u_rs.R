

library(readxl)
library(dplyr)

if (!file.exists("data-raw/cpi_u_rs.xlsx")) {
  download.file(
    url = "https://www.bls.gov/cpi/research-series/r-cpi-u-rs-allitems.xlsx",
    destfile = "data-raw/cpi_u_rs.xlsx",
    mode = "wb"
  )
}

raw_cpi_u_rs <- read_xlsx("data-raw/cpi_u_rs.xlsx", skip = 5)

cpi_u_rs <- raw_cpi_u_rs %>%
  rename_with(tolower) %>%
  select(year, cpi_u_rs = avg) %>%
  filter(year > 1977)

usethis::use_data(cpi_u_rs, overwrite = TRUE)
