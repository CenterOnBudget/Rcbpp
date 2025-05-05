
library(dplyr)
library(Rcbpp)
library(arrow)
library(haven)

acs <- open_dataset(
  sp_data_path("ACS/parquet/1-year/2023"),
  partitioning = schema(st = string())
) |>
  filter(np > 0) |>
  select(
    serialno, sporder, st, agep, rac1p, hisp, pincp, adjinc, pwgtp
  ) |>
  collect() |>
  slice_sample(n = 200, weight_by = pwgtp)

cps_unzapped <- read_dta(
  sp_data_path("CPS/mar2024/mar2024.dta"),
  col_select = c(
    h_seq, pppos, gestfips, a_age, prdtrace, pehspnon, ptotval, marsupwt
  )
) |>
  relocate(
    h_seq, pppos, gestfips, a_age, prdtrace, pehspnon, ptotval, marsupwt
  ) |>
  slice_sample(n = 200, weight_by = marsupwt)

cps <- zap_stata(cps_unzapped)

usethis::use_data(acs, overwrite = TRUE)
usethis::use_data(cps_unzapped, overwrite = TRUE)
usethis::use_data(cps, overwrite = TRUE)
