
#' State FIPS codes
#'
#' A dataset containing state FIPS codes along with corresponding state names
#' and postal abbreviations. State equivalents (U.S. territories, outlying
#' areas, and Freely Associated States) are included.
#'
#' @format
#' A data frame with 57 rows and 3 columns:
#' \describe{
#'   \item{state_fips}{State FIPS code (numeric)}
#'   \item{state_abbrv}{State postal abbreviation (factor)}
#'   \item{state_name}{State name (factor)}
#' }
#'
#' @source <https://www.census.gov/library/reference/code-lists/ansi.html>
#'
#' @examples
#' state_fips
#'

"state_fips"


#' CPI-U-RS
#'
#' A dataset containing all available R-CPI-U-RS (formerly known as CPI-U-RS)
#' calendar year averages.
#'
#' @format
#' A data frame with 2 columns:
#' \describe{
#'   \item{year}{Year}
#'   \item{cpi_u_rs}{Annual average R-CPI-U-RS}
#' }
#'
#' @source <https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm>
#'
#' @examples
#' cpi_u_rs
#'

"cpi_u_rs"


#' Toy ACS microdata
#'
#' A toy version of ACS microdata with 200 random person
#' observations.
#'
#' @format A data frame 200 rows and 9 columns:
#' \describe{
#'   \item{serialno}{Household/GQ ID}
#'   \item{sporder}{Person ID}
#'   \item{st}{State FIPS code (character)}
#'   \item{agep}{Age}
#'   \item{rac1p}{Race recode}
#'   \item{hisp}{Hispanic origin recode}
#'   \item{pincp}{Total person's income; use `adjinc` to adjust to constant dollars}
#'   \item{adjinc}{Adjustment factor for income dollar amounts; 6 implied decimal places}
#'   \item{pwgtp}{Person weight}
#' }
#'
#' @source U.S. Census Bureau's 2023 American Community Survey 1-year PUMS, via
#'   CBPP datasets library (parquet format)
#'
#' @examples
#' acs
#'

"acs"

#' Toy CPS ASEC microdata
#'
#' Two toy versions of CPS ASEC microdata with 200 random person observations:
#' - In `cps_unzapped`, all variables are exactly as imported from a Stata DTA
#'   format file, with labels intact.
#' - In `cps`, all variables are numeric (a "zapped" version of `cps_unzapped`).
#'
#' @source U.S. Census Bureau's March 2024 Current Population Survey Annual
#'   Social and Economic Supplement microdata, via CBPP datasets library
#'
#' @seealso [zap_stata()]
#'
#' @examples
#' cps
#' cps_unzapped
#'
#' @format `cps` is a data frame 200 rows and 8 columns, all numeric:
#' \describe{
#'   \item{h_seq}{Household/GQ ID}
#'   \item{pppos}{Person ID}
#'   \item{gestfips}{State FIPS code}
#'   \item{a_age}{Age}
#'   \item{prdtrace}{Race recode}
#'   \item{pehspnon}{Hispanic origin recode}
#'   \item{ptotval}{Total person's income}
#'   \item{marsupwt}{Person weight}
#' }
#'

"cps"

#' @format `cps_unzapped` is a data frame 200 rows and 8 columns, exactly as
#'   imported by [haven::read_dta] from CBPP's datasets library:
#' \describe{
#'   \item{h_seq}{Household sequence number}
#'   \item{pppos}{Person ID}
#'   \item{gestfips}{State FIPS code (labeled)}
#'   \item{a_age}{Age}
#'   \item{prdtrace}{Race recode (labeled)}
#'   \item{pehspnon}{Hispanic origin recode (labeled)}
#'   \item{ptotval}{Total person's income}
#'   \item{marsupwt}{Person weight}
#' }
#'
#' @rdname cps
#'

"cps_unzapped"

