

#' Load CPI-U-RS data
#'
#' \code{get_cpi_u_rs()} loads all available CPI-U-RS calendar year averages
#' from the
#' \href{https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm}{BLS website}.
#'
#' @param base_year Year to use for base year column. This argument is optional.
#'   If left \code{NULL} (default), data are returned without a base year
#'   column.
#' @return A \href{https://tibble.tidyverse.org/}{tibble}.
#'
#' @examples
#' # Load all available CPI-U-RS calendar year averages
#' get_cpi_u_rs()
#' # Specify a base year column
#' get_cpi_u_rs(base_year = 2019)
#' @export
get_cpi_u_rs <- function(base_year = NULL) {

  # Check args -----------------------------------------------------------------

  if (!is.null(base_year) && !is_number(base_year)) {
    stop("`base_year` must be `NULL` or a number", call. = FALSE)
  }

  # Get data -------------------------------------------------------------------

  temp <- tempfile(fileext = ".xlsx")
  utils::download.file(
    url = "https://www.bls.gov/cpi/research-series/r-cpi-u-rs-allitems.xlsx",
    destfile = temp,
    mode = "wb",
    quiet = TRUE
  )

  df <- readxl::read_xlsx(temp, skip = 5)
  file.remove(temp)

  # Clean data -----------------------------------------------------------------

  names(df) <- tolower(names(df))
  df <- df[df$year >= 1978, c("year", "avg")]
  names(df)[2] <- "cpi_u_rs"

  # Return data ----------------------------------------------------------------

  if (is.null(base_year)) {
    return(df)
  }

  if (base_year %!in% df$year) {
    stop(
      "Invalid `base_year`, years ", min(df$year), " to ", max(df$year), " are available",
      call. = FALSE
    )
  }

  base_year_col <- paste0("cpi_u_rs_", base_year)
  df[[base_year_col]] <- df$cpi_u_rs[df$year == base_year]
  df
}
