

#' Load CPI-U-RS data
#'
#' \code{get_cpi_u_rs()} loads CPI-U-RS calendar year averages from the
#' \href{https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm}{BLS website}.
#'
#' @param base_year Optionally specify a base year column.
#' @return A tibble.
#'
#' @export
get_cpi_u_rs <- function(base_year = NULL) {

  # Check args ----

  if (!is.null(base_year)) {
    if (length(base_year) != 1 || !is.numeric(base_year)) {
      stop("Pass one `base_year` at a time as a number", call. = FALSE)
    }
  }

  # Get data ----

  temp <- tempfile()
  utils::download.file(
    url = "https://www.bls.gov/cpi/research-series/r-cpi-u-rs-allitems.xlsx",
    destfile = temp,
    mode = "wb",
    quiet = TRUE
  )

  df <- readxl::read_excel(temp, skip = 5)
  file.remove(temp)

  # Clean and return data ----

  names(df) <- tolower(names(df))
  df <- df[df$year >= 1978, c("year", "avg")]
  names(df)[2] <- "cpi_u_rs"

  if (is.null(base_year)) {
    return(df)
  }

  if (!(base_year %in% df$year)) {
    stop(
      glue::glue(
        "Invalid `base_year`, ",
        "years {df$year[1]} to {df$year[length(df$year)]} are available"
      ),
      call. = FALSE
    )
  }

  base_year_col <- glue::glue("cpi_u_rs_{base_year}")
  df[[base_year_col]] <- df$cpi_u_rs[df$year == base_year]
  df
}
