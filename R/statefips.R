#' Convert state FIPS codes to a labeled factor
#'
#' Convert a vector of state FIPS codes into a factor labeled by state name or
#' postal abbreviation.
#'
#' @param x A numeric or character vector of state FIPS codes.
#' @param labels Either `"name"`, to use state names for the factor levels (the
#'   default), or `"abbrv"`, to use state postal abbreviations.
#' @param drop_unused Whether to drop unused levels. If `FALSE`, the default,
#'   the levels of the resulting factor will include every state and state
#'   equivalent.
#'
#' @returns A factor.
#'
#' @seealso [state_fips] for a dataset containing state FIPS codes, state names,
#'   and postal abbreviations.
#'
#' @examples
#' fips <- c(1, 6, 11, 48)
#' fct_statefips(fips)
#'
#' # Character vectors work too; elements that can't be coerced to an integer
#' # will be `NA`
#' fips_chr <- c("01", "06", "11", "48", "XYZ")
#' fct_statefips(fips_chr)
#'
#' # Resulting levels may be restricted to those present in the input
#' fct_statefips(fips, drop_unused = TRUE)
#'
#' @export

fct_statefips <- function(x, labels = c("name", "abbrv"), drop_unused = FALSE) {
  labels <- rlang::arg_match(labels)

  if (!(rlang::is_integerish(x) | rlang::is_character(x))) {
    cli::cli_abort(c(
      "{.arg x} must be an integer or character vector.",
      "x" = "{.arg x} is {.obj_type_friendly {x}}."
    ))
  }

  if (identical(labels, "name")) {
    lbls <- Rcbpp::state_fips$state_name
  }
  if (identical(labels, "abbrv")) {
    lbls <- Rcbpp::state_fips$state_abbrv
  }

  x <- as.numeric(x)

  f <- factor(
    x,
    levels = Rcbpp::state_fips$state_fips,
    labels = lbls
  )

  if (drop_unused) {
    f <- droplevels(f)
  }

  f
}
