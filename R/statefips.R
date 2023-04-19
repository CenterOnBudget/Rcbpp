

#' Convert a state FIPS code to a labeled factor
#'
#' @param x Vector of state FIPS codes. If x is a character vector, it will
#'   first be coerced to numeric.
#' @param labels A character string specifying how to label the levels: `"name"`
#'   for state names (the default), or `"abbrv"` for state postal abbreviations.
#'
#' @return x as a labeled factor
#' @seealso [state_fips] for a dataset containing state FIPS codes, state names,
#'   and postal abbreviations.
#'
#' @export
#'
#' @examples
#' fips <- c(1, 6, 11, 48)
#' fct_statefips(fips)
#' fct_statefips(fips, labels = "abbrv")
#'

fct_statefips <- function(x, labels = c("name", "abbrv")) {

  labels <- match.arg(labels)

  if (labels == "name") {
    lbls <- Rcbpp::state_fips$state_name
  }
  if (labels == "abbrv") {
    lbls <- Rcbpp::state_fips$state_abbrv
  }

  x <- as.numeric(x)

  factor(
    x,
    levels = Rcbpp::state_fips$state_fips,
    labels = lbls
  )

}
