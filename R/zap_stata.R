

#' Remove all Stata dataset attributes from a data frame
#'
#' `zap_stata()` removes all Stata dataset attributes from a data frame
#' created via [haven::read_dta()]. This function is meant to cut down on
#' repetitive code associated with importing a DTA file when a CSV file is not
#' readily available. The name of this function is a nod to the
#' `haven::zap_*()` functions.
#'
#' More specifically, [zap_stata()]:
#'  - Removes any value labels and converts any tagged missing values to `NA`.
#'  - Removes any variable labels or formats.
#'  - Removes the dataset label.
#'  - Removes any dataset notes.
#'
#'
#' @param df A data frame.
#' @return A data frame.
#'
#' @export
zap_stata <- function(df) {
  stopifnot(is.data.frame(df))

  df[] <- lapply(df, zap_stata_vec)
  attr(df, "label") <- NULL
  attr(df, "notes") <- NULL

  df
}

# See https://github.com/tidyverse/haven for details

zap_stata_vec <- function(x) {
  if (inherits(x, "haven_labelled")) {
    attr(x, "labels") <- NULL
    class(x) <- NULL
  }

  attr(x, "label") <- NULL
  attr(x, "format.stata") <- NULL

  x
}
