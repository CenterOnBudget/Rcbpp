#' Remove all Stata dataset attributes from a data frame
#'
#' @description
#' `zap_stata()` removes all Stata attributes from a data frame returned by
#' [haven::read_dta()]. Specifically, it:
#'
#' - Removes variable labels, value labels, format attributes, and display
#' width attributes from all variables
#' - Removes dataset label and notes
#' - Converts all tagged missing values to regular R `NA`
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param df `r lifecycle::badge("deprecated")` Use of `df` is now deprecated;
#'   please use `data` instead.
#'
#' @returns The input data frame sans Stata attributes.
#'
#' @seealso [haven::zap_label()] and friends
#'
#' @examples
#' library(haven)
#'
#' # The built-in dataset `cps_unzapped` has Stata attributes
#' cps_unzapped
#' attr(cps_unzapped, "label")
#'
#' # Remove them all:
#' cps_zapped <- zap_stata(cps_unzapped)
#' cps_zapped
#' attr(cps_zapped, "label")
#'
#' @export

# Adapted from haven::zap_*() family of functions

zap_stata <- function(data, df = deprecated()) {
  if (lifecycle::is_present(df)) {
    lifecycle::deprecate_warn("1.0.0", "zap_stata(df)", "zap_stata(data)")
    data <- df
  }

  rlang::check_data_frame(data)

  attr(data, "notes") <- NULL
  attr(data, "label") <- NULL

  data[] <- lapply(data, zap_stata_vec)
  data
}


zap_stata_vec <- function(x) {
  if (inherits(x, "haven_labelled")) {
    attr(x, "labels") <- NULL
    class(x) <- NULL
    x[is.na(x)] <- NA
  }

  attr(x, "label") <- NULL
  attr(x, "format.stata") <- NULL
  attr(x, "display_width") <- NULL

  x
}
