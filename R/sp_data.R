

#' Create paths to files in the SharePoint datasets library
#'
#' \code{sp_data()} provides a compact way of creating paths to files in the
#' SharePoint datasets library.
#'
#' This function creates one or more file paths based on the user's path to
#' the SharePoint datasets library (accessed via environment variable
#' \code{spdatapath}) and the directory structure and file naming convention of
#' each dataset library. This function does not itself verify whether the
#' resulting files actually exist.
#'
#' @param dataset Dataset to build file paths for. One of \code{"acs"},
#'   \code{"cps_asec"}, or \code{"cps_basic"}.
#' @param y One or more years.
#' @param m One or more months (specified numerically). Only applicable if
#'   \code{dataset} is \code{"cps_basic"}.
#' @param f Format of data. One of \code{"csv"} or \code{"dta"}.
#' @return A character vector containing the created file paths.
#'
#' @export
sp_data <- function(dataset, y, m = NULL, f) {

  # Get SP data path -----------------------------------------------------------

  path <- Sys.getenv("spdatapath")

  if (path == "") {
    stop(
      "Path to SharePoint datasets library not found, supply with env var `spdatapath`",
      call. = FALSE
    )
  }

  # Check args -----------------------------------------------------------------

  if (!is_string(dataset)) {
    stop("`dataset` must be a string", call. = FALSE)
  }

  if (dataset %!in% c("acs", "cps_asec", "cps_basic")) {
    stop('`dataset` must be one of "acs", "cps_asec", or "cps_basic"', call. = FALSE)
  }

  if (!is.numeric(y)) {
    stop("`y` must be a numeric vector", call. = FALSE)
  }

  if (dataset == "cps_basic") {
    if (is.null(m)) {
      stop('If `dataset` is "cps_basic", you must supply `m`', call. = FALSE)
    }

    if (!is.numeric(m) || !all(m %in% 1:12)) {
      stop("`m` must be a numeric vector with values ranging from 1 to 12", call. = FALSE)
    }
  } else {
    if (!is.null(m)) {
      stop('`m` is only applicable if `dataset` is "cps_basic"', call. = FALSE)
    }
  }

  if (!is_string(f)) {
    stop("`f` must be a string", call. = FALSE)
  }

  if (f %!in% c("csv", "dta")) {
    stop('`f` must be one of "csv" or "dta"', call. = FALSE)
  }

  # Make file paths ------------------------------------------------------------

  f <- paste0(".", f)

  if (dataset == "cps_asec") {
    paste0(path, "CPS/mar", y, "/mar", y, f)
  } else if (dataset == "cps_basic") {
    df <- expand.grid(m = m, y = y)
    df$y_sub <- substr(df$y, 3, 4)
    df$m_abb <- tolower(month.abb)[df$m]
    paste0(path, "CPS-BASIC/", df$y, "/", df$m_abb, df$y_sub, "pub", f)
  } else {
    paste0(path, "ACS/", y, "/", y, "us", f)
  }
}
