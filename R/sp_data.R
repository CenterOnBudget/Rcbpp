

#' Create paths to files in the SharePoint datasets library
#'
#' Functions for creating paths to files in the SharePoint datasets library.
#' These functions are designed to be simple and compact.
#'
#' These functions create one or more file paths based on the user's path to
#' the SharePoint datasets library (accessed via environment variable
#' \code{SP_DATA_PATH}) and the directory structure and file naming convention
#' of each dataset library. These functions do not verify whether the resulting
#' files actually exist.
#'
#' @param y One or more years.
#' @param m One or more months (specified numerically).
#' @param f Format of data. One of \code{"csv"} or \code{"dta"}.
#' @return A character vector containing the created file paths.
#' @name sp_data
NULL


#' @rdname sp_data
#' @export
sp_acs <- function(y, f) {
  path <- get_spdatapath()

  check_y(y)
  check_f(f)

  paste0(path, "ACS/", y, "/", y, "us", ".", f)
}


#' @rdname sp_data
#' @export
sp_cps_asec <- function(y, f) {
  path <- get_spdatapath()

  check_y(y)
  check_f(f)

  paste0(path, "CPS/mar", y, "/mar", y, ".", f)
}


#' @rdname sp_data
#' @export
sp_cps_basic <- function(y, m, f) {
  path <- get_spdatapath()

  check_y(y)

  if (!is.numeric(m) || !all(m %in% 1:12)) {
    stop("`m` must be a numeric vector with values ranging from 1 to 12", call. = FALSE)
  }

  check_f(f)

  df <- expand.grid(m = m, y = y)
  df$y_sub <- substr(df$y, 3, 4)
  df$m_abb <- tolower(month.abb)[df$m]

  paste0(path, "CPS-BASIC/", df$y, "/", df$m_abb, df$y_sub, "pub", ".", f)
}


get_spdatapath <- function() {
  path <- Sys.getenv("SP_DATA_PATH")

  if (path == "") {
    stop(
      "Path to SharePoint datasets library not found, supply with env var `SP_DATA_PATH`",
      call. = FALSE
    )
  }

  path
}

check_y <- function(y) {
  if (!is.numeric(y)) {
    stop("`y` must be a numeric vector", call. = FALSE)
  }
}

check_f <- function(f) {
  if (!is_string(f)) {
    stop("`f` must be a string", call. = FALSE)
  }

  if (f %!in% c("csv", "dta")) {
    stop('`f` must be one of "csv" or "dta"', call. = FALSE)
  }
}
