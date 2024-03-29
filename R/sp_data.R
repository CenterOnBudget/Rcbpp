

#' Create paths to files in the SharePoint datasets library
#'
#' Functions for creating paths to files in the SharePoint datasets library.
#' These functions are designed to be simple and compact.
#'
#' These functions create one or more file paths based on the user's path to
#' the SharePoint datasets library (as created by
#' [sp_data_path()] and the directory
#' structure and file naming convention of each dataset library. These functions
#' do not verify whether the resulting files actually exist.
#'
#' @section Requesting a function:
#' If you'd like to request an `sp_*()` function for a dataset you use
#' frequently, please file an
#' [issue](https://github.com/CenterOnBudget/Rcbpp/issues) on GitHub.
#'
#' @param y One or more years.
#' @param m One or more months (specified numerically).
#' @param f Format of data. One of `"csv"`, `"dta"`, or `"parquet"`.
#' @return A character vector containing the created file paths.
#' @seealso [sp_data_path()] to get your
#'   basic path to the SharePoint datasets library.
#'
#' @name sp_data
NULL


#' @rdname sp_data
#' @export
sp_acs <- function(y, f) {
  check_y(y)
  check_f(f)

  if (f == "csv") {
    stop('"csv" file does not exist', call. = FALSE)
  }

  path <- sp_data_path()
  paste0(path, "ACS/", y, "/", y, "us", ".", f)
}


#' @rdname sp_data
#' @export
sp_cps_asec <- function(y, f) {
  check_y(y)
  check_f(f)

  if (f == "parquet") {
    stop('"parquet" file does not exist', call. = FALSE)
  }

  path <- sp_data_path()
  paste0(path, "CPS/mar", y, "/mar", y, ".", f)
}


#' @rdname sp_data
#' @export
sp_cps_basic <- function(y, m, f) {
  check_y(y)

  if (!is.numeric(m) || !all(m %in% 1:12)) {
    stop(
      "`m` must be a numeric vector with values ranging from 1 to 12",
      call. = FALSE
    )
  }

  check_f(f)

  if (f == "parquet") {
    stop('"parquet" file does not exist', call. = FALSE)
  }

  df <- expand.grid(m = m, y = y)
  df$y_sub <- substr(df$y, 3, 4)
  df$m_abb <- tolower(month.abb)[df$m]

  path <- sp_data_path()
  paste0(path, "CPS-BASIC/", df$y, "/", df$m_abb, df$y_sub, "pub", ".", f)
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

  if (f %!in% c("csv", "dta", "parquet")) {
    stop('`f` must be one of "csv", "dta", or "parquet"', call. = FALSE)
  }
}
