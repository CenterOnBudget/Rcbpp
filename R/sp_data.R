

#' Create paths to files in the SharePoint datasets library
#'
#' Functions for creating paths to files in the SharePoint datasets library.
#' These functions are designed to be simple and compact.
#'
#' These functions create one or more file paths based on the user's path to
#' the SharePoint datasets library (as created by
#' \code{\link[=make_sp_data_path]{make_sp_data_path()}}) and the directory
#' structure and file naming convention of each dataset library. These functions
#' do not verify whether the resulting files actually exist.
#'
#' @section Requesting a function:
#' If you'd like to request an \code{sp_*()} function for a dataset you use
#' frequently, please file an
#' \href{https://github.com/CenterOnBudget/Rcbpp/issues}{issue} on GitHub.
#'
#' @param y One or more years.
#' @param m One or more months (specified numerically).
#' @param f Format of data. One of \code{"csv"} or \code{"dta"}.
#' @return A character vector containing the created file paths.
#' @seealso \code{\link[=make_sp_data_path]{make_sp_data_path()}} to get your
#'   basic path to the SharePoint datasets library.
#'
#' @name sp_data
NULL


#' @rdname sp_data
#' @export
sp_acs <- function(y, f) {
  check_y(y)
  check_f(f)

  path <- make_sp_data_path()
  paste0(path, "ACS/", y, "/", y, "us", ".", f)
}


#' @rdname sp_data
#' @export
sp_cps_asec <- function(y, f) {
  check_y(y)
  check_f(f)

  path <- make_sp_data_path()
  paste0(path, "CPS/mar", y, "/mar", y, ".", f)
}


#' @rdname sp_data
#' @export
sp_cps_basic <- function(y, m, f) {
  check_y(y)

  if (!is.numeric(m) || !all(m %in% 1:12)) {
    stop("`m` must be a numeric vector with values ranging from 1 to 12", call. = FALSE)
  }

  check_f(f)

  df <- expand.grid(m = m, y = y)
  df$y_sub <- substr(df$y, 3, 4)
  df$m_abb <- tolower(month.abb)[df$m]

  path <- make_sp_data_path()
  paste0(path, "CPS-BASIC/", df$y, "/", df$m_abb, df$y_sub, "pub", ".", f)
}


#' Create a path to the SharePoint datasets library
#'
#' \code{make_sp_data_path()} creates a path to the SharePoint datasets library
#' for the user.
#'
#' This function comes in handy when you want to create one or more file paths
#' for a dataset that is used infrequently or that does not yet have an
#' \code{\link{sp_data}} function.
#'
#' @return A character vector of length one.
#' @seealso \code{\link{sp_data}} for functions for creating paths to files in
#'   the SharePoint datasets library.
#'
#' @export
make_sp_data_path <- function() {
  sys_info <- Sys.info()

  paste0(
    if (sys_info["sysname"] == "Windows") "C:",
    "/Users/", sys_info["user"],
    "/Center on Budget and Policy Priorities/Datasets - "
  )
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
