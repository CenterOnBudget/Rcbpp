#' Create paths to files in a synced datasets library
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions have been deprecated and no longer work. Instead, please use
#' [sp_data_path()] to construct paths to datasets within the library.
#'
#' @param y One or more years.
#' @param m One or more months (specified numerically).
#' @param f Format of data. One of `"csv"`, `"dta"`, or `"parquet"`.
#'
#' @seealso [sp_data_path()]
#'
#' @name sp_data
#'
#' @keywords internal

NULL

#' @rdname sp_data
#' @export
sp_acs <- function(y, f) {
  lifecycle::deprecate_stop(
    when = "1.0.0",
    what = "sp_acs()",
    details = "Please DIY with {.fun sp_data_path()} instead"
  )
}

#' @rdname sp_data
#' @export
sp_cps_asec <- function(y, f) {
  lifecycle::deprecate_stop(
    when = "1.0.0",
    what = "sp_cps_asec()",
    details = "Please DIY with {.fun sp_data_path()} instead"
  )
}

#' @rdname sp_data
#' @export
sp_cps_basic <- function(y, m, f) {
  lifecycle::deprecate_stop(
    when = "1.0.0",
    what = "sp_cps_basic()",
    details = "Please DIY with {.fun sp_data_path()} instead"
  )
}


#' Install the path to your Stata executable in your `.Renviron` file
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated as we no longer want to programmatically modify
#' the user's `.Renviron`. Instead, users may seamlessly edit `.Renviron` with
#' [usethis::edit_r_environ()].
#'
#' @param path Path to your Stata executable
#' @param install Install the path in your `.Renviron` file for use in future
#'   sessions
#' @param overwrite Overwrite an existing STATA_EXE that you already have in
#'   your `.Renviron` file
#'
#' @keywords internal
#'
#' @export

stata_exe <- function(path, install = FALSE, overwrite = FALSE) {
  lifecycle::deprecate_stop(
    when = "1.0.0",
    what = "stata_exe()",
    details = "Please DIY with {.fun usethis::edit_r_environ} instead"
  )
}


#' Make a race/ethnicity variable
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been replaced by [add_race_eth_var()], a nearly-drop-in
#' replacement, and no longer works.
#'
#' @param df Data frame to add race/ethnicity variable to.
#' @param dataset Dataset corresponding to `df`. One of `"acs"`, `"cps_asec"`,
#'   or `"cps_basic"`.
#' @param name Name to give race/ethnicity variable. Defaults to `"race_eth"`.
#' @return A data frame.
#'
#' @seealso [add_race_eth_var()]
#'
#' @keywords internal
#'
#' @export

make_race_eth_var <- function(df, dataset, name = "race_eth") {
  lifecycle::deprecate_stop(
    "1.0.0",
    "make_race_eth_var()",
    "add_race_eth_var()"
  )
}

#' Make an age group variable
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been replaced by [add_age_group_var()], a nearly-drop-in
#' replacement, and no longer works.
#'
#' @param df Data frame to add age group variable to.
#' @param name Name to give age group variable. Defaults to `"age_group"`.
#' @inheritParams make_race_eth_var
#'
#' @return A data frame.
#'
#' @seealso [add_age_group_var()]
#'
#' @keywords internal
#'
#' @export

make_age_group_var <- function(df, dataset, name = "age_group") {
  lifecycle::deprecate_stop(
    "1.0.0",
    "make_age_group_var()",
    "add_age_group_var()"
  )
}
