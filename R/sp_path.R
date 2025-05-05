
#' Create a path within SharePoint and OneDrive directories
#'
#' @description
#' Each of these functions constructs a path within synced OneDrive or
#' SharePoint directories.
#'
#' - `sp_path()` constructs a path to the user's SharePoint directory.
#' - `od_path()` constructs a path to the user's OneDrive directory.
#' - `sp_data_path()` is used to construct paths to datasets library folders
#'    within the user's SharePoint directory. For example, `sp_data_path("ACS",
#'    "2023")` is equivalent to `sp_path("Datasets - ACS", "2023")`
#'
#' @param ... Additional paths appended to the directory by [fs::path()].
#'
#' @returns A character vector of paths.
#'
#' @examplesIf interactive()
#' sp_path()
#'
#' od_path("my_folder/my_file.csv")
#'
#' sp_data_path("CPS-BASIC", "2023", paste0(tolower(month.abb), "23pub.dta"))
#'
#' @name sp_path

NULL


#' @rdname sp_path
#' @export
sp_path <- function(...) {
  fs::path_home(cbpp(), ...)
}

#' @rdname sp_path
#' @export
sp_data_path <- function(...) {
  fs::path(sp_path(), paste("Datasets -", fs::path(...)))
}

#' @rdname sp_path
#' @export
od_path <- function(...) {
  fs::path(Sys.getenv("OneDrive"), ...)
}

cbpp <- function() {
  "Center on Budget and Policy Priorities"
}


