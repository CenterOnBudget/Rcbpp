

#' Create a path to SharePoint and OneDrive directories
#'
#' @description
#' `sp_path()` creates a path to the user's SharePoint directory.
#'
#' `sp_data_path()` creates a path to the SharePoint datasets library
#' for the user.
#'
#' `od_path()` creates a path to the user's OneDrive.
#'
#' These functions do not verify whether the resulting directories actually
#' exist.
#'
#' @param path Unique file path string. Defaults to NULL.
#' @return A character vector of length one (invisibly).
#' @aliases make_sp_data_path
#' @seealso [`sp_data`] for functions for creating paths to files in
#'   the SharePoint datasets library.
#'
#' @name sp_path
NULL


#' @rdname sp_path
#' @export
sp_data_path <- function(path = NULL) {
  paste0(user_home(), "/Center on Budget and Policy Priorities/Datasets - ",
         path)
}

#' @export
make_sp_data_path <- function() {
  .Deprecated("sp_data_path")
  sp_data_path()
}

#' @rdname sp_path
#' @export
od_path <- function(path = NULL) {
  paste(user_home(), "OneDrive - Center on Budget and Policy Priorities", path,
        sep = "/")
}

#' @rdname sp_path
#' @export
sp_path <- function(path = NULL) {
  paste(user_home(), "Center on Budget and Policy Priorities", path, sep = "/")
}


user_home <- function() {
  sys_info <- Sys.info()
  if (sys_info["sysname"] == "Windows") {
    root <- "C:/"
  } else {
    root <- "/"
  }
  paste0(root, "Users/", sys_info["user"])
}
