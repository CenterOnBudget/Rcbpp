
#' Install the path to your Stata executable in your `.Renviron` file
#'
#' This function will add the path to your Stata executable to your `.Renviron`
#' file. After you have installed your Stata path, it can be called at any time
#' with `Sys.getenv("STATA_EXE")`. [do_stata()] will automatically use
#' `STATA_EXE` when running `.do` files in batch mode.
#'
#' If you do not have an `.Renviron` file, the function will create one for you.
#' If you already have an `.Renviron` file, the function will append the key to
#' your existing file, while making a backup of your original file for disaster
#' recovery purposes.
#'
#' @param path Path to your Stata executable
#' @param install Install the path in your `.Renviron` file for use in future
#'   sessions
#' @param overwrite Overwrite an existing STATA_EXE that you already have in
#'   your `.Renviron` file
#'
#' @importFrom utils write.table read.table
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' stata_exe("C:/Program Files/Stata17/StataMP-64.exe", install = TRUE)
#' # After first installation, reload your environment or restart R
#' readRenviron("~/.Renviron")
#'
#' }
#'

stata_exe <- function(path, install = FALSE, overwrite = FALSE) {

  # Adapted from tidycensus::census_api_key()

  if (!file.exists(path)) {
    stop("Stata executable not found at the path specified.")
  }


  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if (file.exists(renv)) {
      # Backup original .Renviron before doing anything else here
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if (!file.exists(renv)) {
      file.create(renv)
    } else {
      if (overwrite) {
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.") #nolint
        oldenv <- read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[-grep("STATA_EXE", oldenv), ]
        write.table(
          newenv, renv, quote = FALSE, sep = "\n",
          col.names = FALSE, row.names = FALSE
        )
      } else {
        tv <- readLines(renv)
        if (any(grepl("STATA_EXE", tv))) {
          stop("STATA_EXE already exists in your .Renviron. You can overwrite it with the argument `overwrite = TRUE`", call. = FALSE) #nolint
        }
      }
    }

    stata_exe_var <- paste0("STATA_EXE='", path, "'")
    # Append Stata path to .Renviron file
    write(stata_exe_var, renv, sep = "\n", append = TRUE)
    message('The path to your Stata executable has been stored in your .Renviron and can be accessed by Sys.getenv("STATA_EXE"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`') #nolint
    return(path)

  } else {
    message("To install the path to your Stata executable for use in future sessions, run this function with `install = TRUE`.") #nolint
    Sys.setenv(STATA_EXE = path)
  }

}


#' Run a Stata `.do` file in batch mode
#'
#' Run a Stata `.do` file in batch mode from the command line. It is recommended
#' to install the path to your Stata executable first with [stata_exe()].
#'
#' @param file A path to a Stata `.do` file.
#' @param wd Working directory in which to run the `.do` file. If `NULL`, the
#'   default, the current working directory is used.
#' @param stata_exe The path to your Stata executable. If `NULL`, the default,
#'   will use the STATA_EXE environment variable.
#'
#' @seealso [stata_exe()]
#' @export
#'
#' @examples
#
#' \dontrun{
#'
#' # Install the path to your Stata executable first
#' stata_exe("YOUR STATA EXE")
#' # Run a do file in batch mode
#' do_stata("script.do")
#'
#' # Run a do file in batch mode, specifying the Stata executable
#' do_stata("script.do", stata_exe = "C:/Program Files/Stata17/StataMP-64.exe")
#'
#' }
#'

do_stata <- function(file, wd = NULL, stata_exe = NULL) {

  if (!file.exists(file)) {
    stop("The `.do` file you have supplied does not exist.")
  }

  if (is.null(stata_exe)) {
    stata_exe <- Sys.getenv("STATA_EXE")
    if (stata_exe == "") {
      stop("You have not set up the path to your Stata executable. \nPlease supply the path to the `stata_exe` argument, or use the `stata_exe()` function to store the path for this session.") #nolint
    }
  }

  if (!is.null(stata_exe) && !file.exists(stata_exe)) {
    stop("The Stata executable path you have supplied does not exist.")
  }

  invisible(
    processx::run(
      stata_exe,
      c("/e", "do", file),
      wd = wd,
      spinner = TRUE
    )
  )

}
