#' Run a Stata .do file in batch mode
#'
#' @description
#' `do_stata()` calls Stata to run a specified `.do` file in [batch
#' mode](https://www.stata.com/support/faqs/windows/batch-mode/).
#'
#' @details
#' ## Specifying the Stata executable
#'
#' `do_stata()` needs to know the location of your Stata executable. To avoid
#' supplying the path to every time, add a STATA_EXE environment variable to
#' your `.Renviron` file. For example, `STATA_EXE='C:/Program
#' Files/Stata18/StataMP-64.exe'`. (Tip: open your `.Renviron` with
#' [usethis::edit_r_environ()].)
#'
#' ## Log file and Stata errors
#'
#' Running a `.do` file in batch mode will create a `.log` file in the same
#' working directory in which the `.do` file ran. This is a feature of Stata
#' batch mode and cannot be disabled.
#'
#' `do_stata()` will **not** throw an error if the `.do` file code errors out in
#' Stata. Stata output and any error messages can be found in the `.log` file.
#'
#' @param file Path to a Stata `.do` file.
#' @param wd Working directory in which to run the `.do` file. Defaults to the
#'   current working directory.
#' @param stata_exe Path to your Stata executable. If `NULL`, the default, the
#'   STATA_EXE environment variable is used.
#'
#' @returns Path to the Stata `.do` file, invisibly.
#'
#' @examplesIf FALSE
#' # Specify the Stata executable
#' do_stata("script.do", stata_exe = "C:/Program Files/Stata18/StataMP-64.exe")
#'
#' # After storing the Stata executable in `.Renviron`
#' do_stata("script.do")
#'
#' @export

do_stata <- function(file, wd = NULL, stata_exe = NULL) {
  if (!fs::file_exists(file)) {
    cli::cli_abort("{.val {file}} does not exist.")
  }
  if (!identical(fs::path_ext(file), "do")) {
    cli::cli_abort("{.val {file}} is not a .do file.")
  }

  log_file <- fs::path_ext_set(file, ".log")

  if (!rlang::is_null(wd)) {
    if (fs::dir_exists(wd)) {
      cli::cli_abort("{.val {wd}} does not exist.")
    }
    log_file <- fs::path(wd, log_file)
  }

  if (rlang::is_null(stata_exe)) {
    stata_exe <- Sys.getenv("STATA_EXE")

    if (identical(stata_exe, "")) {
      cli::cli_abort(c(
        "Stata executable not specified.",
        "To specify the path your Stata executable:",
        "*" = "Supply the path to {.arg stata_exe}",
        "*" = "Add {.field STATA_EXE='path_to_your_stata_executable'} to your {.code .Renviron} file.",
        " " = "Edit your {.code .Renviron} file with {.run usethis::edit_r_environ()}."
      ))
    }
  }

  if (!fs::file_exists(stata_exe)) {
    cli::cli_abort("{.arg {stata_exe}} does not exist.")
  }

  cli::cli_progress_step("Running {.val {file}}")
  invisible(
    processx::run(
      stata_exe,
      c("/e", "do", file),
      wd = wd,
      spinner = TRUE
    )
  )

  cli::cli_progress_message(
    "Stata created {.file {log_file}}",
    current = FALSE,
    .auto_close = FALSE
  )

  invisible(file)
}
