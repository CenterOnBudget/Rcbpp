
check_data_frame <- function(
    x, arg = rlang::caller_arg(x), call = rlang::caller_env()
) {
  if (!is.data.frame(x)) {
    cli::cli_abort(
      "{.arg arg} must be a data frame, not {.obj_type_friendly x}",
      call = call
    )
  }
}
