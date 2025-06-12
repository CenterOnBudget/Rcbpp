
#' Compute weighted statistics
#'
#' @description
#' - `wt_mean()` produces the weighted arithmetic mean.
#' - `wt_sum()` produces the weighted sum.
#' - `wt_median()` is a simple wrapper around `wt_quantile()` that produces
#' the median.
#' - `wt_quantile()` and `wt_quantiles_df()` produce weighted sample quantiles
#' corresponding to the given probabilities or, alternatively, number of
#' quantiles.
#'
#' @param x A logical or numeric vector.
#' @param wt A numeric vector of frequency weights the same length as `x`.
#'   Negative weights are not permitted.
#' @param na.rm Should cases with `NA` values in `x` or `wt` be removed?
#' @param n,probs Either of two arguments may be used to specify the quantiles
#'   to be produced:
#'   - `n`: A single integer giving the number of quantiles.
#'   - `probs`: A numeric vector of probabilities with values greater than 0 and
#'   less than 1.
#' @param names If `TRUE`, the result will have [names] of the form specified in
#'   `names_format`.
#' @param names_format A function or formula to transform the probabilities into
#'   a vector of names. Default is `\(p) paste0(round(p * 100, 1), "%")`. Only
#'   used when `names` is `TRUE`.
#'
#'
#' @returns
#' - `wt_sum()`, `wt_mean()`, and `wt_median()`: A numeric vector of length one.
#' - `wt_quantile()`: When the `n` argument is used, a numeric vector of length
#' `n - 1`. When the `probs` argument is used, a numeric vector of length
#' `length(probs)`.
#' - `wt_quantile_df()`: A tibble with two columns: `prob` for the probability
#' and `val` for the sample quantile of `x` corresponding to `prob`.
#'
#' @details
#' With `na.rm = TRUE`, only complete cases of `x` and `wt` are included in the
#' calculation. This matches the behavior of
#' [collapse::fast-statistical-functions] and [collapse::fquantile()], and
#' deviates from the behavior of [stats::weighted.mean()] which always produces
#' `NA` if there are any missing weights.
#'
#' Quantiles are computed using the type 2 quantile algorithm described in
#' Hyndman and Fan (1996), which is Stata's default formula for percentiles.
#'
#' @references
#' Hyndman, R. J. and Fan, Y. (1996) Sample quantiles in statistical packages,
#' American Statistician 50, 361–365. doi:10.2307/2684934.
#'
#' StataCorp (2023) pctile — Create variable containing percentile, Stata 18
#' Base Reference Manual, College Station, TX: Stata Press.
#'
#' @examples
#' library(dplyr)
#'
#' cps |>
#'   summarize(
#'     mean = wt_mean(ptotval, wt = marsupwt),
#'     median = wt_median(ptotval, wt = marsupwt),
#'     p75 = wt_quantile(ptotval, wt = marsupwt, probs = 0.75)
#'   )
#'
#' # `wt_quantiles_df()` is designed for use with `dplyr::reframe()`
#' acs |>
#'   reframe(
#'     wt_quantile_df(agep, wt = pwgtp, n = 4)
#'   )
#'
#' @name wt_stats

NULL


#' @rdname wt_stats
#' @export
wt_sum <- function(x, wt, na.rm = FALSE) {

  check_wt_inputs(x, wt, na.rm)

  if (na.rm) {
    complete_cases <- complete.cases(x, wt)
    x <- x[complete_cases]
    wt <- wt[complete_cases]
  }

  sum(x * wt)

}


#' @rdname wt_stats
#' @export
wt_mean <- function(x, wt, na.rm = FALSE) {

  check_wt_inputs(x, wt, na.rm)

  if (na.rm) {
    complete_cases <- complete.cases(x, wt)
    x <- x[complete_cases]
    wt <- wt[complete_cases]
  }

  weighted.mean(x, wt)

}


#' @rdname wt_stats
#' @export
wt_median <- function(x, wt, na.rm = FALSE) {
  wt_quantile(x = x, wt = wt, n = 2, na.rm = na.rm, names = FALSE)
}


#' @rdname wt_stats
#' @export
wt_quantile <- function(
    x,
    wt,
    n,
    probs,
    na.rm = FALSE,
    names = TRUE,
    names_format = \(p) paste0(round(p * 100), "%")
) {

  rlang::check_exclusive(n, probs)

  if (!missing(probs)) {
    if (
      !is.numeric(probs) ||
      any(probs >= 1, probs <= 0, is.na(probs), is.null(probs), na.rm = TRUE)
    ) {
      cli::cli_abort(
        "{.arg probs} must be a numeric vector with values greater than 0 and less than 1"
      )
    }
  }

  if (!missing(n)) {
    if (!rlang::is_scalar_integerish(n, finite = TRUE)) {
      cli::cli_abort(
        "{.arg n} must be a single integer, not {.obj_type_friendly n}"
      )
    }
    if ((n < 2) || (n > 100)) {
      cli::cli_abort(
        "{.arg n} must be between 2 and 100"
      )
    }
    probs <- seq_len(n - 1) / n
  }

  check_wt_inputs(x, wt, na.rm)

  if (is.logical(x)) {
    x <- as.integer(x)
  }

  if (na.rm) {
    # Drop cases with NA in x or wt
    complete_cases <- complete.cases(x, wt)
    x <- x[complete_cases]
    wt <- wt[complete_cases]
  }

  # Initialize output vector
  q <- vector(mode = "numeric", length = length(probs))


  # Don't bother proceeding if there are NAs in x or wt; result will be NA
  any_missing <- any(is.na(x), is.na(wt))
  if (any_missing) {
    q[] <- NA
  }

  if (!any_missing) {

    # Drop cases with 0 wt
    zero_weight <- wt == 0
    if (any(zero_weight)) {
      x <- x[!zero_weight]
      wt <- wt[!zero_weight]
    }

    # Stata's default formula for percentiles is Hyndman and Fan (1996) type 2
    # https://www.stata.com/manuals/dpctile.pdf#page=11

    o <- order(x)
    x <- x[o]
    wt <- wt[o]

    share <- wt / sum(wt)
    cum_share <- cumsum(share)

    for (k in seq_along(probs)) {
      prob <- probs[k]
      i <- match(TRUE, cum_share >= prob)
      if (cum_share[i] == prob) {
        q[k] <- (x[i] + x[i + 1]) / 2
      } else {
        q[k] <- x[i]
      }
    }

  }

  if (names) {

    names_format <- rlang::as_function(names_format)

    nms <- names_format(probs)

    if (length(nms) != length(probs)) {
      cli::cli_abort(
        "{.arg names_format} must return a vector the same length as {.arg probs}"
      )
    }

    names(q) <- nms

  }

  q

}


#' @rdname wt_stats
#' @export
wt_quantile_df <- function(x, wt, n, probs, na.rm = FALSE) {

  quantiles <- wt_quantile(
    x = x,
    wt = wt,
    n = n,
    probs = probs,
    na.rm = na.rm,
    names = FALSE
  )

  if (!rlang::is_missing(n)) {
    probs <- seq_len(n - 1) / n
  }

  tibble::tibble(
    prob = probs,
    value = quantiles
  )

}


check_wt_inputs <- function(x, wt, na.rm, call = rlang::caller_env()) {

  if (!(is.numeric(x) | rlang::is_logical(x))) {
    cli::cli_abort(
      "{.arg x} must be a numeric or logical vector, not {.obj_type_friendly {x}}",
      call = call
    )
  }
  if (!is.numeric(wt)) {
    cli::cli_abort(
      "{.arg wt} must be a numeric vector, not {.obj_type_friendly {wt}}",
      call = call
    )
  }
  if (length(x) != length(wt)) {
    cli::cli_abort(c(
      "{.arg x} and {.arg wt} must be the same length",
      "{.arg x} has length {length(x)} and {.arg wt} has length {length(wt)}"
    ),
    call = call
    )
  }
  if (any(wt[!is.na(wt)] < 0)) {
    cli::cli_abort(
      "{.arg wt} may not contain negative values",
      call = call

    )
  }
  if (!na.rm) {

    x_has_na <- any(is.na(x))
    wt_has_na <- any(is.na(wt))
    na_args <- c("x", "wt")[c(x_has_na, wt_has_na)]

    if (x_has_na || wt_has_na) {
      cli::cli_warn(c(
        "{.arg {na_args}} contain{?s/} missing values; result will be `NA`",
        "i" = "Set {.arg na.rm = TRUE} to remove cases with `NA` values in {.arg x} or {.arg wt}"
      ))
    }

  }

}
