

#' Compute basic weighted statistics
#'
#' Functions for computing basic weighted statistics. These functions are
#' designed to be strict, consistent, and useful within
#' [dplyr::summarize()].
#'
#' If any `NA` values are present in `x` or `wt`, these functions
#' will return `NA`.
#'
#' `wt` must not contain any negative values. `wt` may contain values
#' of zero but must not only contain values of zero.
#'
#' @param x A logical or numeric vector.
#' @param wt A numeric vector of weights the same length as `x`.
#' @param n A numeric vector of length one (i.e., a number) giving the number
#'   of equally sized groups to split `x` into. Note that for `n`
#'   groups, there are `n - 1` quantiles (i.e., cut points). Quantiles
#'   currently supported include the median (`n = 2`), quartiles
#'   (`n = 4`), quintiles (`n = 5`), deciles (`n = 10`), and
#'   ventiles (`n = 20`).
#' @return For all except `wt_quantile()`, a numeric vector of length
#'   one.
#'
#'   For `wt_quantile()`, a named numeric vector of length `n - 1`
#'   unless any `NA` values are present in `x` or `wt`, in which
#'   case a numeric vector of length one (containing `NA`) is returned.
#' @name wt_stats
NULL


#' @rdname wt_stats
#' @export
wt_sum <- function(x, wt) {
  dry_run <- wt_stat_dry_run(x, wt)

  if (is.na(dry_run)) {
    return(dry_run)
  }

  sum(x * wt)
}


#' @rdname wt_stats
#' @export
wt_mean <- function(x, wt) {
  dry_run <- wt_stat_dry_run(x, wt)

  if (is.na(dry_run)) {
    return(dry_run)
  }

  sum(x * wt) / sum(wt)
}


# References:
# https://en.wikipedia.org/wiki/Weighted_median
# https://www.stata.com/manuals/dpctile.pdf (page 11)
# https://www.stata.com/manuals/rsummarize.pdf (pages 9-10)

#' @rdname wt_stats
#' @export
wt_median <- function(x, wt) {
  unname(wt_quantile(x, wt, n = 2))
}

#' @rdname wt_stats
#' @export
wt_quantile <- function(x, wt, n) {

  # Check args -----------------------------------------------------------------

  dry_run <- wt_stat_dry_run(x, wt)

  if (!is_number(n)) {
    stop("`n` must be a number", call. = FALSE)
  }

  if (n %!in% c(2, 4, 5, 10, 20)) {
    stop("`n` must be 2, 4, 5, 10, or 20", call. = FALSE)
  }

  if (is.na(dry_run)) {
    return(dry_run)
  }

  # Prep inputs ----------------------------------------------------------------

  if (is.logical(x)) {
    x <- as.integer(x)
  }

  z <- wt == 0

  if (any(z)) {
    x <- x[!z]
    wt <- wt[!z]
  }

  o <- order(x)
  x <- x[o]
  wt <- wt[o]

  share <- wt / sum(wt)
  cum_share <- cumsum(share)

  # Get quantiles --------------------------------------------------------------

  probs <- seq_len(n - 1) / n

  q <- vector(mode = "numeric", length = length(probs))
  names(q) <- paste0(round(probs * 100), "%")

  for (k in seq_along(probs)) {
    prob <- probs[k]
    i <- match(TRUE, cum_share >= prob)

    if (cum_share[i] == prob) {
      q[k] <- (x[i] + x[i + 1]) / 2
    } else {
      q[k] <- x[i]
    }
  }

  # Return quantiles -----------------------------------------------------------

  q
}


wt_stat_dry_run <- function(x, wt) {
  if (!is.logical(x) && !is.numeric(x)) {
    stop("`x` must be a logical or numeric vector", call. = FALSE)
  }

  if (!is.numeric(wt)) {
    stop("`wt` must be a numeric vector", call. = FALSE)
  }

  if (length(x) != length(wt)) {
    stop("`x` and `wt` must be the same length", call. = FALSE)
  }

  if (any(is.na(x)) || any(is.na(wt))) {
    return(NA_integer_)
  }

  if (any(wt < 0)) {
    stop("`wt` must not contain any negative values", call. = FALSE)
  }

  if (sum(wt) == 0) {
    stop("`wt` must not only contain values of zero", call. = FALSE)
  }

  0L # Arbitrary non-`NA` integer
}
