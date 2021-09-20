

#' Compute basic weighted statistics
#'
#' Functions for computing basic weighted statistics. These functions are
#' designed to be strict, consistent, and useful within
#' \code{dplyr::summarize()}.
#'
#' @param x A logical or numeric vector.
#' @param wt A numeric vector of weights the same length as \code{x}.
#' @return A numeric vector of length one.
#' @name wt_stats
NULL


#' NULL
#' @rdname wt_stats
#' @export
wt_sum <- function(x, wt) {
  check_wt_stat_args(x, wt)
  sum(x * wt)
}


#' NULL
#' @rdname wt_stats
#' @export
wt_mean <- function(x, wt) {
  check_wt_stat_args(x, wt)
  sum(x * wt) / sum(wt)
}


#' NULL
#' @rdname wt_stats
#' @export
wt_median <- function(x, wt) {
  check_wt_stat_args(x, wt)

  o <- order(x)
  x <- x[o]
  wt <- wt[o]

  share <- wt / sum(wt)
  cum_share <- cumsum(share)
  k <- match(FALSE, cum_share < 0.5)

  if (cum_share[k] == 0.5) {
    (x[k] + x[k + 1]) / 2
  } else {
    x[k]
  }
}


check_wt_stat_args <- function(x, wt) {
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
    stop("`x` and `wt` must not contain any `NA` values", call. = FALSE)
  }
}
