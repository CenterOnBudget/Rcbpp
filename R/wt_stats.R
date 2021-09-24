

#' Compute basic weighted statistics
#'
#' Functions for computing basic weighted statistics. These functions are
#' designed to be strict, consistent, and useful within
#' \code{dplyr::summarize()}.
#'
#' @param x A logical or numeric vector.
#' @param wt A numeric vector of weights the same length as \code{x}.
#' @param nq A numeric vector of length one (i.e., a number) giving the number
#'   of quantiles. Quantiles currently supported include the median
#'   (\code{nq = 2}), quartiles (\code{nq = 4}), quintiles (\code{nq = 5}),
#'   deciles (\code{nq = 10}), and ventiles (\code{nq = 20}).
#' @return For all except \code{wt_quantile()}, a numeric vector of length
#'   one.
#'
#'   For \code{wt_quantile()}, a named numeric vector of length \code{nq - 1}
#'   unless any \code{NA} values are present in \code{x} or \code{wt}, in which
#'   case a single \code{NA} is returned.
#' @name wt_stats
NULL


#' @rdname wt_stats
#' @export
wt_sum <- function(x, wt) {
  check_wt_stat_args(x, wt)

  if (wt_stat_any_na(x, wt)) {
    return(NA_integer_)
  }

  check_valid_wt(wt)
  sum(x * wt)
}


#' @rdname wt_stats
#' @export
wt_mean <- function(x, wt) {
  check_wt_stat_args(x, wt)

  if (wt_stat_any_na(x, wt)) {
    return(NA_integer_)
  }

  check_valid_wt(wt)
  sum(x * wt) / sum(wt)
}


# References:
# https://en.wikipedia.org/wiki/Weighted_median
# https://www.stata.com/manuals/dpctile.pdf (page 11)
# https://www.stata.com/manuals/rsummarize.pdf (pages 9-10)

#' @rdname wt_stats
#' @export
wt_median <- function(x, wt) {
  wt_med <- wt_quantile(x, wt, nq = 2)
  unname(wt_med)
}

#' @rdname wt_stats
#' @export
wt_quantile <- function(x, wt, nq) {

  # Check args -----------------------------------------------------------------

  check_wt_stat_args(x, wt)

  if (!is_number(nq)) {
    stop("`nq` must be a number", call. = FALSE)
  }

  if (nq %!in% c(2, 4, 5, 10, 20)) {
    stop("`nq` must be 2, 4, 5, 10, or 20", call. = FALSE)
  }

  if (wt_stat_any_na(x, wt)) {
    return(NA_integer_)
  }

  check_valid_wt(wt)

  # Prep inputs ----------------------------------------------------------------

  if (is.logical(x)) {
    x <- as.integer(x)
  }

  o <- order(x)
  x <- x[o]
  wt <- wt[o]

  share <- wt / sum(wt)
  cum_share <- cumsum(share)

  # Get quantiles --------------------------------------------------------------

  q <- seq_len(nq - 1) / nq

  output <- vector(mode = "numeric", length = length(q))
  names(output) <- paste0(round(q * 100), "%")

  for (k in seq_along(q)) {
    i <- match(TRUE, cum_share >= q[k])

    if (cum_share[i] == q[k]) {
      output[k] <- (x[i] + x[i + 1]) / 2
    } else {
      output[k] <- x[i]
    }
  }

  output
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
}

wt_stat_any_na <- function(x, wt) {
  any(is.na(x)) || any(is.na(wt))
}

check_valid_wt <- function(wt) {
  if (any(wt < 0)) {
    stop("`wt` must not contain any negative values", call. = FALSE)
  }

  if (sum(wt) == 0) {
    stop("`wt` must not only contain values of zero", call. = FALSE)
  }
}
