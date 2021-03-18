

#' Make race/ethnicity variable
#'
#' \code{make_race_eth_var()} creates a 5-level race/ethnicity variable in ACS,
#' CPS ASEC, or basic monthly CPS microdata. Levels include white only, not
#' Latino; Black only, not Latino; Latino (any race); Asian only, not Latino;
#' and another race or multi-racial, not Latino.
#'
#' \code{make_race_eth_var()} is designed to work with original, Census-based
#' ACS or CPS microdata. (As opposed to Census microdata that have been
#' processed by, for examples, IPUMS.) Therefore, prior to using this function,
#' you should ensure that your data are behaving according to their official
#' data dictionary.
#'
#' For the ACS, years 2005 to present are supported and required variables
#' are \code{rac1p} and \code{hisp}. For the CPS ASEC, survey years 2003 to
#' present are supported and required variables are \code{prdtrace} and
#' \code{pehspnon}. For the basic monthly CPS, years 2003 to present are
#' supported and required variables are \code{ptdtrace} and \code{pehspnon}.
#'
#' In order to use \code{make_race_eth_var()}, the required variables for a
#' given \code{dataset} must have lowercase names, be \code{numeric}, and not
#' contain any \code{NA} values.
#'
#' @param df Data frame to add race/ethnicity variable to.
#' @param dataset Dataset corresponding to \code{df}. One of \code{"acs"},
#'   \code{"cps_asec"}, or \code{"cps_basic"}.
#' @param name Name to give race/ethnicity variable. Defaults to
#'   \code{"race_eth"}.
#' @return A data frame.
#'
#' @export
make_race_eth_var <- function(df, dataset, name = "race_eth") {

  # Preliminary checks ---------------------------------------------------------

  if (!is.data.frame(df)) {
    stop("`df` must be a data frame", call. = FALSE)
  }

  if (length(dataset) != 1 || !is.character(dataset)) {
    stop("`dataset` must be a string", call. = FALSE)
  }

  datasets <- c("acs", "cps_asec", "cps_basic")

  if (!(dataset %in% datasets)) {
    stop(
      "`dataset` must be one of `acs`, `cps_asec`, or `cps_basic`",
      call. = FALSE
    )
  }

  if (length(name) != 1 || !is.character(name)) {
    stop("`name` must be a string", call. = FALSE)
  }

  # Dataset info ---------------------------------------------------------------

  lookup <- list(
    acs = list(
      # Good for years 2005 to present
      # https://www.census.gov/programs-surveys/acs/microdata/documentation.html
      racevar = "rac1p",
      hispvar = "hisp",
      race = c(white = 1, black = 2, asian = 6),
      hisp = 2:24 # As in, YES Hispanic
    ),
    cps_asec = list(
      # Good for survey years 2003 to present
      # https://www.census.gov/data/datasets/time-series/demo/cps/cps-asec.html
      racevar = "prdtrace",
      hispvar = "pehspnon",
      race = c(white = 1, black = 2, asian = 4),
      hisp = 1
    ),
    cps_basic = list(
      # Good for years 2003 to present
      # https://www.census.gov/data/datasets/time-series/demo/cps/cps-basic.html
      racevar = "ptdtrace",
      hispvar = "pehspnon",
      race = c(white = 1, black = 2, asian = 4),
      hisp = 1
    )
  )

  # Data frame checks ----------------------------------------------------------

  racevar <- lookup[[dataset]]$racevar
  hispvar <- lookup[[dataset]]$hispvar
  race <- lookup[[dataset]]$race
  hisp <- lookup[[dataset]]$hisp

  needed_vars <- c(racevar, hispvar)

  if (!all(needed_vars %in% names(df))) {
    stop(
      glue::glue(
        "If `dataset` is `{dataset}`, `df` must contain `{racevar}` and ",
        "`{hispvar}` columns"
      ),
      call. = FALSE
    )
  }

  if (name %in% needed_vars) {
    stop(
      glue::glue("Cannot use `{racevar}` or `{hispvar}` for `name`"),
      call. = FALSE
    )
  }

  if (name %in% names(df)) {
    stop(
      glue::glue("`df` cannot already contain `{name}` column"),
      call. = FALSE
    )
  }

  if (!is.numeric(df[[racevar]]) || !is.numeric(df[[hispvar]])) {
    stop(
      glue::glue("`{racevar}` and `{hispvar}` columns must be numeric"),
      call. = FALSE
    )
  }

  if (any(is.na(df[[racevar]])) || any(is.na(df[[hispvar]]))) {
    stop(
      glue::glue(
        "`{racevar}` and `{hispvar}` columns cannot contain `NA` values"
      ),
      call. = FALSE
    )
  }

  # Make race/ethnicity var ----------------------------------------------------

  race_eth <- c(
    white = "White only, not Latino",
    black = "Black only, not Latino",
    latino = "Latino (any race)",
    asian = "Asian only, not Latino",
    other = "Another race or multi-racial, not Latino"
  )

  newvar <- name

  df[[newvar]] <- ifelse(
    df[[hispvar]] %in% hisp,
    race_eth["latino"],
    ifelse(
      df[[racevar]] == race["white"],
      race_eth["white"],
      ifelse(
        df[[racevar]] == race["black"],
        race_eth["black"],
        ifelse(
          df[[racevar]] == race["asian"],
          race_eth["asian"],
          race_eth["other"]
        )
      )
    )
  )

  df[[newvar]] <- factor(df[[newvar]], levels = race_eth)

  # Return data frame ----------------------------------------------------------

  df
}
