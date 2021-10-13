

#' Make a race/ethnicity variable
#'
#' \code{make_race_eth_var()} creates a 5-level race/ethnicity variable in ACS,
#' CPS ASEC, or basic monthly CPS microdata. Levels include white only, not
#' Latino; Black only, not Latino; Latino (any race); Asian only, not Latino;
#' and another race or multi-racial, not Latino.
#'
#' For the ACS, years 2005 to present are supported and required variables
#' are \code{rac1p} and \code{hisp}. For the CPS ASEC, survey years 2003 to
#' present are supported and required variables are \code{prdtrace} and
#' \code{pehspnon}. For the basic monthly CPS, years 2003 to present are
#' supported and required variables are \code{ptdtrace} and \code{pehspnon}.
#'
#' @section Warning:
#' This function is designed to work with original, Census-based ACS or CPS
#' microdata. (As opposed to Census microdata that have been processed by, for
#' example, IPUMS.) Therefore, prior to using this function you should ensure
#' that your data are behaving according to their official data dictionary.
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

  # Check args -----------------------------------------------------------------

  check_make_var_args(df = df, dataset = dataset, name = name)

  # Get dataset info -----------------------------------------------------------

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

  racevar <- lookup[[dataset]]$racevar
  hispvar <- lookup[[dataset]]$hispvar
  race <- lookup[[dataset]]$race
  hisp <- lookup[[dataset]]$hisp

  # Check data frame -----------------------------------------------------------

  check_make_var_df(df = df, needed_vars = c(racevar, hispvar))

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


#' Make an age group variable
#'
#' \code{make_age_group_var()} creates a 3-level age group variable in ACS,
#' CPS ASEC, or basic monthly CPS microdata. Levels include under 18, 18 to 64,
#' and 65 and over.
#'
#' For the ACS, years 2005 to present are supported and the required variable
#' is \code{agep}. For the CPS ASEC, survey years 1998 to present are supported
#' and the required variable is \code{a_age}. For the basic monthly CPS, years
#' 1998 to present are supported and the required variable is \code{prtage}.
#'
#' @inheritSection make_race_eth_var Warning
#'
#' @param df Data frame to add age group variable to.
#' @param name Name to give age group variable. Defaults to \code{"age_group"}.
#' @inheritParams make_race_eth_var
#' @return A data frame.
#'
#' @export
make_age_group_var <- function(df, dataset, name = "age_group") {

  # Checks args ----------------------------------------------------------------

  check_make_var_args(df = df, dataset = dataset, name = name)

  # Get dataset info -----------------------------------------------------------

  lookup <- c(acs = "agep", cps_asec = "a_age", cps_basic = "prtage")
  agevar <- lookup[dataset]

  # Check data frame -----------------------------------------------------------

  check_make_var_df(df = df, needed_vars = agevar)

  # Make age group var ---------------------------------------------------------

  age_group <- c("Under 18", "18 to 64", "65 and over")
  newvar <- name

  df[[newvar]] <- ifelse(
    df[[agevar]] < 18,
    age_group[1],
    ifelse(
      df[[agevar]] < 65,
      age_group[2],
      age_group[3]
    )
  )

  df[[newvar]] <- factor(df[[newvar]], levels = age_group)

  # Return data frame ----------------------------------------------------------

  df
}


check_make_var_args <- function(df, dataset, name) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame", call. = FALSE)
  }

  if (!is_string(dataset)) {
    stop("`dataset` must be a string", call. = FALSE)
  }

  if (dataset %!in% c("acs", "cps_asec", "cps_basic")) {
    stop(
      '`dataset` must be one of "acs", "cps_asec", or "cps_basic"',
      call. = FALSE
    )
  }

  if (!is_string(name)) {
    stop("`name` must be a string", call. = FALSE)
  }

  if (name %in% names(df)) {
    stop(
      "Invalid `name`, `df` already contains a `", name, "` column",
      call. = FALSE
    )
  }
}


check_make_var_df <- function(df, needed_vars) {
  if (!all(needed_vars %in% names(df))) {
    stop(
      "`df` must contain the following column(s): ",
      paste0("`", needed_vars, "`", collapse = ", "),
      call. = FALSE
    )
  }

  df <- df[, needed_vars, drop = FALSE]

  for (i in seq_along(df)) {
    col <- df[[i]]
    col_name <- names(df)[i]

    if (!is.numeric(col)) {
      stop("Column `", col_name, "` must be numeric", call. = FALSE)
    }

    if (any(is.na(col))) {
      stop("Column `", col_name, "` must not contain any `NA` values", call. = FALSE)
    }
  }
}
