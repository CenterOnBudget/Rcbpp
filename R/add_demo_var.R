
#' Add a categorical race/ethnicity variable to ACS or CPS microdata
#'
#' @description
#' `add_race_eth_var()` adds a 5-level race/ethnicity variable to a data frame
#' of ACS, CPS ASEC, or basic monthly CPS microdata. Levels are:
#'
#' 1. White alone, not Latino
#' 1. Black alone, not Latino
#' 1. Latino (of any race)
#' 1. Asian alone, not Latino
#' 1. Multiracial or another race, not Latino
#'
#' @details
#' For the ACS, years 2005 to present are supported and required variables are
#' `rac1p` and `hisp`. For the CPS ASEC and basic monthly CPS, survey years 2003
#' to present are supported and required variables are `prdtrace` and
#' `pehspnon`.
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param dataset Name of dataset corresponding to `data`, either `"acs"`,
#'   `"cps_asec"`, or `"cps_basic"`.
#' @param name Name (quoted or unquoted) to give the race/ethnicity variable.
#'   Default is `"race_eth"`.
#'
#' @returns An object of the same type as `data`, with a new column for the
#'   race/ethnicity variable.
#'
#' @examples
#' library(dplyr)
#'
#' acs |>
#'   select(rac1p, hisp) |>
#'   add_race_eth_var(dataset = "acs")
#'
#' cps |>
#'   select(prdtrace, pehspnon) |>
#'   add_race_eth_var(dataset = "cps_asec", name = race_category)
#'
#' @export
#'

add_race_eth_var <- function(
    data,
    dataset = c("acs", "cps_asec", "cps_basic"),
    name = "race_eth"
) {

  check_data_frame(data)

  dataset <- rlang::arg_match(dataset)

  if (dataset == "acs") {
    race_var = "rac1p"
    hisp_var = "hisp"
    race_vals = c(white = 1, black = 2, asian = 6)
    hisp_vals = c(2:24)
  }

  if (dataset == "cps_asec") {
    race_var = "prdtrace"
    hisp_var = "pehspnon"
    race_vals = c(white = 1, black = 2, asian = 4)
    hisp_vals = 1
  }

  if (dataset == "cps_basic") {
    race_var = "ptdtrace"
    hisp_var = "pehspnon"
    race_vals = c(white = 1, black = 2, asian = 4)
    hisp_vals = 1

  }

  race_eth_lbl <- c(
    white = "White, not Latino",
    black = "Black, not Latino",
    latino = "Latino (of any race)",
    asian = "Asian, not Latino",
    other = "Multiracial or another race, not Latino"
  )

  name <- rlang::ensym(name)

  dplyr::mutate(
    data,
    "{{name}}" := dplyr::if_else(
      !is.na(.data[[hisp_var]]) & !is.na(.data[[race_var]]),
      dplyr::case_when(
        .data[[hisp_var]] %in% hisp_vals ~ race_eth_lbl["latino"],
        .data[[race_var]] == race_vals["white"] ~ race_eth_lbl["white"],
        .data[[race_var]] == race_vals["black"] ~ race_eth_lbl["black"],
        .data[[race_var]] == race_vals["asian"] ~ race_eth_lbl["asian"],
        .default = race_eth_lbl["other"],
        .ptype = factor(levels = race_eth_lbl)
      ),
      NA
    )
  )

}


#' Add a categorical age group variable to ACS or CPS microdata
#'
#' @description
#' `add_age_group_var()` adds a 3-level age group variable to a
#' data frame of ACS, CPS ASEC, or basic monthly CPS microdata. Levels are:
#'
#' 1. Under 18
#' 1. 18 to 64
#' 1. 65 and over
#'
#' @details
#' For the ACS, years 2005 to present are supported and the required
#' variable is `agep`. For the CPS ASEC, survey years 1998 to present are
#' supported and the required variable is `a_age`. For the basic monthly CPS,
#' years 1998 to present are supported and the required variable is `prtage`.
#'
#' @inherit add_race_eth_var params
#' @param name Name (quoted or unquoted) to give age group variable. Default is
#'   `"age_group"`.
#'
#' @inherit add_race_eth_var return
#'
#' @examples
#' library(dplyr)
#'
#' acs %>%
#'   select(agep) %>%
#'   add_age_group_var(dataset = "acs")
#'
#' cps %>%
#'   select(a_age) %>%
#'   add_age_group_var(dataset = "cps_asec", name = agecat3)
#'
#' @export
#'

add_age_group_var <- function(
    data,
    dataset = c("acs", "cps_asec", "cps_basic"),
    name = "age_group"
) {

  check_data_frame(data)

  dataset <- rlang::arg_match(dataset)

  age_var <- switch(
    dataset,
    "acs" = "agep",
    "cps_asec" = "a_age",
    "cps_basic" = "prtage"
  )

  name <- rlang::ensym(name)

  dplyr::mutate(
    data,
    "{{name}}" := cut(
      .data[[age_var]],
      breaks = c(0, 18, 64, Inf),
      include.lowest = TRUE,
      labels = c("Under 18", "18 to 64", "65 and over")
    )
  )

}
