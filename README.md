
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rcbpp

<!-- badges: start -->

[![R-CMD-check](https://github.com/CenterOnBudget/Rcbpp/workflows/R-CMD-check/badge.svg)](https://github.com/CenterOnBudget/Rcbpp/actions)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

## Overview

R utilities to streamline CBPP research, including functions for
generating commonly used variables, creating paths to files in the
SharePoint datasets library, and computing basic weighted statistics.
Rcbpp also includes data that frequently come in handy, including state
FIPS codes (along with corresponding state names and postal
abbreviations) and calendar year averages of the CPI-U-RS.

## Installation

To install Rcbpp, run the following code:

``` r
# install.packages("devtools")
devtools::install_github("CenterOnBudget/Rcbpp")
```

## Example

``` r
library(Rcbpp)
library(tidyverse)

mar21 <- read_csv(
  file = sp_cps_asec(2021, f = "csv"),
  col_select = c(marsupwt, a_age, spm_resources, spm_povthreshold)
)

mar21 |>
  make_age_group_var("cps_asec") |>
  mutate(
    pop = TRUE, 
    b100 = spm_resources < spm_povthreshold
  ) |>
  group_by(age_group) |>
  summarize(
    across(c(pop, b100), \(x) wt_sum(x, wt = marsupwt)), 
    .groups = "drop"
  ) |>
  mutate(
    across(c(pop, b100), round), 
    pov_rate = b100 / pop
  )
#> # A tibble: 3 × 4
#>   age_group         pop     b100 pov_rate
#>   <fct>           <dbl>    <dbl>    <dbl>
#> 1 Under 18     72777497  7044256   0.0968
#> 2 18 to 64    197582013 17446130   0.0883
#> 3 65 and over  55835929  5290704   0.0948

with(mar21, wt_quantile(a_age, wt = marsupwt, n = 10))
#> 10% 20% 30% 40% 50% 60% 70% 80% 90% 
#>   8  16  24  31  38  46  54  62  71
```
