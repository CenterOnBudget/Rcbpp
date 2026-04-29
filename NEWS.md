# Rcbpp 1.0.0

## Breaking changes

-   `add_race_eth_var()` and `add_age_group_var()` replace `make_race_eth_var()` and `make_age_group_var()`, respectively. In the new functions, `NA` values in the variables of `data` used to create the new variable no longer throw an error, and `name` may be specified either quoted or unquoted. `add_race_eth_var()` uses a different label than `make_race_eth_var()`for the residual group: "Multiracial or another race, not Latino".

-   `sp_acs()`, `sp_cps_asec()`, and `sp_cps_basic()` have been deprecated and no longer work. Instead, users may construct paths to datasets within the datasets library with with `sp_data_path()`.

-   `stata_exe()` has been deprecated and no longer works. Instead, users may manually add the path to their Stata executable to their `.Renviron` file (open it with `usethis::edit_r_environ()`).

## Enhancements

-   `wt_mean()`, `wt_sum()`, `wt_median()`, and `wt_quantile()` gain an `na.rm` argument to drop cases where either `x` or `wt` are missing.

-   `wt_quantile()` has been enhanced as follows:

    -   The quantiles to be produced may now be specified as a vector of probabilities to new argument `probs`.

    -   Naming of the resulting vector is now controllable via two new arguments: `names` specifies whether or not the result should be named, and `names_format` specifies a how to format the probabilities as names.

-   New function `wt_quantile_df()` is a version of `wt_quantile()` suitable for use with `dplyr::reframe()`. It returns a data frame with two columns, `prob` for the probability and `val` for the sample quantile of `x` corresponding to `prob`.

## Built-in datasets

-   `cpi_u_rs` now contains rows for 2024 and 2025.

-   Added three toy microdatasets for examples and testing: `acs`, `cps`, and `cps_unzapped`

## Other user-facing changes

-   `wt_mean()`, `wt_sum()`, `wt_median()`, and `wt_quantile()` no longer throw an error if all values of `wt` are zero.

-   `wt_quantile()` now always returns a vector of length `n - 1`. Previously, if either `x` or `wt` contained `NA` values, the result would be the single value `NA`.

-   `zap_stata()`'s `df` argument has been renamed `data` for consistency with other functions.

-   `do_stata()` now invisibly returns the path to the Stata `.do` file.

## Lifecycle changes

-   Rcbpp now requires R \>= 4.1, and will follow the [tidyverse's R version support policy](https://www.tidyverse.org/blog/2019/04/r-version-support/) going forward.

-   Added cli, dplyr, fs, lifecycle, rlang, stats, and tibble to imports. As Rcbpp users likely already have the tidyverse installed, Rcbpp will not incur increased installation costs in most cases.

# Rcbpp 0.10.3

-   Hot fix for `sp_*` functions ([#28](https://github.com/CenterOnBudget/Rcbpp/issues/28))

# Rcbpp 0.10.2

-   The `cpi_u_rs` dataset is updated with the latest available figures, for calendar year 2023.

# Rcbpp 0.10.1

-   The `cpi_u_rs` dataset is updated with the latest available figures, for calendar year 2022.

# Rcbpp 0.10.0

-   Added `do_stata()`, a function to execute Stata .do files in [batch mode](https://www.stata.com/support/faqs/windows/batch-mode/) from R. Helper function `stata_exe()` stores the path to the user's Stata executable in `.Renviron` for use by `do_stata()`.

-   Added `fct_statefips()`, a function for converting a numeric vector of state FIPS codes into a labeled factor.

# Rcbpp 0.9.4

-   New functions `sp_path` and `od_path` construct paths to the user's SharePoint and OneDrive directories, respectively.

-   `sp_data_path` is the new name for `make_sp_data_path`. The old function name is retained but will throw a warning.

-   The `cpi_u_rs` dataset is updated with the latest available figures, for calendar year 2021.
