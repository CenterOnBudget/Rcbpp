# Rcbpp 0.10.2

- The `cpi_u_rs` dataset is updated with the latest available figures, for calendar year 2023.


# Rcbpp 0.10.1

- The `cpi_u_rs` dataset is updated with the latest available figures, for calendar year 2022.


# Rcbpp 0.10.0

- Added `do_stata()`, a function to execute Stata .do files in [batch mode](https://www.stata.com/support/faqs/windows/batch-mode/) from R. Helper function `stata_exe()` stores the path to the user's Stata executable in `.Renviron` for use by `do_stata()`.

- Added `fct_statefips()`, a function for converting a numeric vector of state FIPS codes into a labeled factor. 


# Rcbpp 0.9.4

- New functions `sp_path` and `od_path` construct paths to the user's SharePoint and OneDrive directories, respectively. 

- `sp_data_path` is the new name for `make_sp_data_path`. The old function name is retained but will throw a warning.

- The `cpi_u_rs` dataset is updated with the latest available figures, for calendar year 2021.

