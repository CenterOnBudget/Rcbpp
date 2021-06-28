

is_number <- function(x) is.numeric(x) && length(x) == 1
is_string <- function(x) is.character(x) && length(x) == 1
`%!in%` <- function(x, table) match(x, table, nomatch = 0) == 0
