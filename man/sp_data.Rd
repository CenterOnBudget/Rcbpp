% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sp_data.R
\name{sp_data}
\alias{sp_data}
\alias{sp_acs}
\alias{sp_cps_asec}
\alias{sp_cps_basic}
\title{Create paths to files in the SharePoint datasets library}
\usage{
sp_acs(y, f)

sp_cps_asec(y, f)

sp_cps_basic(y, m, f)
}
\arguments{
\item{y}{One or more years.}

\item{f}{Format of data. One of \code{"csv"}, \code{"dta"}, or \code{"parquet"}.}

\item{m}{One or more months (specified numerically).}
}
\value{
A character vector containing the created file paths.
}
\description{
Functions for creating paths to files in the SharePoint datasets library.
These functions are designed to be simple and compact.
}
\details{
These functions create one or more file paths based on the user's path to
the SharePoint datasets library (as created by
\code{\link[=sp_data_path]{sp_data_path()}} and the directory
structure and file naming convention of each dataset library. These functions
do not verify whether the resulting files actually exist.
}
\section{Requesting a function}{

If you'd like to request an \verb{sp_*()} function for a dataset you use
frequently, please file an
\href{https://github.com/CenterOnBudget/Rcbpp/issues}{issue} on GitHub.
}

\seealso{
\code{\link[=sp_data_path]{sp_data_path()}} to get your
basic path to the SharePoint datasets library.
}
