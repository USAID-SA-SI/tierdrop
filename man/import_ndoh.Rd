% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_ndoh.R
\name{import_ndoh}
\alias{import_ndoh}
\title{Import NDOH tabs}
\usage{
import_ndoh(filepath, qtr, kp = FALSE)
}
\arguments{
\item{filepath}{provide the full file path for the NDOH import file}

\item{qtr}{return the quarter of the NDOH reporting file}

\item{kp}{processes KP tabs if TRUE and non-KP tabs if FALSE (default kp = FALSE)}
}
\description{
Import NDOH tabs
}
\examples{
 \dontrun{
   ndoh_all <- import_ndoh(qtr = "Q3", kp = FALSE)
   ndoh_kp <- import_ndoh(qtr = "Q3", kp = TRUE)
}

}
