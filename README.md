<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/USAID-SA-SI/tierdrop/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/USAID-SA-SI/tierdrop/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# tierdrop <img src="man/figures/logo.png" align="right" height="120" />


TierDrop is a package to process, validate, and consolidate MER data from 2 primary data sources (TIER data from the National Department of Health (NDoH) and non-TIER data reported from implementing partners) for import to DATIM for USAID South Africa.

This package will allow users to tidy and process data from NDoH into a DATIM import file, consolidate TIER and non-TIER files, and run verification checks to build the Data Quality Reporting Trackers (DQRT).

## Installation

You can install the development version of tierdrop from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("USAID-SA-SI/tierdrop")
```
