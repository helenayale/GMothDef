
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GMothDef

<!-- badges: start -->
<!-- badges: end -->

GMothDef is designed for the study of spongy moth defoliation. The goal
is to to run each data processing in one line.

## Installation

You can install the development version of GMothDef from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("helenayale/GMothDef")
```

## Functions

``` r
## Current functions include:
# dat_tiff(): import and extract data from a set of Tiff files, each of which include same amount of band layers ordered in the same way
# dat_folder(): import and extract data from single layer files grouped by band names in separate folders
# dat_int(): interpolate data
# dat_smth(): smooth data with LOESS smoothing method
# smth_span(): compare the smoothing results with different smoothing spans
# rf_reg(): build random forest regression model
# predict_dat(): predict data using a random forest model
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(GMothDef)
## basic example code
```
