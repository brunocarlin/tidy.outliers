
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidy.outliers

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/tidy.outliers)](https://CRAN.R-project.org/package=tidy.outliers)
[![Codecov test
coverage](https://codecov.io/gh/brunocarlin/tidy.outliers/branch/master/graph/badge.svg)](https://codecov.io/gh/brunocarlin/tidy.outliers?branch=master)
[![R-CMD-check](https://github.com/brunocarlin/tidy.outliers/workflows/R-CMD-check/badge.svg)](https://github.com/brunocarlin/tidy.outliers/actions)
<!-- badges: end -->

The goal of tidy.outliers is to allow for easy usage of many outliers
removal methods, I currently plan to implement at least:

-   all methods in the [OutlierDetection
    package](https://cran.r-project.org/web/packages/OutlierDetection/index.html)
-   method of the [lookout package](https://github.com/Sevvandi/lookout)

## Installation

You can not yet install the released version of tidy.outliers from
[CRAN](https://CRAN.R-project.org) with:

``` r
#install.packages("tidy.outliers")
```

And the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("brunocarlin/tidy.outliers")
```

## Example

This is a basic example which shows you how to solve a common problem:

### Load Libraries

``` r
library(recipes)
library(tidy.outliers)
library(OutlierDetection)
```

### Create recipe

``` r
rec_obj <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_maha(all_numeric(), -all_outcomes()) %>%
  prep(mtcars)
```

### Investigate results

``` r
juice(rec_obj) %>% 
  select(.outliers_maha)
#> Registered S3 method overwritten by 'cli':
#>   method     from    
#>   print.boxx spatstat
#> # A tibble: 32 x 1
#>    .outliers_maha
#>             <dbl>
#>  1          0.411
#>  2          0.374
#>  3          0.222
#>  4          0.192
#>  5          0.124
#>  6          0.350
#>  7          0.481
#>  8          0.493
#>  9          0.985
#> 10          0.737
#> # ... with 22 more rows
```

``` r
tidy(rec_obj,number = 1)
#> # A tibble: 32 x 3
#>    index outlier_probability id                 
#>    <int>               <dbl> <chr>              
#>  1     1               0.411 outliers_maha_0Hro0
#>  2     2               0.374 outliers_maha_0Hro0
#>  3     3               0.222 outliers_maha_0Hro0
#>  4     4               0.192 outliers_maha_0Hro0
#>  5     5               0.124 outliers_maha_0Hro0
#>  6     6               0.350 outliers_maha_0Hro0
#>  7     7               0.481 outliers_maha_0Hro0
#>  8     8               0.493 outliers_maha_0Hro0
#>  9     9               0.985 outliers_maha_0Hro0
#> 10    10               0.737 outliers_maha_0Hro0
#> # ... with 22 more rows
```
