
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidy.outliers

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/tidy.outliers)](https://CRAN.R-project.org/package=tidy.outliers)
[![Codecov test
coverage](https://codecov.io/gh/brunocarlin/tidy.outliers/branch/main/graph/badge.svg)](https://codecov.io/gh/brunocarlin/tidy.outliers?branch=master)
[![R-CMD-check](https://github.com/brunocarlin/tidy.outliers/workflows/R-CMD-check/badge.svg)](https://github.com/brunocarlin/tidy.outliers/actions)

<!-- badges: end -->

The goal of tidy.outliers is to allow for easy usage of many outliers
removal methods, currently implemented are:

Simple methods:

-   Univariate based function
-   Mahalanobis distance
-   ecdf

Model Methods:

-   [lookout](https://github.com/Sevvandi/lookout)
-   [h2o.extendedIsolationForest](https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/eif.html)
-   [outForest](https://github.com/mayer79/outForest)

# What are outlier scores?

The package works on the principal that all basic step_outlier\_\*
functions return an outlier “score” that can be used for filtering
outliers where 0 is a very low outlier score and 1 is a very high
outlier score, so you could filter, for example all rows where the
outlier score is greater than .9.

# Installation

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

# Usage

## Load Libraries

``` r
library(recipes)
library(tidy.outliers)
```

## Create a recipe for calculation the outlier scores

I keep the mpg as an example outcome since you should remove outlier
from your outcome, you also shouldn’t remove outlier from testing data
so the default is to skip the steps of the package when predicting.

``` r
rec_obj <-
  recipe(mpg ~ ., data = mtcars) |>
  step_outliers_maha(all_numeric(), -all_outcomes()) |>
  step_outliers_lookout(all_numeric(),-contains(r"(.outliers)"),-all_outcomes()) |> 
  prep(mtcars)
```

## Return scores

``` r
juice(rec_obj) |> 
  select(contains(r"(.outliers)")) |> 
  arrange(.outliers_lookout |> desc())
#> # A tibble: 32 x 2
#>    .outliers_maha .outliers_lookout
#>             <dbl>             <dbl>
#>  1          0.959            1     
#>  2          0.967            0.506 
#>  3          0.951            0.403 
#>  4          0.654            0.108 
#>  5          0.864            0.0795
#>  6          0.741            0.0787
#>  7          0.411            0     
#>  8          0.374            0     
#>  9          0.222            0     
#> 10          0.192            0     
#> # ... with 22 more rows
```

# Example filtering based on scores

## Create recipe filtering outliers

``` r
rec_obj2 <-
  recipe(mpg ~ ., data = mtcars) |>
  step_outliers_maha(all_numeric(), -all_outcomes()) |>
  step_outliers_lookout(all_numeric(),-contains(r"(.outliers)"),-all_outcomes()) |> 
  step_outliers_remove(contains(r"(.outliers)")) |> 
  prep(mtcars)
```

## Returns the filtered rows

We filtered one row from the dataset and and automatically removed the
extra outlier columns.

``` r
juice(rec_obj2) |> glimpse()
#> Rows: 31
#> Columns: 11
#> $ cyl  <dbl> 6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 8,~
#> $ disp <dbl> 160.0, 160.0, 108.0, 258.0, 360.0, 225.0, 360.0, 146.7, 140.8, 16~
#> $ hp   <dbl> 110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123, 180, 180, 180~
#> $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.92, 3.92,~
#> $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, 3.150, 3.~
#> $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20.00, 22.90, 18~
#> $ vs   <dbl> 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0,~
#> $ am   <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0,~
#> $ gear <dbl> 4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3, 3,~
#> $ carb <dbl> 4, 4, 1, 1, 2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1, 2, 1, 1, 2,~
#> $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17.8,~
```

``` r
mtcars |> glimpse()
#> Rows: 32
#> Columns: 11
#> $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17.8,~
#> $ cyl  <dbl> 6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 8,~
#> $ disp <dbl> 160.0, 160.0, 108.0, 258.0, 360.0, 225.0, 360.0, 146.7, 140.8, 16~
#> $ hp   <dbl> 110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123, 180, 180, 180~
#> $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.92, 3.92,~
#> $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, 3.150, 3.~
#> $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20.00, 22.90, 18~
#> $ vs   <dbl> 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0,~
#> $ am   <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0,~
#> $ gear <dbl> 4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3, 3,~
#> $ carb <dbl> 4, 4, 1, 1, 2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1, 2, 1, 1, 2,~
```

## Investigate why some rows were filtered

And we can get which were the outliers and their score

``` r
tidy(rec_obj2,number = 3) |> 
  arrange(aggregation_results |> desc())
#> # A tibble: 32 x 3
#>    index outliers aggregation_results
#>    <int> <lgl>                  <dbl>
#>  1    31 TRUE                   0.980
#>  2    29 FALSE                  0.736
#>  3    27 FALSE                  0.677
#>  4     9 FALSE                  0.493
#>  5    19 FALSE                  0.472
#>  6    28 FALSE                  0.410
#>  7    30 FALSE                  0.381
#>  8    21 FALSE                  0.372
#>  9    10 FALSE                  0.369
#> 10    24 FALSE                  0.347
#> # ... with 22 more rows
```

# Integration with tidymodels

The package was made to play nice with tune and friends from tidymodels
check out the article on our [github pkgdown
page](https://brunocarlin.github.io/tidy.outliers/articles/integration_tidymodels.html)!

# Next steps

Although it is possible to manually change the function of model
parameters using the options argument it would be nice to add the option
to tune those internal parameters as well.

So instead of this.

``` r
rec_obj2 <-
  recipe(mpg ~ ., data = mtcars) |> 
  step_outliers_outForest(
    all_numeric(),
    -all_outcomes(),
    options = list(
    impute_multivariate_control = list(
      num.trees = 200
    )
  ))
```

You would write something like this

``` r
rec_obj2 <-
  recipe(mpg ~ ., data = mtcars) |> 
  step_outliers_outForest(
    all_numeric(),
    -all_outcomes(),
    options = list(
    impute_multivariate_control = list(
      num.trees = tune::tune('tree')
    )
  ))
#> Registered S3 method overwritten by 'tune':
#>   method                   from   
#>   required_pkgs.model_spec parsnip
```

The main problem is that this would require manually going model by
model and incorporating those arguments as tunable components.
