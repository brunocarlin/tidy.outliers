
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

-   the method on the [lookout
    package](https://github.com/Sevvandi/lookout)
-   the maha distance method
-   some other simple outlier detenctions such as IQR

## Installation

You can not yet install the released version of tidy.outliers from
[CRAN](https://CRAN.R-project.org) with:

``` r
#install.packages("tidy.outliers")
```

And the development version from GitHub with:

``` r
# install.packages("devtools")
# devtools::install_github("brunocarlin/tidy.outliers")
```

## Example Mutation

This is a basic example which shows you how to solve a common problem:

### Load Libraries

``` r
library(recipes)
library(tidy.outliers)
```

### Create recipe mutating probabilities

``` r
rec_obj <-
  recipe(mpg ~ ., data = mtcars) |>
  step_outliers_maha(all_numeric(), -all_outcomes()) |>
  step_outliers_lookout(all_numeric(),-contains(r"(.outliers)"),-all_outcomes()) |> 
  prep(mtcars)
```

### Investigate results

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

``` r
tidy(rec_obj,number = 1)
#> # A tibble: 32 x 3
#>    index outlier_probability id                 
#>    <int>               <dbl> <chr>              
#>  1     1               0.411 outliers_maha_Tjwjd
#>  2     2               0.374 outliers_maha_Tjwjd
#>  3     3               0.222 outliers_maha_Tjwjd
#>  4     4               0.192 outliers_maha_Tjwjd
#>  5     5               0.124 outliers_maha_Tjwjd
#>  6     6               0.350 outliers_maha_Tjwjd
#>  7     7               0.481 outliers_maha_Tjwjd
#>  8     8               0.493 outliers_maha_Tjwjd
#>  9     9               0.985 outliers_maha_Tjwjd
#> 10    10               0.737 outliers_maha_Tjwjd
#> # ... with 22 more rows
```

``` r
tidy(rec_obj,number = 2)
#> # A tibble: 32 x 3
#>    index outlier_probability id                    
#>    <int>               <dbl> <chr>                 
#>  1     1                   0 outliers_lookout_PJD8R
#>  2     2                   0 outliers_lookout_PJD8R
#>  3     3                   0 outliers_lookout_PJD8R
#>  4     4                   0 outliers_lookout_PJD8R
#>  5     5                   0 outliers_lookout_PJD8R
#>  6     6                   0 outliers_lookout_PJD8R
#>  7     7                   0 outliers_lookout_PJD8R
#>  8     8                   0 outliers_lookout_PJD8R
#>  9     9                   0 outliers_lookout_PJD8R
#> 10    10                   0 outliers_lookout_PJD8R
#> # ... with 22 more rows
```

## Example filtering

### Create recipe filtering outliers

``` r
rec_obj2 <-
  recipe(mpg ~ ., data = mtcars) |>
  step_outliers_maha(all_numeric(), -all_outcomes()) |>
  step_outliers_lookout(all_numeric(),-contains(r"(.outliers)"),-all_outcomes()) |> 
  step_outliers_remove(contains(r"(.outliers)")) |> 
  prep(mtcars)
```

### Investigate results

We can see that the mtcars dataset got reduced by our function but way
less than if we used just one function on this case

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

And we can get which were the outliers and their probability

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

## Integration with tidymodels

### Load tidymodels

``` r
library(tidymodels)
```

### Get data

``` r
data(ames)


data_split <- ames |>
  mutate(Sale_Price = log10(Sale_Price)) |>
  initial_split(strata = Sale_Price)
ames_train <- training(data_split)
ames_test  <- testing(data_split)
```

### create the recipe

``` r
ames_rec <- 
  recipe(Sale_Price ~ Gr_Liv_Area + Longitude + Latitude, data = ames_train) |> 
  step_log(Gr_Liv_Area, base = 10) |> 
  step_ns(Longitude, deg_free = tune("long df")) |> 
  step_ns(Latitude,  deg_free = tune("lat df")) |> 
  step_outliers_maha(all_numeric(), -all_outcomes()) |>
  step_outliers_lookout(all_numeric(),-contains(r"(.outliers)"),-all_outcomes()) |> 
  step_outliers_remove(contains(r"(.outliers)"),probability_dropout = tune("dropout"),aggregation_function = tune("aggregation"))
```

### See the parameters

``` r
parameters(ames_rec)
#> Collection of 4 parameters for tuning
#> 
#>   identifier                 type    object
#>      long df             deg_free nparam[+]
#>       lat df             deg_free nparam[+]
#>      dropout  probability_dropout nparam[+]
#>  aggregation aggregation_function dparam[+]
```

### There is already a function for dropouts implemented by dials

``` r
ames_param <- 
  ames_rec |> 
  parameters() |> 
  update(
    `long df` = spline_degree(), 
    `lat df` = spline_degree(),
    dropout = dropout(range = c(0.75, 1)),
    aggregation = aggregation() |> value_set(c("mean","weighted_mean"))
  )
```

## Create weighted_mean func

``` r
weighted_mean <- function(x) {
  x[[1]] * .75 + x[[2]] * .25
}
```

### Grid Search picks random points

``` r
spline_grid <- grid_max_entropy(ames_param, size = 20)
spline_grid
#> # A tibble: 20 x 4
#>    `long df` `lat df` dropout aggregation  
#>        <int>    <int>   <dbl> <chr>        
#>  1         6        7   0.849 mean         
#>  2         3        2   0.858 mean         
#>  3         7        4   0.826 weighted_mean
#>  4        10       10   0.980 mean         
#>  5         1        5   0.921 weighted_mean
#>  6         9       10   0.984 weighted_mean
#>  7         7        1   0.825 mean         
#>  8         9        2   0.985 weighted_mean
#>  9         3        7   0.894 mean         
#> 10         7        4   0.908 mean         
#> 11         2        6   0.835 mean         
#> 12         2       10   0.842 weighted_mean
#> 13         4        3   0.979 mean         
#> 14         7       10   0.783 mean         
#> 15         6       10   0.954 mean         
#> 16         6        6   0.807 weighted_mean
#> 17         9        9   0.870 mean         
#> 18         9        5   0.972 weighted_mean
#> 19         5        3   0.956 weighted_mean
#> 20         2        3   0.762 mean
```

### create a simple model

``` r
lin_mod <-
  linear_reg() |>
  set_engine("lm")
```

### create a simple workflow

``` r
wf_tune <- workflow() |>
  add_recipe(ames_rec) |> 
  add_model(lin_mod)
```

### create training folds

``` r
set.seed(2453)
cv_splits <- vfold_cv(ames_train, v = 5, strata = Sale_Price)
```

### Tune the grid

``` r
ames_res <- tune_grid(wf_tune, resamples = cv_splits, grid = spline_grid)
```

``` r
estimates <- collect_metrics(ames_res)

rmse_vals <- 
  estimates |> 
  dplyr::filter(.metric == "rmse") |> 
  arrange(mean)
rmse_vals
#> # A tibble: 20 x 10
#>    `long df` `lat df` dropout aggregation   .metric .estimator   mean     n
#>        <int>    <int>   <dbl> <chr>         <chr>   <chr>       <dbl> <int>
#>  1         9       10   0.984 weighted_mean rmse    standard   0.0965     5
#>  2        10       10   0.980 mean          rmse    standard   0.0967     5
#>  3         6       10   0.954 mean          rmse    standard   0.0969     5
#>  4         9        9   0.870 mean          rmse    standard   0.0969     5
#>  5         7       10   0.783 mean          rmse    standard   0.0973     5
#>  6         2       10   0.842 weighted_mean rmse    standard   0.0975     5
#>  7         6        7   0.849 mean          rmse    standard   0.0987     5
#>  8         9        5   0.972 weighted_mean rmse    standard   0.0988     5
#>  9         3        7   0.894 mean          rmse    standard   0.0992     5
#> 10         6        6   0.807 weighted_mean rmse    standard   0.0992     5
#> 11         5        3   0.956 weighted_mean rmse    standard   0.0997     5
#> 12         2        6   0.835 mean          rmse    standard   0.0997     5
#> 13         7        4   0.908 mean          rmse    standard   0.0998     5
#> 14         1        5   0.921 weighted_mean rmse    standard   0.0998     5
#> 15         7        4   0.826 weighted_mean rmse    standard   0.0998     5
#> 16         9        2   0.985 weighted_mean rmse    standard   0.100      5
#> 17         4        3   0.979 mean          rmse    standard   0.101      5
#> 18         2        3   0.762 mean          rmse    standard   0.103      5
#> 19         3        2   0.858 mean          rmse    standard   0.103      5
#> 20         7        1   0.825 mean          rmse    standard   0.111      5
#> # ... with 2 more variables: std_err <dbl>, .config <chr>
```

## Plot it

``` r
autoplot(ames_res,metric = "rmse")
```

<img src="man/figures/README-unnamed-chunk-15-1.png" width="100%" />
