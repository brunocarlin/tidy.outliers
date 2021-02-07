
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

## Example Mutation

This is a basic example which shows you how to solve a common problem:

### Load Libraries

``` r
library(recipes)
library(tidy.outliers)
library(OutlierDetection)
```

### Create recipe mutating probabilities

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
#>  1     1               0.411 outliers_maha_mYwUv
#>  2     2               0.374 outliers_maha_mYwUv
#>  3     3               0.222 outliers_maha_mYwUv
#>  4     4               0.192 outliers_maha_mYwUv
#>  5     5               0.124 outliers_maha_mYwUv
#>  6     6               0.350 outliers_maha_mYwUv
#>  7     7               0.481 outliers_maha_mYwUv
#>  8     8               0.493 outliers_maha_mYwUv
#>  9     9               0.985 outliers_maha_mYwUv
#> 10    10               0.737 outliers_maha_mYwUv
#> # ... with 22 more rows
```

## Example filtering

### Create recipe filtering outliers

``` r
rec_obj2 <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_maha(all_numeric(), -all_outcomes()) %>%
  step_outliers_remove(contains(r"(.outliers)")) %>% 
  prep(mtcars)
```

### Investigate results

We can see that the mtcars dataset got reduced by our function

``` r
juice(rec_obj2) %>% glimpse()
#> Rows: 28
#> Columns: 11
#> $ cyl  <dbl> 6, 6, 4, 6, 8, 6, 8, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 8,...
#> $ disp <dbl> 160.0, 160.0, 108.0, 258.0, 360.0, 225.0, 360.0, 146.7, 167.6,...
#> $ hp   <dbl> 110, 110, 93, 110, 175, 105, 245, 62, 123, 123, 180, 180, 180,...
#> $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.92, 3....
#> $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, 3.440,...
#> $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20.00, 18.30,...
#> $ vs   <dbl> 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0,...
#> $ am   <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0,...
#> $ gear <dbl> 4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3, 3,...
#> $ carb <dbl> 4, 4, 1, 1, 2, 1, 4, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1, 2, 1, 1, 2,...
#> $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 19.2, 17.8, 16...
```

``` r
mtcars %>% glimpse()
#> Rows: 32
#> Columns: 11
#> $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17...
#> $ cyl  <dbl> 6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4,...
#> $ disp <dbl> 160.0, 160.0, 108.0, 258.0, 360.0, 225.0, 360.0, 146.7, 140.8,...
#> $ hp   <dbl> 110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123, 180, 180, ...
#> $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.92, 3....
#> $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, 3.150,...
#> $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20.00, 22.90,...
#> $ vs   <dbl> 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,...
#> $ am   <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0,...
#> $ gear <dbl> 4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3,...
#> $ carb <dbl> 4, 4, 1, 1, 2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1, 2, 1, 1,...
```

And we can get which were the outliers and their probability

``` r
tidy(rec_obj2,number = 2) %>% 
  arrange(aggregation_results %>% desc())
#> # A tibble: 32 x 3
#>    index outliers aggregation_results
#>    <int> <lgl>                  <dbl>
#>  1     9 TRUE                   0.985
#>  2    29 TRUE                   0.967
#>  3    31 TRUE                   0.959
#>  4    27 TRUE                   0.951
#>  5    19 FALSE                  0.864
#>  6    21 FALSE                  0.745
#>  7    28 FALSE                  0.741
#>  8    10 FALSE                  0.737
#>  9    24 FALSE                  0.693
#> 10    30 FALSE                  0.654
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


data_split <- ames %>%
  mutate(Sale_Price = log10(Sale_Price)) %>%
  initial_split(strata = Sale_Price)
ames_train <- training(data_split)
ames_test  <- testing(data_split)
```

### create the recipe

``` r
ames_rec <- 
  recipe(Sale_Price ~ Gr_Liv_Area + Longitude + Latitude, data = ames_train) %>% 
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_ns(Longitude, deg_free = tune("long df")) %>% 
  step_ns(Latitude,  deg_free = tune("lat df")) %>% 
  step_outliers_maha(all_numeric(), -all_outcomes()) %>%
  step_outliers_remove(contains(r"(.outliers)"),probability_dropout = tune("dropout"))
```

### See the parameters

``` r
parameters(ames_rec)
#> Collection of 3 parameters for tuning
#> 
#>  identifier                type    object
#>     long df            deg_free nparam[+]
#>      lat df            deg_free nparam[+]
#>     dropout probability_dropout nparam[+]
```

### There is already a function for dropouts implemented by dials

``` r
ames_param <- 
  ames_rec %>% 
  parameters() %>% 
  update(
    `long df` = spline_degree(), 
    `lat df` = spline_degree(),
    dropout = dropout(range = c(0.75, 1))
  )
```

### Grid Search picks random points

``` r
spline_grid <- grid_max_entropy(ames_param, size = 10)
spline_grid
#> # A tibble: 10 x 3
#>    `long df` `lat df` dropout
#>        <int>    <int>   <dbl>
#>  1         6        1   0.779
#>  2         3       10   0.836
#>  3         4        1   0.929
#>  4         9        4   0.769
#>  5         6       10   0.753
#>  6         5        9   0.991
#>  7         9        3   0.980
#>  8         9        9   0.919
#>  9        10        8   0.811
#> 10         2        5   0.759
```

### create a simple model

``` r
lin_mod <-
  linear_reg() %>%
  set_engine("lm")
```

### create a simple workflow

``` r
wf_tune <- workflow() %>%
  add_recipe(ames_rec) %>% 
  add_model(lin_mod)
```

### create training folds

``` r
set.seed(2453)
cv_splits <- vfold_cv(ames_train, v = 10, strata = Sale_Price)
```

### Tune the grid

``` r
ames_res <- tune_grid(wf_tune, resamples = cv_splits, grid = spline_grid)
```

``` r
estimates <- collect_metrics(ames_res)

rmse_vals <- 
  estimates %>% 
  dplyr::filter(.metric == "rmse") %>% 
  arrange(mean)
rmse_vals
#> # A tibble: 10 x 9
#>    `long df` `lat df` dropout .metric .estimator   mean     n std_err .config   
#>        <int>    <int>   <dbl> <chr>   <chr>       <dbl> <int>   <dbl> <chr>     
#>  1         9        9   0.919 rmse    standard   0.0961    10 0.00239 Preproces~
#>  2         5        9   0.991 rmse    standard   0.0963    10 0.00234 Preproces~
#>  3        10        8   0.811 rmse    standard   0.0970    10 0.00246 Preproces~
#>  4         3       10   0.836 rmse    standard   0.0974    10 0.00247 Preproces~
#>  5         6       10   0.753 rmse    standard   0.0976    10 0.00231 Preproces~
#>  6         9        3   0.980 rmse    standard   0.0989    10 0.00239 Preproces~
#>  7         9        4   0.769 rmse    standard   0.0999    10 0.00231 Preproces~
#>  8         4        1   0.929 rmse    standard   0.109     10 0.00248 Preproces~
#>  9         6        1   0.779 rmse    standard   0.111     10 0.00250 Preproces~
#> 10         2        5   0.759 rmse    standard   0.174     10 0.00651 Preproces~
```

## Plot it

``` r
autoplot(ames_res,metric = "rmse")
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="100%" />
