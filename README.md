
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

-   the method on the [lookout package](https://github.com/Sevvandi/lookout)
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
devtools::install_github("brunocarlin/tidy.outliers")
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
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_maha(all_numeric(), -all_outcomes()) %>%
  step_outliers_lookout(all_numeric(),-contains(r"(.outliers)"),-all_outcomes()) %>% 
  prep(mtcars)
```

### Investigate results

``` r
juice(rec_obj) %>% 
  select(contains(r"(.outliers)")) %>% 
  arrange(.outliers_lookout %>% desc())
#> Registered S3 method overwritten by 'cli':
#>   method     from    
#>   print.boxx spatstat
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
#>  1     1               0.411 outliers_maha_gjygR
#>  2     2               0.374 outliers_maha_gjygR
#>  3     3               0.222 outliers_maha_gjygR
#>  4     4               0.192 outliers_maha_gjygR
#>  5     5               0.124 outliers_maha_gjygR
#>  6     6               0.350 outliers_maha_gjygR
#>  7     7               0.481 outliers_maha_gjygR
#>  8     8               0.493 outliers_maha_gjygR
#>  9     9               0.985 outliers_maha_gjygR
#> 10    10               0.737 outliers_maha_gjygR
#> # ... with 22 more rows
```

``` r
tidy(rec_obj,number = 2)
#> # A tibble: 32 x 3
#>    index outlier_probability id                    
#>    <int>               <dbl> <chr>                 
#>  1     1                   0 outliers_lookout_rwNzw
#>  2     2                   0 outliers_lookout_rwNzw
#>  3     3                   0 outliers_lookout_rwNzw
#>  4     4                   0 outliers_lookout_rwNzw
#>  5     5                   0 outliers_lookout_rwNzw
#>  6     6                   0 outliers_lookout_rwNzw
#>  7     7                   0 outliers_lookout_rwNzw
#>  8     8                   0 outliers_lookout_rwNzw
#>  9     9                   0 outliers_lookout_rwNzw
#> 10    10                   0 outliers_lookout_rwNzw
#> # ... with 22 more rows
```

## Example filtering

### Create recipe filtering outliers

``` r
rec_obj2 <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_maha(all_numeric(), -all_outcomes()) %>%
  step_outliers_lookout(all_numeric(),-contains(r"(.outliers)"),-all_outcomes()) %>% 
  step_outliers_remove(contains(r"(.outliers)")) %>% 
  prep(mtcars)
```

### Investigate results

We can see that the mtcars dataset got reduced by our function but way
less than if we used just one function on this case

``` r
juice(rec_obj2) %>% glimpse()
#> Rows: 31
#> Columns: 11
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
#> $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17...
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
tidy(rec_obj2,number = 3) %>% 
  arrange(aggregation_results %>% desc())
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
  step_outliers_lookout(all_numeric(),-contains(r"(.outliers)"),-all_outcomes()) %>% 
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
  ames_rec %>% 
  parameters() %>% 
  update(
    `long df` = spline_degree(), 
    `lat df` = spline_degree(),
    dropout = dropout(range = c(0.75, 1)),
    aggregation = aggregation() %>% value_set(c("mean","weighted_mean"))
  )
```

## Create weighted\_mean func

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
#>  1         2        3   0.983 mean         
#>  2         3        9   0.992 weighted_mean
#>  3         9        3   0.766 mean         
#>  4         9        1   0.995 weighted_mean
#>  5        10        9   0.857 weighted_mean
#>  6         5       10   0.878 mean         
#>  7         5        1   0.839 weighted_mean
#>  8         9        3   0.808 weighted_mean
#>  9         6        6   0.843 weighted_mean
#> 10         2        4   0.954 weighted_mean
#> 11         9        1   0.992 mean         
#> 12         5        3   0.841 mean         
#> 13         9        7   0.980 weighted_mean
#> 14         3        9   0.838 weighted_mean
#> 15         2        2   0.797 mean         
#> 16         2        4   0.790 weighted_mean
#> 17         8        9   0.982 mean         
#> 18        10        7   0.888 mean         
#> 19         2        8   0.877 weighted_mean
#> 20         9        6   0.752 weighted_mean
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
#> # A tibble: 20 x 10
#>    `long df` `lat df` dropout aggregation .metric .estimator   mean     n
#>        <int>    <int>   <dbl> <chr>       <chr>   <chr>       <dbl> <int>
#>  1         8        9   0.982 mean        rmse    standard   0.0987    10
#>  2         5       10   0.878 mean        rmse    standard   0.0988    10
#>  3        10        9   0.857 weighted_m~ rmse    standard   0.0989    10
#>  4         3        9   0.992 weighted_m~ rmse    standard   0.0993    10
#>  5         2        8   0.877 weighted_m~ rmse    standard   0.0994    10
#>  6         3        9   0.838 weighted_m~ rmse    standard   0.0995    10
#>  7         9        7   0.980 weighted_m~ rmse    standard   0.100     10
#>  8        10        7   0.888 mean        rmse    standard   0.101     10
#>  9         9        6   0.752 weighted_m~ rmse    standard   0.101     10
#> 10         6        6   0.843 weighted_m~ rmse    standard   0.101     10
#> 11         9        3   0.808 weighted_m~ rmse    standard   0.101     10
#> 12         9        3   0.766 mean        rmse    standard   0.102     10
#> 13         5        3   0.841 mean        rmse    standard   0.102     10
#> 14         2        4   0.954 weighted_m~ rmse    standard   0.102     10
#> 15         2        3   0.983 mean        rmse    standard   0.102     10
#> 16         2        2   0.797 mean        rmse    standard   0.104     10
#> 17         2        4   0.790 weighted_m~ rmse    standard   0.106     10
#> 18         9        1   0.995 weighted_m~ rmse    standard   0.110     10
#> 19         9        1   0.992 mean        rmse    standard   0.110     10
#> 20         5        1   0.839 weighted_m~ rmse    standard   0.112     10
#> # ... with 2 more variables: std_err <dbl>, .config <chr>
```

## Plot it

``` r
autoplot(ames_res,metric = "rmse")
```

<img src="man/figures/README-unnamed-chunk-15-1.png" width="100%" />
