library(recipes)
library(tidy.outliers)
library(OutlierDetection)

rec_obj <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_maha(all_numeric(),-all_outcomes()) %>%
  step_outliers_remove(contains(r"(.outliers)")) %>%
  prep(mtcars)

juice_result <- juice(rec_obj)

tidy_result <- tidy(rec_obj,number = 2)

aggregation_results <- tidy_result$aggregation_results

test_that("aggregation results make sense", {
  expect_gte(min(aggregation_results),0)
  expect_lte(min(aggregation_results),1)
  expect_equal(any(is.na(aggregation_results)),expected = F)
})

#> Test passed 😸


test_that("na values create an error", {
  expect_error(recipe(mpg ~ ., data = mtcars2) %>%
                 step_outliers_maha(all_numeric(),-all_outcomes()) %>%
                 step_outliers_remove(contains(r"(.outliers)")) %>%
                 prep(mtcars2))
})

#> Test passed 😀


test_that("juice results works", {
  expect_gte(nrow(mtcars),nrow(juice_result))
})



#> Test passed 😀
