# libraries -------------------------------------------------------------------------------------------------------
library(testthat)
library(recipes)
library(tidy.outliers)


# setup -----------------------------------------------------------------------------------------------------------

rec_obj <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_maha(all_numeric_predictors()) %>%
  prep(mtcars)


juice_result <- juice(rec_obj)

outlier_score <- juice_result$.outliers_maha


# usual cases -----------------------------------------------------------------------------------------------------

test_scores(outlier_score)
# Test Passed


na_values_break_fun(step_outliers_maha)
# Test Passed


# tidy method -----------------------------------------------------------------------------------------------------

tidy_result <- tidy(rec_obj, number = 1)

test_that("tidy probs work", {
  expect_equal(nrow(mtcars), nrow(tidy_result))
})
# Test passed


# recipe without prep cases ---------------------------------------------------------------------------------------

tidy_rec_obj_not_prep <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_maha(all_numeric_predictors()) %>%
  tidy(number = 1)

test_that("tidy probs go to NA", {
  expect_equal(all(is.na(tidy_rec_obj_not_prep$outlier_score)), expected = T)
})
# Test passed
