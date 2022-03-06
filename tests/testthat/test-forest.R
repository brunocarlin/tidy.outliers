# libraries -------------------------------------------------------------------------------------------------------

library(purrr)
library(recipes)
library(tidy.outliers)


# setup -----------------------------------------------------------------------------------------------------------

rec_obj <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_forest(all_numeric(), -all_outcomes()) %>%
  prep(mtcars)

juice_result <- juice(rec_obj)

outlier_score <- juice_result$.outliers_forest


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
# Test Passed


# recipe without prep cases ---------------------------------------------------------------------------------------

tidy_rec_obj_not_prep <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_forest(all_numeric(), -all_outcomes()) %>%
  tidy(number = 1)

test_that("tidy probs go to NA", {
  expect_equal(all(is.na(tidy_rec_obj_not_prep$outlier_score)), expected = T)
})
# Test passed


# original_result -------------------------------------------------------------------------------------------------

rec_obj_original_result <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_forest(all_numeric(), -all_outcomes(), original_result = TRUE) %>%
  prep(mtcars)

tibbles_to_test_original_result <- rec_obj_original_result |>
  juice() |>
  select(contains("outliers")) |>
  mutate(scores = map(.outliers_forest$score, \(x) x |> pull(score))) |>
  summarise(
    max = max(scores |> unlist(), na.rm = TRUE),
    min = min(scores |> unlist(), na.rm = TRUE),
    some_nas = max(scores |> unlist())
  )

test_that("orignal result is return valid results", {
  expect_gt(tibbles_to_test_original_result$min, 0)
  expect_gt(tibbles_to_test_original_result$max, tibbles_to_test_original_result$min)
  expect_identical(tibbles_to_test_original_result$some_nas, NA_real_)
})


# outlier_score_function  -----------------------------------------------------------------------------------------

rec_obj_outlier_score_function <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_forest(all_numeric(), -all_outcomes(), outlier_score_function = \(x) {
    sum(x) / sum(x)
  }) %>%
  prep(mtcars)

juice_result_outlier_score_function <- juice(rec_obj_outlier_score_function)

outlier_score_outlier_score_function <- juice_result_outlier_score_function$.outliers_forest

non_zeroes <- outlier_score_outlier_score_function[outlier_score_outlier_score_function != 0]

test_that("custom functions are changing the results", {
  expect_identical(max(non_zeroes), min(non_zeroes))
})
# Test passed
