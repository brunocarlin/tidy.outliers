library(recipes)
library(tidy.outliers)
library(OutlierDetection)

rec_obj <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_maha(all_numeric(), -all_outcomes()) %>%
  step_outliers_remove(contains(r"(.outliers)")) %>%
  prep(mtcars)

juice_result <- juice(rec_obj)

tidy_result <- tidy(rec_obj, number = 2)

aggregation_results <- tidy_result$aggregation_results


test_probabilities(aggregation_results)
# Test Passed

test_that("na values create an error", {
  expect_error(recipe(mpg ~ ., data = mtcars2) %>%
    step_outliers_maha(all_numeric(), -all_outcomes()) %>%
    step_outliers_remove(contains(r"(.outliers)")) %>%
    prep(mtcars2))
})
# Test Passed



test_that("juice results works", {
  expect_gte(nrow(mtcars), nrow(juice_result))
})



# Test passed


tidy_rec_obj_not_prep <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_maha(all_numeric(), -all_outcomes()) %>%
  step_outliers_remove(contains(r"(.outliers)")) %>%
  tidy(number = 1)

test_that("tidy not prepped works", {
  expect_equal(all(tidy_rec_obj_not_prep$aggregation_results == 0), expected = T)
  expect_equal(all(tidy_rec_obj_not_prep$outliers == F), expected = T)
})


rec_obj_tune <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_maha(all_numeric(), -all_outcomes()) %>%
  step_outliers_remove(contains(r"(.outliers)"), probability_dropout = tune("dropout"))


rec_param <- tunable(rec_obj_tune)


test_that("tune wrorks", {
  expect_equal(rec_param$name, c("probability_dropout","aggregation_function"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})
