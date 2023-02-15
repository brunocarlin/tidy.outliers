# libraries -------------------------------------------------------------------------------------------------------
library(testthat)
library(tidy.outliers)
library(tidymodels)


# setup -----------------------------------------------------------------------------------------------------------

rec_obj <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_maha(all_numeric_predictors()) %>%
  step_outliers_remove(contains(r"(.outliers)")) %>%
  prep(mtcars)

juice_result <- juice(rec_obj)

tidy_result <- tidy(rec_obj, number = 2)

aggregation_results_tbl <- tidy_result$aggregation_results


# usual cases -----------------------------------------------------------------------------------------------------

test_scores(aggregation_results_tbl)
# Test Passed

test_that("na values create an error", {
  expect_error(recipe(mpg ~ ., data = mtcars2) %>%
    step_outliers_maha(all_numeric_predictors()) %>%
    step_outliers_remove(contains(r"(.outliers)")) %>%
    prep(mtcars2))
})
# Test Passed

test_that("juice results works", {
  expect_gte(nrow(mtcars), nrow(juice_result))
})
# Test passed



# recipe without prep cases ---------------------------------------------------------------------------------------


tidy_rec_obj_not_prep <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_maha(all_numeric_predictors()) %>%
  step_outliers_remove(contains(r"(.outliers)")) %>%
  tidy(number = 2)

test_that("tidy not prepped works", {
  expect_equal(all(tidy_rec_obj_not_prep$aggregation_results == 0), expected = T)
  expect_equal(all(tidy_rec_obj_not_prep$outliers == F), expected = T)
})


# tune cases ------------------------------------------------------------------------------------------------------


rec_obj_tune <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_maha(all_numeric_predictors()) %>%
  step_outliers_remove(contains(r"(.outliers)"), score_dropout = tune("dropout"))


rec_param <- tune::tunable(rec_obj_tune)


test_that("tune wrorks", {
  expect_equal(rec_param$name, c("score_dropout", "aggregation_function"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})


# custom functions ------------------------------------------------------------------------------------------------


data(ames)


rec_obj_tune <-
  recipe(Sale_Price ~ Lot_Frontage + Lot_Area, data = ames) %>%
  step_outliers_maha(all_numeric_predictors()) %>%
  step_outliers_lookout(all_numeric(), -contains(r"(.outliers)"), -all_outcomes()) |>
  step_outliers_remove(contains(r"(.outliers)"),
    score_dropout = tune("dropout"),
    aggregation_function = tune("aggregation")
  )


  tune_grid <- rec_obj_tune |>
    extract_parameter_set_dials(
      dropout = dials::dropout(range = c(0.75, 1)),
      aggregation = aggregation()
    )

spline_grid <- grid_max_entropy(tune_grid, size = 10)

lin_mod <-
  linear_reg() |>
  set_engine("lm")

workflow <- workflow() |>
  add_recipe(rec_obj_tune) |>
  add_model(lin_mod)

tune_grid_result <- tune_grid(workflow, resamples = vfold_cv(ames, 2), grid = spline_grid)

best_five <- tune_grid_result |> tune::show_best(metric = "rmse")

test_that("grid search works", {
  expect_gt(max(best_five$mean), min(best_five$mean))
})

