# libraries -------------------------------------------------------------------------------------------------------
library(testthat)
library(recipes)
library(tidy.outliers)


# setup -----------------------------------------------------------------------------------------------------------


set.seed(24)

race_data <- tibble(distance = sample(c(2,3,4), size = 1000, replace = T))

race_data <-
  race_data |>
  rowwise() |>
  mutate(final_time = sample(c(rnorm(1, mean = 3, sd = 0.1) * distance,
                               rnorm(1, mean = 7, sd = 0.1) * distance),
                             1,
                             prob = c(0.95,0.05))
  ) |>
  ungroup()


rec_obj <-recipe(final_time ~ distance,data = race_data) |>
  step_outliers_ecdf(everything()) |>
  prep(race_data)


juice_result <- juice(rec_obj)

outlier_score <- juice_result$.outliers_ecdf


# usual cases -----------------------------------------------------------------------------------------------------

test_scores(outlier_score)
# Test Passed


na_values_break_fun(step_outliers_ecdf)
# Test Passed


# tidy method -----------------------------------------------------------------------------------------------------

tidy_result <- tidy(rec_obj, number = 1)

test_that("tidy probs work", {
  expect_equal(nrow(race_data), nrow(tidy_result))
})
# Test passed


# recipe without prep cases ---------------------------------------------------------------------------------------

tidy_rec_obj_not_prep <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_ecdf(everything()) %>%
  tidy(number = 1)

test_that("tidy probs go to NA", {
  expect_equal(all(is.na(tidy_rec_obj_not_prep$outlier_score)), expected = T)
})
# Test passed


# another outcome

rec_obj_another_outcome <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_ecdf(everything(),outcome = 'disp') %>%
  prep(mtcars)


juice_result_another_outcome <- juice(rec_obj_another_outcome)

outlier_score_another_outcome <- juice_result_another_outcome$.outliers_ecdf

test_scores(outlier_score_another_outcome)
# Test Passed


na_values_break_fun(step_outliers_ecdf)
# Test Passed
