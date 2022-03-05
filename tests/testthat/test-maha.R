library(recipes)
library(tidy.outliers)


rec_obj <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_maha(all_numeric(), -all_outcomes()) %>%
  prep(mtcars)


juice_result <- juice(rec_obj)

outlier_score <- juice_result$.outliers_maha

test_scores(outlier_score)
# Test Passed


na_values_break_fun(step_outliers_maha)
# Test Passed


tidy_result <- tidy(rec_obj, number = 1)

test_that("tidy probs work", {
  expect_equal(nrow(mtcars), nrow(tidy_result))
})


# > Test passed

tidy_rec_obj_not_prep <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_maha(all_numeric(), -all_outcomes()) %>%
  tidy(number = 1)

test_that("tidy probs go to NA", {
  expect_equal(all(is.na(tidy_rec_obj_not_prep$outlier_score)), expected = T)
})


# > Test passed
