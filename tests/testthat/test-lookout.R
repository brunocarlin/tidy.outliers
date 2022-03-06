# libraries -------------------------------------------------------------------------------------------------------

library(recipes)
library(tidy.outliers)
library(tune)


# setup -----------------------------------------------------------------------------------------------------------

rec_obj <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_lookout(all_numeric(), -all_outcomes()) %>%
  prep(mtcars)


juice_result <- juice(rec_obj)

outlier_score <- juice_result$.outliers_lookout


# usual cases -----------------------------------------------------------------------------------------------------

test_scores(outlier_score)
# Test passed <U+0001F638>


na_values_break_fun(step_outliers_lookout)
# Test passed <U+0001F600>


# tidy method -----------------------------------------------------------------------------------------------------

tidy_result <- tidy(rec_obj, number = 1)


test_that("tidy probs work", {
  expect_equal(nrow(mtcars), nrow(tidy_result))
})
# Test passed <U+0001F600>


# recipe without prep cases ---------------------------------------------------------------------------------------

tidy_rec_obj_not_prep <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_lookout(all_numeric(), -all_outcomes()) %>%
  tidy(number = 1)

test_that("tidy probs go to NA", {
  expect_equal(all(is.na(tidy_rec_obj_not_prep$outlier_score)), expected = T)
})
# Test passed
