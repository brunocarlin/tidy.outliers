library(recipes)
library(tidy.outliers)
library(OutlierDetection)

rec_obj <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_lookout(all_numeric(),-all_outcomes()) %>%
  prep(mtcars)


juice_result <- juice(rec_obj)

outlier_probability <- juice_result$.outliers_lookout

test_that("probabilities make sense", {
  expect_gte(min(outlier_probability),0)
  expect_lte(min(outlier_probability),1)
  expect_equal(any(is.na(outlier_probability)),expected = F)
})

#> Test passed 😸


test_that("na values create an error", {
  expect_error(recipe(mpg ~ ., data = mtcars2) %>%
                 step_outliers_maha(all_numeric(),-all_outcomes()) %>%
                 prep(mtcars2))
})


#> Test passed 😀


tidy_result <- tidy(rec_obj,number = 1)


test_that("tidy probs work", {
  expect_equal(nrow(mtcars),nrow(tidy_result))
})
#> Test passed 😀


tidy_rec_obj_not_prep <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_outliers_lookout(all_numeric(),-all_outcomes()) %>%
  tidy(number = 1)

test_that("tidy probs go to NA", {
  expect_equal(all(is.na(tidy_rec_obj_not_prep$outlier_probability)),expected = T)
})