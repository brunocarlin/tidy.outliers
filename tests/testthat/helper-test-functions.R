test_probabilities <- function(outlier_probability) {
  test_that("probabilities make sense", {
    expect_gte(min(outlier_probability), 0)
    expect_lte(min(outlier_probability), 1)
    expect_false(any(is.na(outlier_probability)))
  })
}


na_values_break_fun <- function(recipe_step, ...) {
  mtcars2 <- mtcars
  mtcars2[2, 2] <- NA

  test_that("na values create an error", {
    expect_error(
      recipe(mpg ~ ., data = mtcars2) %>%
        recipe_step(all_numeric(), -all_outcomes()) %>%
        prep(mtcars2)
    )
  })
}
