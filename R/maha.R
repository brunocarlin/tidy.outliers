#' Calculate the Mahalanobis outlier "score"
#'
#' `step_outliers_maha` creates a *specification* of a recipe
#'  step that will calculate the outlier score using the
#'  Chisquare distribution [stats::pchisq()] of the Mahalanobis [stats::mahalanobis()] distances.
#'
#' @keywords datagen
#' @concept preprocessing
#' @importFrom stats cov mahalanobis pchisq
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which
#'  variables will be transformed. See [selections()] for
#'  more details. For the `tidy` method, these are not
#'  currently used.
#' @param role not defined for this function
#' @param outlier_score a placeholder for the exit of this function don't change
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the terms argument.
#' @param name_mutate the name of the generated column with Mahalanobis scores
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any), with the name on `name_mutate` and the scores calculated. For the
#'  `tidy` method, a tibble with columns `index` (the row indexes of the tibble) and `outlier_score` (the scores).
#'
#' @export
#'
#' @details
#' All columns in the data are sampled and returned by [juice()]
#'  and [bake()].
#'
#' All columns used in this step must be numeric with no missing data.
#'
#' When used in modeling, users should strongly consider using the
#'  option `skip = TRUE` so that this operation is _not_
#'  conducted outside of the training set.
#' @examples
#' library(recipes)
#' library(tidy.outliers)
#' rec <-
#'   recipe(mpg ~ ., data = mtcars) %>%
#'   step_outliers_maha(all_numeric_predictors()) %>%
#'   prep(mtcars)
#'
#' bake(rec, new_data = NULL)
#'
#' tidy(rec, number = 1)
step_outliers_maha <- function(recipe,
                               ...,
                               role = NA,
                               trained = FALSE,
                               outlier_score = NULL,
                               columns = NULL,
                               name_mutate = ".outliers_maha",
                               skip = TRUE,
                               id = rand_id("outliers_maha")) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_outliers_maha_new(
      terms = terms,
      trained = trained,
      role = role,
      outlier_score = outlier_score,
      columns = columns,
      name_mutate = name_mutate,
      skip = skip,
      id = id
    )
  )
}



step_outliers_maha_new <-
  function(terms,
           role,
           trained,
           outlier_score,
           columns,
           name_mutate,
           skip,
           id) {
    step(
      subclass = "outliers_maha",
      terms = terms,
      role = role,
      trained = trained,
      outlier_score = outlier_score,
      columns = columns,
      name_mutate = name_mutate,
      skip = skip,
      id = id
    )
  }


get_train_score_maha <- function(x, args = NULL) {
  m <- mahalanobis(x, colMeans(x), cov(x))
  p <- pchisq(m, ncol(x))

  return(p)
}


#' @export
prep.step_outliers_maha <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  ## You can add error trapping for non-numeric data here and so on.

  check_type(training[, col_names])



  subset_to_check <- training[col_names]
  nr_na <- colSums(is.na(subset_to_check))
  if (any(nr_na > 0)) {
    with_na <- names(nr_na[nr_na > 0])
    with_na_str <- paste(paste0("`", with_na, "`"), collapse = ", ")
    rlang::abort(paste0(
      "The following columns contain missing values: ",
      with_na_str, "."
    ))
  }


  outlier_score <- training[, col_names] %>% get_train_score_maha()


  step_outliers_maha_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outlier_score = outlier_score,
    columns = col_names,
    name_mutate = x$name_mutate,
    skip = x$skip,
    id = x$id
  )
}


#' @export
bake.step_outliers_maha <- function(object, new_data, ...) {
  new_data[[object$name_mutate]] <- object$outlier_score

  new_data
}


format_prob <- function(step_outlier) {
  x <- step_outlier$outlier_score
  tibble::tibble(
    index = seq_len(length(x)),
    outlier_score = x
  )
}

#' @rdname step_outliers_maha
#' @param x A `step_outliers_maha` object.
#' @export
tidy.step_outliers_maha <- function(x, ...) {
  if (is_trained(x)) {
    res <- format_prob(x)
  } else {
    res <-
      tibble(
        index = seq_len(length(x)),
        outlier_score = rlang::na_dbl
      )
  }

  # Always return the step id:
  res$id <- x$id
  res
}
