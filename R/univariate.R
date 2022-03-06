#' Calculate the univariatelanobis outlier "score"
#'
#' `step_outliers_univariate` creates a *specification* of a recipe
#'  step that will calculate the outlier score using the user supplied outlier_function on a univariate basis
#'  the combination is of multiple variables is handled  by the combination_function
#'
#' @keywords datagen
#' @concept preprocessing
#' @importFrom tidyr pivot_longer
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which
#'  variables will be transformed. See [selections()] for
#'  more details. For the `tidy` method, these are not
#'  currently used.
#' @param role not defined for this function
#' @param outlier_score a placeholder for the exit of this function don't change
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the terms argument.
#' @param name_mutate the name of the generated column with univariate scores
#' @param outlier_function the function used on a column basis to calculate the outlier score.
#' @param combination_function the function used on a row basis to combine diffent outlier function results.
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
#' rec_obj <-
#'   recipe(mpg ~ ., data = mtcars) %>%
#'   step_outliers_univariate(all_numeric(), -all_outcomes()) %>%
#'   prep(mtcars)
#'
#' juice(rec_obj)
#'
#' tidy(rec_obj, number = 1)
step_outliers_univariate <- function(recipe,
                               ...,
                               role = NA,
                               trained = FALSE,
                               outlier_score = NULL,
                               columns = NULL,
                               name_mutate = ".outliers_univariate",
                               outlier_function = \(x){
                                 (percent_rank(x) - 0.5) |>
                                   abs() * 2
                               },
                               combination_function = mean,
                               skip = TRUE,
                               id = rand_id("outliers_univariate")) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_outliers_univariate_new(
      terms = terms,
      trained = trained,
      role = role,
      outlier_score = outlier_score,
      columns = columns,
      name_mutate = name_mutate,
      outlier_function = outlier_function,
      combination_function = combination_function,
      skip = skip,
      id = id
    )
  )
}



step_outliers_univariate_new <-
  function(terms,
           role,
           trained,
           outlier_score,
           columns,
           name_mutate,
           outlier_function,
           combination_function,
           skip,
           id) {
    step(
      subclass = "outliers_univariate",
      terms = terms,
      role = role,
      trained = trained,
      outlier_score = outlier_score,
      columns = columns,
      name_mutate = name_mutate,
      outlier_function = outlier_function,
      combination_function = combination_function,
      skip = skip,
      id = id
    )
  }


get_train_score_univariate <- function(x,outlier_function,combination_function) {

  res <- x |>
    mutate(across(.cols = everything(),.fns = outlier_function)) |>
    mutate(row = row_number()) |>
    pivot_longer(-row) |>
    group_by(row) |>
    summarise(score = .data$value |> combination_function()) |>
    pull(.data$score)

  return(res)
}


#' @export
prep.step_outliers_univariate <- function(x, training, info = NULL, ...) {
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


  outlier_score <- training[, col_names] %>% get_train_score_univariate(outlier_function = x$outlier_function,
                                                                        combination_function = x$combination_function)


  step_outliers_univariate_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outlier_score = outlier_score,
    columns = col_names,
    name_mutate = x$name_mutate,
    outlier_function = x$outlier_function,
    combination_function = x$combination_function,
    skip = x$skip,
    id = x$id
  )
}


#' @export
bake.step_outliers_univariate <- function(object, new_data, ...) {
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

#' @rdname step_outliers_univariate
#' @param x A `step_outliers_univariate` object.
#' @export
tidy.step_outliers_univariate <- function(x, ...) {
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
