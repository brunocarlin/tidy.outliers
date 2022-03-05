#' Calculate the aggregation of a set of outlier columns and filters the resulting tibble
#'
#' `step_outliers_remove` creates a *specification* of a recipe
#'  step that will calculate the probability of the row of selected columns using an aggregation function and filter the resulting tibble based on the filter function
#'
#' @keywords datagen
#' @concept preprocessing
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which
#'  variables will be transformed. See [selections()] for
#'  more details. For the `tidy` method, these are not
#'  currently used.
#' @param role not defined for this function
#' @param aggregation_function a function that returns a value between 0 and 1 on an applied row
#' @param probability_dropout a value between 0 and 1 to decide outliers uses ">=" rule
#' @param col_names name of the columns being operated on, after filtering they will be removed
#' @param outliers_indexes placeholder for the tidy method
#' @param aggregation_results a placeholder for the vector of probabilities
#'
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any), with the name on `name_mutate` and the probabilities calculated. For the
#'  `tidy` method, a tibble with columns `index` (the row indexes of the tibble), `outliers` (the filtered outliers), `aggregation_results` the "probabilities calculated".
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
#'   step_outliers_maha(all_numeric(), -all_outcomes()) %>%
#'   step_outliers_remove(contains(r"(.outliers)")) %>%
#'   prep(mtcars)
#'
#' juice(rec_obj)
#'
#' tidy(rec_obj, number = 2)
step_outliers_remove <- function(
                                 recipe,
                                 ...,
                                 aggregation_function = mean,
                                 probability_dropout = .95,
                                 outliers_indexes = NULL,
                                 aggregation_results = NULL,
                                 col_names = NULL,
                                 role = NA,
                                 trained = FALSE,
                                 skip = TRUE,
                                 id = rand_id("outliers_remove")) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_outliers_remove_new(
      terms = terms,
      trained = trained,
      role = role,
      aggregation_function = aggregation_function,
      probability_dropout = probability_dropout,
      outliers_indexes = outliers_indexes,
      aggregation_results = aggregation_results,
      col_names = col_names,
      skip = skip,
      id = id
    )
  )
}



step_outliers_remove_new <-
  function(terms,
           role,
           trained,
           aggregation_function = aggregation_function,
           probability_dropout = probability_dropout,
           outliers_indexes = outliers_indexes,
           aggregation_results = aggregation_results,
           col_names = col_names,
           skip,
           id) {
    step(
      subclass = "outliers_remove",
      terms = terms,
      role = role,
      trained = trained,
      aggregation_function = aggregation_function,
      probability_dropout = probability_dropout,
      outliers_indexes = outliers_indexes,
      aggregation_results = aggregation_results,
      col_names = col_names,
      skip = skip,
      id = id
    )
  }


get_outliers_combination <- function(x, aggregation_function, probability_dropout) {
  aggregation_results <- apply(x, 1, aggregation_function)


  outliers_indexes <- which(aggregation_results >= probability_dropout)

  list(outliers_indexes = outliers_indexes, aggregation_results = aggregation_results)
}

#' @export
prep.step_outliers_remove <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms,training, info = info)
  ## You can add error trapping for non-numeric data here and so on.

  check_type(training[, col_names])


  outliers_combination <- get_outliers_combination(training[, col_names],
    aggregation_function = x$aggregation_function,
    probability_dropout = x$probability_dropout
  )

  outliers_indexes <- outliers_combination$outliers_indexes

  aggregation_results <- outliers_combination$aggregation_results

  ## Use the constructor function to return the updated object.
  ## Note that `trained` is now set to TRUE

  step_outliers_remove_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    aggregation_function = x$aggregation_function,
    probability_dropout = x$probability_dropout,
    outliers_indexes = outliers_indexes,
    aggregation_results = aggregation_results,
    col_names = col_names,
    skip = x$skip,
    id = x$id
  )
}


#' @export
bake.step_outliers_remove <- function(object, new_data, ...) {
  if (identical(object$outliers_indexes, as.integer())) {
    # check for when nothing was filtered out
    # do nothing
  }

  else {
    new_data <- new_data[-object$outliers_indexes, ]
  }

  ## Remove all extra columns

  new_data <- new_data[, !names(new_data) %in% object$col_names, drop = F]

  ## Always convert to tibbles on the way out

  tibble::as_tibble(new_data)
}


format_remove <- function(step_outlier) {
  outliers_indexes <- step_outlier$outliers_indexes

  aggregation_results <- step_outlier$aggregation_results



  original_length <- length(aggregation_results)

  index <- seq_len(original_length)


  outliers <- index %in% outliers_indexes

  tibble::tibble(
    index = index,
    outliers = outliers,
    aggregation_results = aggregation_results
  )
}

#' @rdname step_outliers_remove
#' @param x A `step_outliers_remove` object.
#' @export
tidy.step_outliers_remove <- function(x, ...) {
  if (is_trained(x)) {
    res <- format_remove(x)
  }
  else {
    res <-
      tibble(
        index = seq_len(length(x)),
        outliers = FALSE,
        aggregation_results = 0
      )
  }
}

#' function used to generate valid choices for `aggregation_function`
#' @param values a few common choices for aggregation
#'
#' @export
aggregation <- function(values = c( "mean","min", "max")) {
  dials::new_qual_param(
    type = "character",
    values = values,
    # By default, the first value is selected as default. We'll specify that to
    # make it clear.
    default = "mean",
    label = c(aggregation = "Aggregation Method")
  )
}


#' @export
tunable.step_outliers_remove <- function(x, ...) {
  probability_dropout <- tibble::tibble(
    name = c("probability_dropout"),
    call_info = list(list(pkg = "dials", fun = "dropout")),
    source = "recipe",
    component = "step_outliers_remove",
    component_id = x$id
  )
  aggregation_function <- tibble::tibble(
    name = c("aggregation_function"),
    call_info = list(list(pkg = "tidy.outliers", fun = "aggregation")),
    source = "recipe",
    component = "step_outliers_remove",
    component_id = x$id
  )
  dplyr::bind_rows(probability_dropout,aggregation_function)
}
