#' Calculate the [h2o package extendedIsolationForest][h2o.extendedIsolationForest] outlier "score"
#'
#' `step_outliers_h2o.extendedIsolationForest` creates a *specification* of a recipe
#'  step that will calculate the outlier score using [h2o.extendedIsolationForest] from `h2o`.
#'
#' @keywords datagen
#' @concept preprocessing
#' @inheritParams recipes::step_center
#' @importFrom h2o as.h2o h2o.init h2o.extendedIsolationForest h2o.predict h2o.shutdown
#' @param role not defined for this function
#' @param outlier_score a placeholder for the exit of this function don't change
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the terms argument.
#' @param name_mutate the name of the generated column with h2o.extendedIsolationForest scores
#' @param options a list with arguments to  [h2o::h2o.extendedIsolationForest] function.
#' @param init_options a list with parameters to [h2o::h2o.init]
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
#' When used in modeling, users should strongly consider using the
#'  option `skip = TRUE` so that this operation is _not_
#'  conducted outside of the training set.
#' @examples
#' library(recipes)
#' library(tidy.outliers)
#' rec_obj <-
#'   recipe(mpg ~ ., data = mtcars) %>%
#'   step_outliers_h2o.extendedIsolationForest(all_numeric(), -all_outcomes()) %>%
#'   prep(mtcars)
#'
#' juice(rec_obj)
#'
#' tidy(rec_obj, number = 1)
step_outliers_h2o.extendedIsolationForest <- function(recipe,
                                  ...,
                                  role = NA,
                                  trained = FALSE,
                                  outlier_score = NULL,
                                  columns = NULL,
                                  name_mutate = ".outliers_h2o.extendedIsolationForest",
                                  options = list(
                                    extension_level = 'max'
                                  ),
                                  init_options = list(),
                                  skip = TRUE,
                                  id = rand_id("outliers_h2o.extendedIsolationForest")) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- ellipse_check(...)

  recipes_pkg_check(required_pkgs.step_outliers_h2o.extendedIsolationForest())

  add_step(
    recipe,
    step_outliers_h2o.extendedIsolationForest_new(
      terms = terms,
      trained = trained,
      role = role,
      outlier_score = outlier_score,
      columns = columns,
      name_mutate = name_mutate,
      options = options,
      init_options = init_options,
      skip = skip,
      id = id
    )
  )
}



step_outliers_h2o.extendedIsolationForest_new <-
  function(terms,
           role,
           trained,
           outlier_score,
           columns,
           name_mutate,
           options,
           init_options,
           skip,
           id) {
    step(
      subclass = "outliers_h2o.extendedIsolationForest",
      terms = terms,
      role = role,
      trained = trained,
      outlier_score = outlier_score,
      columns = columns,
      name_mutate = name_mutate,
      options = options,
      init_options = init_options,
      skip = skip,
      id = id
    )
  }


get_train_score_h2o.extendedIsolationForest <- function(x, args = NULL,init_args = NULL) {

  args$sample_size <- min(args$sample_size,nrow(x))
  args$extension_level <- ifelse(args$extension_level == 'max',
                                  ncol(x) - 1,
                                  as.integer(args$extension_level))

  rlang::exec("h2o.init",!!!init_args)

  col_names <- colnames(x)

  x_h2o <- as.h2o(x)

  # Build an Extended Isolation forest model
  model <- rlang::exec("h2o.extendedIsolationForest", training_frame = x_h2o,
                       x = col_names,
                       !!!args)

  score <- h2o.predict(model, x_h2o)

  anomaly_score <- score$anomaly_score

  res <- anomaly_score |>
    as_tibble() |>
    pull('anomaly_score')
  mean_result <- anomaly_score |> mean()

  h2o.shutdown(prompt = FALSE)
  return(res)
}


#' @export
prep.step_outliers_h2o.extendedIsolationForest <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info = info)
  ## You can add error trapping for non-numeric data here and so on.

  # check_type(training[, col_names])
  #
  # subset_to_check <- training[col_names]
  # nr_na <- colSums(is.na(subset_to_check))
  # if (any(nr_na > 0)) {
  #   with_na <- names(nr_na[nr_na > 0])
  #   with_na_str <- paste(paste0("`", with_na, "`"), collapse = ", ")
  #   rlang::abort(paste0(
  #     "The following columns contain missing values: ",
  #     with_na_str, "."
  #   ))
  # }


  outlier_score <- training[, col_names] %>% get_train_score_h2o.extendedIsolationForest(args = x$options,
                                                                                         init_args = x$init_options)


  ## Use the constructor function to return the updated object.
  ## Note that `trained` is now set to TRUE

  step_outliers_h2o.extendedIsolationForest_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    outlier_score = outlier_score,
    columns = col_names,
    name_mutate = x$name_mutate,
    options = x$options,
    init_options = x$init_options,
    skip = x$skip,
    id = x$id
  )
}


#' @export
bake.step_outliers_h2o.extendedIsolationForest <- function(object, new_data, ...) {
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

#' @rdname step_outliers_h2o.extendedIsolationForest
#' @param x A `step_outliers_h2o.extendedIsolationForest` object.
#' @export
tidy.step_outliers_h2o.extendedIsolationForest <- function(x, ...) {
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


required_pkgs.step_outliers_h2o.extendedIsolationForest <- function(x, ...) {
  c("h2o")
}
