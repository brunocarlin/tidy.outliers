#' Calculate the [outForest package][outForest] outlier "score"
#'
#' `step_outliers_outForest` creates a *specification* of a recipe
#'  step that will calculate the outlier score using [outForest] from `outForest`, it internally handles missing data.
#'
#' @keywords datagen
#' @concept preprocessing
#' @importFrom purrr map_lgl
#' @inheritParams recipes::step_center
#' @import dplyr
#' @param role not defined for this function
#' @param outlier_score a placeholder for the exit of this function don't change
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the terms argument.
#' @param name_mutate the name of the generated column with outForest results
#' @importFrom outForest outForest
#' @param options a list with arguments to [outForest] function.
#' @param outlier_score_function a function to decide when there are multivariate outlier scores how to combine them, some examples would be sum or median
#' @param original_result an argument to return a tibble row with the original results of the function instead of an computed score
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any), with the name on `name_mutate` and the probabilities calculated. For the
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
#'   step_outliers_outForest(all_numeric_predictors()) %>%
#'   prep(mtcars)
#'
#' bake(rec, new_data = NULL)
#'
#' tidy(rec, number = 1)
step_outliers_outForest <- function(recipe,
                                 ...,
                                 role = NA,
                                 trained = FALSE,
                                 outlier_score = NULL,
                                 columns = NULL,
                                 name_mutate = ".outliers_outForest",
                                 options = list(
                                   formula = . ~ .,
                                   replace = c("pmm", "predictions", "NA", "no"),
                                   pmm.k = 3,
                                   threshold = 3,
                                   max_n_outliers = Inf,
                                   max_prop_outliers = 1,
                                   min.node.size = 40,
                                   allow_predictions = FALSE,
                                   impute_multivariate = TRUE,
                                   impute_multivariate_control = list(pmm.k = 3, num.trees = 50, maxiter = 3L),
                                   seed = NULL,
                                   verbose = 0
                                 ),
                                 outlier_score_function = mean,
                                 original_result = FALSE,
                                 skip = TRUE,
                                 id = rand_id("outliers_outForest")) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- ellipse_check(...)

  recipes_pkg_check(required_pkgs.step_outliers_outForest())

  add_step(
    recipe,
    step_outliers_outForest_new(
      terms = terms,
      trained = trained,
      role = role,
      outlier_score = outlier_score,
      columns = columns,
      name_mutate = name_mutate,
      options = options,
      outlier_score_function = outlier_score_function,
      original_result = original_result,
      skip = skip,
      id = id
    )
  )
}



step_outliers_outForest_new <-
  function(terms,
           role,
           trained,
           outlier_score,
           columns,
           name_mutate,
           options,
           outlier_score_function,
           original_result,
           skip,
           id) {
    step(
      subclass = "outliers_outForest",
      terms = terms,
      role = role,
      trained = trained,
      outlier_score = outlier_score,
      columns = columns,
      name_mutate = name_mutate,
      options = options,
      outlier_score_function = outlier_score_function,
      original_result = original_result,
      skip = skip,
      id = id
    )
  }

#' Gets the score outForest
#'
#'
#' @param x the data.
#' @param args args to pass to outForest
#' @param original_result whether to return tibbles
#' @param outlier_score_function the function to summarize scores across rows
#'
#' @noRd
#' @keywords internal
get_train_score_outForest <- function(x, args, original_result, outlier_score_function) {
  out <- rlang::exec("outForest", data = x, !!!args)

  data_outliers <- out$outliers |>
    as_tibble()

  if (original_result) {
    nest_outlier <- data_outliers |>
      nest_by(row)

    res <- x |>
      mutate(
        row = row_number(),
        col = NA_character_,
        observed = NA_real_,
        predicted = NA_real_,
        rmse = NA_real_,
        score = NA_real_,
        threshold = NA_real_,
        replacement = NA_real_,
        .keep = "none"
      ) |>
      nest_by(row) |>
      ungroup() |>
      left_join(nest_outlier, by = "row") |>
      mutate(score = if_else(map_lgl(.data$data.y, is.null), .data$data.x, .data$data.y), .keep = "none")

    return(res)
  }


  summarise_outlier <- data_outliers |>
    group_by(row) |>
    summarise(outlier_score = abs(.data$score) |> outlier_score_function(), .groups = "drop")

  res <- x |>
    mutate(
      row = row_number(),
      not_outlier_score = 0,
      .keep = "none"
    ) |>
    left_join(summarise_outlier, by = "row") |>
    mutate(score = coalesce(.data$outlier_score, .data$not_outlier_score)) |>
    summarise(score = percent_rank(.data$score))

  return(res)
}


#' @export
prep.step_outliers_outForest <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info = info)
  ## You can add error trapping for non-numeric data here and so on.

  check_type(training[, col_names])

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

  outlier_score <- training[, col_names] %>% get_train_score_outForest(
    args = x$options,
    original_result = x$original_result,
    outlier_score_function = x$outlier_score_function
  )


  ## Use the constructor function to return the updated object.
  ## Note that `trained` is now set to TRUE

  step_outliers_outForest_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    outlier_score = outlier_score,
    columns = col_names,
    name_mutate = x$name_mutate,
    options = x$options,
    outlier_score_function = x$outlier_score_function,
    original_result = x$original_result,
    skip = x$skip,
    id = x$id
  )
}


#' @export
bake.step_outliers_outForest <- function(object, new_data, ...) {
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

#' @rdname step_outliers_outForest
#' @param x A `step_outliers_outForest` object.
#' @export
tidy.step_outliers_outForest <- function(x, ...) {
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


#' @export
tunable.step_outliers_outForest <- function(x, ...) {
  outlier_score_function <- tibble::tibble(
    name = c("outlier_score_function"),
    call_info = list(list(pkg = "tidy.outliers", fun = "aggregation")),
    source = "recipe",
    component = "step_outliers_outForest",
    component_id = x$id
  )
  dplyr::bind_rows(outlier_score_function)
}

required_pkgs.step_outliers_outForest <- function(x, ...) {
  c("outForest")
}
