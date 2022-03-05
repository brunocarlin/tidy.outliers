#' Calculate the [lookout package][lookout] outlier "score"
#'
#' `step_outliers_lookout` creates a *specification* of a recipe
#'  step that will calculate the outlier score using [lookout] from `lookout` and inverting the results.
#'
#' @keywords datagen
#' @concept preprocessing
#' @inheritParams recipes::step_center
#' @param role not defined for this function
#' @param outlier_score a placeholder for the exit of this function don't change
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the terms argument.
#' @param name_mutate the name of the generated column with lookout probabilities
#' @importFrom lookout lookout
#' @param options a list with alpha, unitize which decides normalization, bw and gdp [lookout] function.
#' @param invert_results controler to transform the original [lookout] function result to an outlier score.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any), with the name on `name_mutate` and the probabilities calculated. For the
#'  `tidy` method, a tibble with columns `index` (the row indexes of the tibble) and `outlier_score` (the probabilites).
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
#'   step_outliers_lookout(all_numeric(), -all_outcomes()) %>%
#'   prep(mtcars)
#'
#' juice(rec_obj)
#'
#' tidy(rec_obj, number = 1)
step_outliers_lookout <- function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    outlier_score = NULL,
    columns = NULL,
    name_mutate = ".outliers_lookout",
    options = list(alpha = 0.05, unitize = TRUE, bw = NULL, gpd = NULL),
    invert_results = FALSE,
    skip = TRUE,
    id = rand_id("outliers_lookout")) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- ellipse_check(...)

  recipes_pkg_check(required_pkgs.step_outliers_lookout())

  add_step(
    recipe,
    step_outliers_lookout_new(
      terms = terms,
      trained = trained,
      role = role,
      outlier_score = outlier_score,
      columns = columns,
      name_mutate = name_mutate,
      options = options,
      invert_results = invert_results,
      skip = skip,
      id = id
    )
  )
}



step_outliers_lookout_new <-
  function(terms,
           role,
           trained,
           outlier_score,
           columns,
           name_mutate,
           options,
           invert_results,
           skip,
           id) {
    step(
      subclass = "outliers_lookout",
      terms = terms,
      role = role,
      trained = trained,
      outlier_score = outlier_score,
      columns = columns,
      name_mutate = name_mutate,
      options = options,
      invert_results = invert_results,
      skip = skip,
      id = id
    )
  }


get_train_score_lookout <- function(x, args = NULL,invert_results = TRUE) {
  res <- rlang::exec("lookout", X = x, !!!args)$`outlier_score`

  if (invert_results) {
   res <- (res - 1) * -1
  }

  return(res)
}


#' @export
prep.step_outliers_lookout <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms,training, info = info)
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


  outlier_score <- training[, col_names] %>% get_train_score_lookout(args = x$options,invert_results = x$invert_results)


  ## Use the constructor function to return the updated object.
  ## Note that `trained` is now set to TRUE

  step_outliers_lookout_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    outlier_score = outlier_score,
    columns = col_names,
    name_mutate = x$name_mutate,
    options = x$options,
    invert_results = x$invert_results,
    skip = x$skip,
    id = x$id
  )
}


#' @export
bake.step_outliers_lookout <- function(object, new_data, ...) {

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

#' @rdname step_outliers_lookout
#' @param x A `step_outliers_lookout` object.
#' @export
tidy.step_outliers_lookout <- function(x, ...) {
  if (is_trained(x)) {
    res <- format_prob(x)
  }
  else {
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


required_pkgs.step_outliers_lookout <- function(x, ...) {
  c("lookout")
}
