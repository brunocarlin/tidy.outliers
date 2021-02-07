#' Calculate the [Mahalanobis][maha] outlier "probability"
#'
#' `step_outliers_maha` creates a *specification* of a recipe
#'  step that will calculate the probability of the row of selected columns being an outliers using [maha] from `OutlierDetection`.
#'
#' @keywords datagen
#' @concept preprocessing
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which
#'  variables will be transformed. See [selections()] for
#'  more details. For the `tidy` method, these are not
#'  currently used.
#' @param role not defined for this function
#' @param outlier_probability a placeholder for the exit of this function don't change
#' @param name_mutate the name of the generated column with maha probabilities
#' @importFrom OutlierDetection maha
#' @param options a list with cutoff point which is ignored and the rnames argument of the [maha] function
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any), with the name on `name_mutate` and the probabilities calculated. For the
#'  `tidy` method, a tibble with columns `index` (the row indexes of the tibble) and `outlier_probability` (the probabilites).
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
#' library(OutlierDetection)
#'  rec_obj <-
#'  recipe(mpg ~ ., data = mtcars) %>%
#'  step_outliers_maha(all_numeric(),-all_outcomes()) %>%
#'  prep(mtcars)
#'
#'juice(rec_obj)
#'
#'tidy(rec_obj,number = 1)
#'
step_outliers_maha <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  outlier_probability = NULL,
  name_mutate = ".outliers_maha",
  options = list(cutoff = 0,rnames = FALSE),
  skip = TRUE,
  id = rand_id("outliers_maha")
) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- ellipse_check(...)

  recipes_pkg_check(required_pkgs.step_outliers_maha())

  add_step(
    recipe,
    step_outliers_maha_new(
      terms = terms,
      trained = trained,
      role = role,
      outlier_probability = outlier_probability,
      name_mutate = name_mutate,
      options = options,
      skip = skip,
      id = id
    )
  )
}



step_outliers_maha_new <-
  function(terms,
           role,
           trained,
           outlier_probability,
           name_mutate,
           options,
           skip,
           id) {
    step(
      subclass = "outliers_maha",
      terms = terms,
      role = role,
      trained = trained,
      outlier_probability =outlier_probability,
      name_mutate = name_mutate,
      options = options,
      skip = skip,
      id = id
    )
  }


get_train_probability <- function(x, args = NULL) {

  args$cutoff = 0

  res <- rlang::exec('maha',x = x,!!!args)

  res$`Outlier Probability`
}


#' @export
prep.step_outliers_maha <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(terms = x$terms, info = info)
  ## You can add error trapping for non-numeric data here and so on.

  check_type(training[, col_names])


  # ## We'll use the names later so
  # if (x$options$names == FALSE) {
  #   rlang::abort("`names` should be set to TRUE")
  # }

  # if (!any(names(x$options) == "probs")) {
  #   x$options$probs <- (0:100)/100
  # } else {
  #   x$options$probs <- sort(unique(x$options$probs))
  # }


  outlier_probability <- training[, col_names] %>% get_train_probability(args = x$options)


  ## Use the constructor function to return the updated object.
  ## Note that `trained` is now set to TRUE

  step_outliers_maha_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    outlier_probability = outlier_probability,
    name_mutate = x$name_mutate,
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}


#' @export
bake.step_outliers_maha <- function(object, new_data, ...) {

  new_data[[object$name_mutate]] <- object$outlier_probability

  new_data

}


format_prob <- function(step_outlier) {

  x <- step_outlier$outlier_probability
  tibble::tibble(
    index = seq_len(length(x)),
    outlier_probability = x
  )
}

#' @rdname step_outliers_maha
#' @param x A `step_outliers_maha` object.
#' @export
tidy.step_outliers_maha <- function(x, ...) {
  if (is_trained(x)) {
    res <-format_prob(x)
  }
  else {
    res <-
      tibble(
        index = seq_len(length(x)),
        outlier_probability = rlang::na_dbl
      )
  }

  # Always return the step id:
  res$id <- x$id
  res
}


required_pkgs.step_outliers_maha <- function(x, ...) {
  c("OutlierDetection")
}
