#' function used to generate valid choices for `aggregation_function`
#' @param values a few common choices for aggregation
#'
#' @export
aggregation <- function(values = c("mean", "min", "max")) {
  dials::new_qual_param(
    type = "character",
    values = values,
    label = c(aggregation = "Aggregation Method")
  )
}
