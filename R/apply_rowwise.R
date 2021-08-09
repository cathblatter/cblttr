
#' Apply a function rowwise (usually per observation)
#'
#' @param data a dataframe
#' @param vars a selection of variables from the dataframe to apply a function over
#' @param na.rm TRUE or FALSE, default = TRUE
#' @param func the function to apply
#'
#' @export
#'
apply_rowwise <- function(data, vars, func, na.rm = TRUE){

  apply(data[ , vars], 1, func, na.rm = na.rm)

}
