#' Add counts of multiple columns at once
#'
#' @param df the dataframe to work on
#' @param ... one or multiple column names to summarise by n() on
#'
#' @return a dataframe
#' @export
#'
#' @import rlang
#'
#' @references Code originally written in R4DS-Channel by
#' https://github.com/HannesOberreiter without testing for edge cases,
#' modified for personal use by me
#'
#' @examples
#' mtcars |> add_counts(cyl, vs, am)
add_counts <- function(df, ...){
  .dots <- rlang::enquos(...)
  # split attributes from our grouping cols
  is_named <- base::names(.dots) != ""
  .groups <- .dots[!is_named]
  .attrs <- .dots[is_named]
  # extract counts and return df
  res <- purrr::map(.groups, ~ df %>% dplyr::add_count(!!.x, !!!.attrs) %>% dplyr::pull(n))
  base::names(res) <- base::paste0("n_", purrr::map_chr(.groups, ~ rlang::as_name(.x)))
  dplyr::bind_cols(df, res)
}

