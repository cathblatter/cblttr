#' Convert a tableone matrix to a dataframe
#'
#' @param mat An object created through tableone()
#'
#' @return a dataframe
#' @export
#'
#' @references developed & published in a gist from Kazuki Yoshida
#' https://gist.github.com/kaz-yos/7b4e97b633d61fa4d1d27f46f2534ae3
tableone_mat_to_data_frame <- function(mat) {
  dplyr::bind_cols(tibble::tibble(Variable = base::rownames(mat)),
                   tibble::as_tibble(mat))
  }
