#' calculating numbers from RAND/UCLA
#'
#' @param x a vector
#'
#' @return a dataframe
#' @export
#'
#' @examples
RAM_func <- function(x) {


  # functions to include
  median_x <- median(x, na.rm = TRUE)

  SD <- sd(x, na.rm = TRUE)

  Min <- min(x, na.rm = T)

  Max <- max(x, na.rm = T)

  N_below_4 <- sum(x < 4, na.rm = T)
  N_btw_4_6 <- sum(x >= 4 & x <= 6, na.rm = T)
  N_above_6 <- sum(x > 6, na.rm = T)

  quant.3 <- quantile(x, 0.3, na.rm = T)
  quant.7 <- quantile(x, 0.7, na.rm = T)


  IPR <- (quant.7 - quant.3)

  IPRCP <- (quant.7+quant.3)/2

  AI <- 5-IPRCP

  IPRAS <- 2.35+(abs(AI)*1.5)

  DI <- IPR/IPRAS

  Disagreement <- ifelse(abs(IPR) > abs(IPRAS), 1, 0)

  answers <- data.frame(median_x, SD, Min, Max, N_below_4, N_btw_4_6, N_above_6,
                        quant.3, quant.7, IPR, IPRCP, IPRAS, DI, Disagreement)

  # testing for inclusion
  Decision <- ifelse(median_x >= 7 & Disagreement %in% 0, 1,
                     ifelse(median_x <= 3 & Disagreement %in% 0, 0, 9))

  answers$Decision <- Decision

  # defining output
  return(answers)

}


#' iterate the RAM_func over a list
#'
#' @param topic a list to iterate over
#'
#' @return a dataframe
#' @export
#'
#' @examples
RAM_table_func <- function(topic){

  # in development-mode currently
  # Variables <- c("Median", "SD", "Min", "Max", "N_below_4", "N_btw_4_6", "N_above_6", "Quant.3", "Quant.7", "IPR", "IPRCP", "AI", "IPRAS", "DI",
  #                "Disagreement", "Decision")

  a <- purrr::map_dfr(topic, RAM_func, .id = "Variable")


  b <- dplyr::mutate_if(a, is.numeric, round, digits = 2)


  kableExtra::kable(b, "latex", booktabs = T, align = "c") %>%
    kable_styling(latex_options = c("striped", "scale_down")) %>%
    row_spec(0, angle = 45)

}


#' summary of all open ended comments - lines with NAs get dropped
#'
#' @param data a dataframe
#' @param col the column with the variables
#'
#' @return a dataframe
#' @export
#'
#' @examples
RAM_char_func <- function(data, col){

  # depends on tidyverse
  symcol <- enquo(col)
  c <- data %>% select(id, degree, position, expertise, !!symcol) %>% filter(!is.na(!!symcol))

  # # base R
  #   a <- data[ , c("id", "degree", "position", "expertise", col)]
  #
  #   b <- a[["id"]][is.na(a[[col]]) %in% FALSE]
  #
  #   c <- a[a[["id"]] %in% b , ]
  #
  #   c

  kableExtra::kable(c, "latex", longtable = T, booktabs = T) %>%
    kable_styling(full_width = T, latex_options = c("striped", "hold_position")) %>%
    row_spec(0, angle = 45) %>%
    column_spec(1, width = ".5cm") %>%
    column_spec(c(2, 3, 4), width = "1cm")

}
