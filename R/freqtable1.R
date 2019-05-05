#' Gives a frequency table with 1 variable showing absolute'n'relative
#' frequencies
#'
#' @param x data and variable to enter (data$variable)
#' @param useNA either "always" or "never", abbr. supported, default="n"
#'
#' @return a frequency table with first row absolute numbers, second table
#' are relative values
#'
#'
#'
#' @examples
#'  a <- c(1:5)
#'  b <- c(11:15)
#'  sample.data <- data.frame(a,b)
#'  freqtable1(sample.data$a)
#'
#' @export
#'
freqtable1 <- function(x, useNA="n") {

  N <- table(x, useNA = useNA)
  Percentage <- round(100*prop.table(table(x, useNA = useNA)),2)

  c <- rbind(N, Percentage)

  return(c)

}
