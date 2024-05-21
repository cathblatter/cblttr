# other stuff
'%ni%' <- Negate('%in%')


#' Insert '%in%'
#'
#' @return '%in%' as text
#' @export
#'
insertInAddin <- function() {
  rstudioapi::insertText(" %in% ")
}
