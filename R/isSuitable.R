#' isSuitable
#'
#' @param y a data structure
#'
#' @return suitable, a boolean- TRUE if the structure is acceptable. Message: error message.
#'
#' @export
#'
#' @examples
#'
#' isThisSuitable <- isSuitable(x)
#'
isSuitable <- function(y){
  if (is.data.frame(y)){
    suitable <- TRUE
  } else if (is.vector(y, mode = "numeric")){
    suitable <- TRUE
  } else if (is.vector(y, mode = "integer")){
    suitable <- TRUE
  } else if (is.matrix(y)){
    suitable <- TRUE
  } else {
    suitable <- FALSE
  }
  if (suitable == TRUE){
    message = NULL
  } else {
    message = "Input is not a dataframe, vector or matrix. \n
    This function won't work. Is it possible you have a list instead?"
  }
  return(c(suitable, message))
  }
