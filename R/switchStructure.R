

#' switch Structure
#'
#' this function takes a single column input. If it is text-based, switches to factors
#' If it's a lubridate::date function, it will switch it to numeric.
#'
#'
#' @param y the column input. Can be vector or dataframe.
#'
#' @return x the column input with structure switched.
#' @export
#'
#' @examples
switchStructure <- function(y){
  require(lubridate)

  if (is.character(y)){
    x <- as.factor(y)
  } else if(lubridate::is.Date(y)){
    x <- as.numeric(y)
  } else {
    x <- y
  }
  return(x)
}
