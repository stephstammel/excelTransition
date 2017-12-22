

#' getNumericDescriptives
#'
#' A function which pulls descriptive statistics for a single numeric input column
#' can be double or integer. Date types switched out to numeric, caution should be applied.
#'
#'
#' @param y input structure
#' @param name name of structure to ID it in future
#'
#' @return descriptives, a vector of outputs
#' @export
#'
#' @examples
#'
#' stats <- getNumericDescriptives(myData, myName, myRounding)
#'
#'
getNumericDescriptives <- function(y, name){
  require(moments)
  require(lubridate)

  outputClosure <- function(item, ...){
    function(y){
        item(y,...)
    }
  }

  descriptives <- matrix(NA, nrow = 1, ncol = 16)
  descriptives[,1] <- name
  if (is.factor(y)){
    descriptives[,2] <- "factor variable"
    descriptives[,3] <- length(levels(y))
  } else if (is.POSIXt(y) | is.POSIXlt(y) | is.POSIXct(y) | is.Date(y)) {
    descriptives[,2] <- "date variable"
    descriptives[,3] <- "no levels"
  } else {
    descriptives[,2] <- "numeric variable"
    descriptives[,3] <- "no levels"
  }

  getmode <- function(v) { # function from: https://www.tutorialspoint.com/r/r_mean_median_mode.htm
                           # accessed on 19/12/17
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  mean_of_x <- outputClosure(mean, na.rm = TRUE)

  median_of_x <- outputClosure(median, na.rm = TRUE)
  mode_of_x <- outputClosure(getmode)
  sd_of_x <- outputClosure(sd, na.rm = TRUE)
  kurt_of_x <- outputClosure(kurtosis, na.rm = TRUE)
  skew_of_x <- outputClosure(skewness, na.rm = TRUE)
  min_of_x <- outputClosure(min, na.rm = TRUE)
  max_of_x <- outputClosure(max, na.rm = TRUE)
  sum_of_x <- outputClosure(sum, na.rm = TRUE)
  length_of_x <- outputClosure(length)

  if (is.numeric(y)){
    descriptives[,4] <- mean_of_x(y)
    descriptives[,5] <- sd(y, na.rm = TRUE)/(sqrt(length(y)))
    descriptives[,6] <- median_of_x(y)
    descriptives[,7] <- mode_of_x(y)
    descriptives[,8] <- sd_of_x(y)
    descriptives[,9] <- as.numeric(descriptives[,8])^2
    descriptives[,10] <- kurt_of_x(y)
    descriptives[,11] <- skew_of_x(y)
    descriptives[,12] <- min_of_x(y)
    descriptives[,13] <- max_of_x(y)
    descriptives[,14] <- sum_of_x(y)
    descriptives[,15] <- sum(is.na(y))/length(y)
    descriptives[,16] <- length_of_x(y)
  } else {
    descriptives[,4] <- NA
    descriptives[,5] <- NA
    descriptives[,6] <- NA
    descriptives[,7] <- mode_of_x(y)
    descriptives[,8] <- NA
    descriptives[,9] <- NA
    descriptives[,10] <- NA
    descriptives[,11] <- NA
    descriptives[,12] <- NA
    descriptives[,13] <- NA
    descriptives[,14] <- NA
    descriptives[,15] <- sum(is.na(y))/length(y)
    descriptives[,16] <- length_of_x(y)
  }
  return(descriptives)
}

