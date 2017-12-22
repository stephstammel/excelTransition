#' Make Descriptives
#'
#' Make descriptive statistics in the style of Excel from your data frame.
#'
#' @param y the data to create summary statistics on
#' @param roundDP the number of decimal places to round relevant stats to
#' @param fName, optional parameter directing where to write out an xlsx file
#' with descriptive statistics. Defaults to NULL and file will be written to current
#' working directory
#'
#' @return tidy data frame with descriptive statistics
#' Also writes out a .xlsx file to designated location with the dataframe of
#' descriptive statistics.
#'
#' If no filename or location specified, it will write to a file called
#'  "descriptives date/time.xlsx" in current working directory.
#' @export
#'
#' @examples
#' descriptives <- makeDescriptives(y)
#'
#'


makeDescriptives <- function(y, roundDP = 2, fName = NULL){
  # a test that we have a continuous/double or integer structure as vector OR data frame OR matrix
  x <- isSuitable(y)
  if(x[1] == FALSE){
      cat(x[2])
      stop
  }
  # next test each column of y and check if it's integer, double or factor.
  # if not, convert to factor.

  x <- purrr::map(y,switchStructure) %>%
        as.data.frame()

  descriptives <- as.data.frame(matrix(NA, nrow = ncol(x), ncol = 16))
  for (i in 1:length(x)){
      descriptives[i,] <- getNumericDescriptives(x[,i], names(x)[i])
  }


  descriptives <- as.data.frame(descriptives, stringsAsFactors = FALSE)

  descriptivesNumeric <- as.matrix(descriptives[,4:16])
  descriptivesNonNumeric <- as.matrix(descriptives[,1:3])

  class(descriptivesNumeric) <- "numeric"
  descriptivesNumeric <- as.data.frame(round(descriptivesNumeric,roundDP))

  descriptivesNonNumeric <- as.data.frame(descriptivesNonNumeric)

  descriptives <- cbind(descriptivesNonNumeric, descriptivesNumeric)

  colnames(descriptives) <- c("Variable Name", "Variable Type", "Factor Levels",
                              "Mean", "Standard Error of the Mean","Median",
                              "Mode", "Standard Deviation", "Variance",
                              "Kurtosis", "Skewness", "Minimum", "Maximum",
                              "Sum", "Proportion of NA", "Number of Observations")


  # make them look like excel's version
  descriptives <- t(descriptives) %>% as.data.frame()

  if (is.null(fName)){
    currentDateTime <- Sys.time()
    filename <- paste("descriptives",currentDateTime, ".xlsx", sep = "")
  } else {
    filename <- fName
  }

  interpretation <- getInterpretation()

  hs <- createStyle(textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize=12,
                    fontName="Arial Narrow", fgFill = "#4F80BD")

  l <- list("Descriptive Stats" = descriptives, "Interpretation Notes" = interpretation)

  write.xlsx(l, filename, asTable = FALSE, colNames = FALSE, rowNames = TRUE,
             keepNA = TRUE,
             startCol = 3, startRow = 3, headerStyle = hs,
             borders = "surrounding",
             colWidths = "auto")

  return(descriptives)


}
