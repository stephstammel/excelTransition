#' makeCharts
#'
#' A function that makes a wad of charts and puts them in your designated
#' working directory. The charts are histograms.
#'
#' @param y
#' @param pName. Optional, defaults to NULL, the path to put the charts in.
#'
#' @return
#' @export
#'
#' @examples
makeCharts <- function(y, pName = NULL){
  if (is.null(pName)){
    currentDateTime <- Sys.time()
    pathname <- currentDateTime
  } else {
    pathname <- pName
  }

  #chartColumns <- which(isSuitable(y)[1] == TRUE) # find all numeric, convert text to factors

  #chartData <- y[,chartColumns]
  chartData <- y
  chartData <- purrr::map(chartData,switchStructure) %>%
    as.data.frame()
  for (i in 1:ncol(chartData)){
    name <- colnames(chartData)[i]
    filename <- paste(pathname, "_", name, sep = "")
    p <- suppressWarnings(makeHistogram(chartData[,i], name, filename))
  }

}
