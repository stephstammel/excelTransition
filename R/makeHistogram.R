#' makeHistogram
#'
#' @param x the data to make a histogram on, univariate, single column.
#' If the data is a factor variable, this is a bar chart instead.
#' @param name, chart title name
#' @param fName, file name, location for charts
#'
#' @return histChart, a ggplot2 object. Will return NULL if a factor object
#' has more than 20 levels or if a date/POSIX-family object is entered.
#' @export
#'
#' @examples
#'
#' p <- makeHistogram(x)
#'
#'
makeHistogram <- function(x, name, fName){


  if ((is.POSIXt(x) | is.POSIXlt(x) | is.POSIXct(x) | is.Date(x))){
    type = "date-like"
  } else if (is.factor(x)){
    type = "factor"
  } else if (is.numeric(as.matrix(x))){
    type = "numeric"
  } else {
    type = "other"
  }

  x <- as.data.frame(x)


  if (type == "numeric"){
    histChart <- ggplot(x, aes(x)) +
             geom_histogram()+
             theme_light()+
             ylab("Frequency")+
             xlab("")+
             ggtitle(name)
  } else if (type == "factor" & length(levels(x)) <= 20){
    histChart <- ggplot(x, aes(x))+
             geom_bar(stat = "count")+
             theme_light()+
             ylab("Frequency")+
             xlab("")+
             ggtitle(name)
  } else {
    histChart <- NULL
  }


  if (!is.null(histChart)){
    ggsave(fName, plot = histChart, device = "jpeg")
    chartSaved <- TRUE
  } else {
    chartSaved <- FALSE
  }


  return (chartSaved)
}
