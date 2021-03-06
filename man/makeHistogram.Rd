% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeHistogram.R
\name{makeHistogram}
\alias{makeHistogram}
\title{makeHistogram}
\usage{
makeHistogram(x, name, fName)
}
\arguments{
\item{x}{the data to make a histogram on, univariate, single column.
If the data is a factor variable, this is a bar chart instead.}

\item{name, }{chart title name}

\item{fName, }{file name, location for charts}
}
\value{
histChart, a ggplot2 object. Will return NULL if a factor object
has more than 20 levels or if a date/POSIX-family object is entered.
}
\description{
makeHistogram
}
\examples{

p <- makeHistogram(x)


}
