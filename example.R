library(readxl) # readxl is the package which allows you to load Excel files in R
library(ggplot2) # everybody likes charts, this is a great package to work with
library(openxlsx) # a package for writing excel files

library(excelTransition) # this is the package where the excelTransition functions live


cropYields <- readRDS("data/cropyields.RDS")


 myResults <- makeDescriptives(cropYields) # get the basic descriptives similar to Excel.
                                                # comes with a sheet offering some notes on interpretation

makeCharts(cropYields)

