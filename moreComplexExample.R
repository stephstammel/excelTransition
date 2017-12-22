library(readxl) # readxl is the package which allows you to load Excel files in R
library(ggplot2) # everybody likes charts, this is a great package to work with
library(openxlsx) # a package for writing excel files

library(excelTransition) # this is the package where the excelTransition functions live


loanBook <- read_xlsx("data/20170930loanbook.xlsx", sheet = "RSLoanBook",
                      col_names = TRUE, skip = 8)
            # Here, I'm loading the spreadsheet


# You generally want to choose where your output goes. Suggest making an
# output folder in your current directory. After that you can direct output
# there like this.

outputName <- "output/descriptives.xlsx"

# You'll also want to choose how many decimal places to round to:

roundDP <- 3

myResults <- makeDescriptives(loanBook, roundDP, outputName) # get the basic descriptives similar to Excel.
                                # comes with a sheet offering some notes on interpretation

pathName <- "output/"

makeCharts(loanBook, pathName)

