# packages.r

library(tidyverse)

'%ni%' <- Negate('%in%')

#'
#'@title read_excel_allsheets
#'@description This function reads all sheets of an excel .xls* file and returns them as a list
#'@import readxl
#'@param filename string; point to the .xls* filepath
#'@param tibble logical; should the function return sheets as tibbles? Default is TRUE
#'@returns a nested list of all excel sheets as tibble objects. 
#'

read_excel_allsheets <- function(filename, tibble = TRUE) {
  if (grepl(".csv", filename)) {
    x <- read.csv(filename, header = TRUE)
  } else {
    require(readxl)
    # I prefer straight data.frames
    # but if you like tidyverse tibbles (the default with read_excel)
    # then just pass tibble = TRUE
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    if (!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
  }
  x
}

