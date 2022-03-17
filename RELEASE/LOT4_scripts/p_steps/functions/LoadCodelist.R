#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 31/01/2022


#' Reads in user specified tabs from code list (Annex 2a, in excel format)
#' @param filename path + name of codelist file
#' @param matches list of partial/complete names of the tabs you want to load.
#' @return list of dataframes that match.

load_codelist <- function(filename, matches, tibble = FALSE) {
  if(!require(readxl)){install.packages("readxl")}
  library(readxl)
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.table)
  names(x) <- sheets
  toMatch <- matches
  x <- x[unique(grep(paste(toMatch,collapse="|"), sheets, ignore.case = TRUE))]
  x
}
