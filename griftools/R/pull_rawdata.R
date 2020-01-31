# Pull raw data from file
#
# This function accepts one or more search terms, searches the current directory (and subdirectories)
# for data files matching these terms, and imports a dataset based on its extension. Throws an error
# if 0 or multiple files are detected, or if an unsupported extension is detected.

require(tidyverse)
require(readxl)
require(haven)

pull_rawdata <- function(..., recursive = TRUE) {
  terms <- list(...) %>% unlist()
  if (length(terms) == 0) {
    stop("search term missing")
  }

  filelist <- dir(recursive = recursive)

  if (any(str_detect(filelist, "~"))) {
    warning("filenames containing \"~\" excluded")
    filelist <- filelist[grep(filelist, pattern = "~", invert = TRUE)]
  }
  infile <- map(terms, ~ str_subset(filelist, .x)) %>%
    reduce(intersect)

  if (length(infile) == 0) {
    stop("no file found matching search terms \'", paste0(terms, collapse = "\', \'"), "\'")
  }
  if (length(infile) > 1) {
    stop(
      "multiple files found matching search terms \'", paste0(terms, collapse = "\', \'"), "\': ",
      paste0(infile, collapse = "\t")
    )
  }

  pos <- str_locate_all(infile, "\\.")[[1]] %>% .[length(.)]
  extension <- str_sub(infile, pos + 1)
  if (extension == "csv") {
    return(read_csv(infile))
  }
  if (extension == "sav") {
    return(read_sav(infile))
  }
  if (extension == "xlsx" | extension == "xls") {
    return(read_excel(infile))
  }
  stop("unsupported file extension: ", extension)
}
