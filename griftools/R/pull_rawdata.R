# Pull raw data from file
#
# This function accepts one or more search terms, searches the current directory (and subdirectories)
# for data files matching these terms, and imports a dataset based on its extension. Throws an error
# if 0 or multiple files are detected, or if an unsupported extension is detected.



pull_rawdata <- function(..., recursive = TRUE) {
  terms <- list(...)
  if (length(terms) == 0) {
    stop("search term missing")
  }

  filelist <- dir(recursive = recursive)

  infile <- map(terms, function(pattern) str_subset(filelist, pattern)) %>%
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

  extension <- str_sub(infile, str_locate(infile, "\\.")[2] + 1)
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
