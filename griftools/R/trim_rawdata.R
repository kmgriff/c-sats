# Trim raw data frame
#
# This function accepts a data frame and a list of columns, selects the columns, strips the column names,
# then removes lines with all NA values.

trim_rawdata <- function(dat, columns) {
  dat %>%
    select(columns) %>%
    rename_all(function(x) {
      paste0("V", 1:length(columns))
    }) %>%
    filter_all(any_vars(!is.na(.)))
}
