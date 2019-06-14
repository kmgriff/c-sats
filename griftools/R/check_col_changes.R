# Check column changes
#
# This function accepts two data frames and two lists of column names of interest. If the column names
# are different, it is assumed that the user has already defined the lists to satisfaction. Otherwise,
# function checks if the column names from previous data are present in the current data. Warning messages
# are given if the column has changed indices or if the column name does not exist. If the column name
# does not exist, the index is used instead, unless this index is out of bounds.



check_col_changes <- function(prevdat, curdat, prevcols, curcols) {
  if (!is.data.frame(prevdat)) {
    stop("no data frame loaded in 'prevdat'")
  }
  if (!is.data.frame(curdat)) {
    stop("no data frame loaded in 'curdat'")
  }

  # If current column list is defined and different than previous, no diagnostic needed
  if (!identical(prevcols, curcols)) {
    cat("diagnostic skipped: current column list already defined")
    return(curcols)
  }

  previndex <- map_int(prevcols, function(x) str_which(names(prevdat), x))
  curindex <- NULL

  for (i in 1:length(prevcols)) {

    # Detect presence of column
    if (prevcols[i] %in% names(curdat)) {
      curindex <- c(curindex, str_which(names(curdat), prevcols[i]))

      # Detect change in column position
      if (previndex[i] != curindex[i]) {
        warning(str_glue("index of '{curcols[i]}' has changed from {previndex[i]} to {curindex[i]}"))
      }
    } else {
      curindex <- c(curindex, previndex[i])
      if (curindex[i] > length(curdat)) {
        curcols[i] <- NA
        warning(str_glue("'{prevcols[i]}' not found; column index {curindex[i]} out of bounds"))
      } else {
        curcols[i] <- names(curdat)[curindex[i]]
        warning(str_glue("'{prevcols[i]}' not found; coercing new column '{curcols[i]}' found at index {curindex[i]}"))
      }
    }
  }

  if (any(is.na(curcols))) {
    warning("run names(cur_dat) to get new list of column names")
  }

  return(curcols)
}
