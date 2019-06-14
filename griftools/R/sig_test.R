# Significance testing function
#
# This function accepts two 2 X N data frames, with the first row containing successes
# and the second row containing sample sizes for each variable. The first variable of
# each data frame must be the total sample size and named "N".
#
# Significance testing uses a Chi-square test without correction, unless argument "type"
# is set to "fisher", in which case Fisher's exact test will be used.
#
# The output is a data frame with the variable names as "rowname", data frame 1's
# proportion as "Previous", data frame 2's proportion as "Current", the difference as
# "Difference", and the p-value of the significance test as "p". "Difference" is blank if
# p >= 0.05.
#
# If only current data is passed, the output is simply a data frame with the variable
# names as "rowname" and data frame 2's proportion as "Current". If no valid data is
# passed, the output is a data frame with "N/A" for "rowname" and "Current". This can
# be forced by passing NA as arguments instead of data frames.



sig_test <- function(dat1, dat2, type = "chisq") {
  if (!type %in% c("chisq", "fisher")) {
    stop("'type' must be either \"chisq\" (default) or \"fisher\".")
  }

  check_df <- function(dat) {
    if (!is.data.frame(dat) | length(dat) < 2 | length(dat[[1]]) != 2) {
      stop("argument 1 and 2 must be data frames with 2 rows and at least 2 columns, or NA")
    }
    if (names(dat)[1] != "N") {
      stop("first column of data frame must be named \"N\"")
    }
  }

  # Return N/A dataframe for export if no current data
  if (!is.data.frame(dat2)) {
    if (is.na(dat2)) {
      return(data.frame(rowname = "N/A", current = "N/A"))
    }
  }

  # Check if current data frame is valid
  check_df(dat2)

  return_current <- function(dat) {
    pulse <- dat %>%

      # Transpose columns into rows
      t() %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      select(rowname, "x" = 2, "n" = 3) %>%
      mutate("Current" = x / n) %>%
      select(rowname, Current)

    return(pulse)
  }

  # Return only current dataframe for export if no previous data available
  if (identical(dat1, dat2)) {
    return(return_current(dat2))
  }
  if (!is.data.frame(dat1)) {
    if (is.na(dat1)) {
      return(return_current(dat2))
    }
  }

  # Check if previous data frame is valid
  check_df(dat1)



  pulse <- bind_rows(dat1, dat2) %>%

    # Transpose columns into rows
    t() %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename("x1" = 2, "n1" = 3, "x2" = 4, "n2" = 5) %>%

    # Remove "N" row
    slice(-1)

  # Significance test
  if (type == "chisq") {
    pulse <- pulse %>%
      rowwise() %>%
      mutate("p" = ifelse((n1 >= x1) & (n2 >= x2),
        prop.test(matrix(c(x1, x2, n1 - x1, n2 - x2), nc = 2), correct = FALSE)$p.value,
        NA
      ))
  } else if (type == "fisher") {
    pulse <- pulse %>%
      rowwise() %>%
      mutate("p" = ifelse((n1 >= x1) & (n2 >= x2),
        fisher.test(matrix(c(x1, x2, n1 - x1, n2 - x2), nc = 2))$p.value,
        NA
      ))
  }

  pulse <- pulse %>%
    mutate(
      "Previous" = x1 / n1,
      "Current" = x2 / n2,
      "Difference" = ifelse(p < 0.05, Current - Previous, NA)
    ) %>%

    # Format NAs/NaNs and round values
    modify_at(c("Previous", "Current", "Difference"), function(x) round(x * 100, 1)) %>%
    modify_at("p", function(x) round(x, 3)) %>%
    modify_at(c("Previous", "Current"), function(x) ifelse(x == "NaN" | is.na(x), "N/A", x)) %>%

    # Choose variables to export
    select(rowname, Previous, Current, Difference, p) %>%

    # Re-add Ns
    add_row(.before = 1, rowname = "N", Previous = dat1$N[1], Current = dat2$N[1])

  return(pulse)
}
