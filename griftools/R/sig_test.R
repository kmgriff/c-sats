# Significance testing function
#
# This function accepts two 2 X N data frames, with the first row containing successes
# and the second row containing sample sizes for each variable. The first variable of
# each data frame must be the total sample size and named "N".
#
# Significance testing uses a Chi-squared test without correction, unless argument "type"
# is set to "fisher", in which case Fisher's Exact test will be used.
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

require(tidyverse)

sig_test <- function(dat1 = NULL, dat2 = NULL, type = "chisq") {
  if (!type %in% c("chisq", "fisher")) {
    stop("'type' must be either \"chisq\" (default) or \"fisher\"")
  }

  if (!is.data.frame(dat1) & is.list(dat1)) {
    dat2 <- dat1[[2]]
    dat1 <- dat1[[1]]
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
      return(data.frame(rowname = "N/A", Current = "N/A"))
    }
  }

  # Check if current data frame is valid
  check_df(dat2)

  return_current <- function(dat) {
    # Transpose columns into rows
    dat <- dat %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column()

    header <- dat %>%
      slice(1) %>%
      select(rowname, "Current" = 2)

    dat <- dat %>%
      slice(-1) %>%
      select(rowname, "x" = 2, "n" = 3) %>%
      mutate("Current" = x / n) %>%
      select(rowname, Current) %>%
      modify_at("Current", ~ round(.x * 100, 1)) %>%
      bind_rows(header, .) %>%
      modify_at("Current", ~ ifelse(is.nan(.x) | is.na(.x), "N/A", .x)) %>%
      return()
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


  # Transpose columns into rows
  pulse <- bind_rows(dat1, dat2) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column()

  # Rename columns
  pulse <- pulse %>%
    rename("x1" = 2, "n1" = 3, "x2" = 4, "n2" = 5)

  # Extract header
  header <- pulse %>%
    slice(1) %>%
    select(rowname, "Previous" = x1, "Current" = x2)

  # Calculate means and st.dev
  pulse <- pulse %>%
    slice(-1) %>%
    mutate("Previous" = x1 / n1, "Current" = x2 / n2) %>%
    mutate(
      "s1" = sqrt(Previous * (1 - Previous) / n1),
      "s2" = sqrt(Current * (1 - Current) / n2)
    ) %>%
    rowwise()

  # Significance test
  if (type == "chisq") {
    pulse <- pulse %>%
      mutate("p" = ifelse((n1 >= x1) & (n2 >= x2) & (n1 > 0) & (n2 > 0),
        prop.test(matrix(c(x1, x2, n1 - x1, n2 - x2), nc = 2), correct = FALSE)$p.value,
        NA
      ))
  } else if (type == "fisher") {
    pulse <- pulse %>%
      mutate("p" = ifelse((n1 >= x1) & (n2 >= x2) & (n1 > 0) & (n2 > 0),
        fisher.test(matrix(c(x1, x2, n1 - x1, n2 - x2), nc = 2))$p.value,
        NA
      ))
  }

  # Add Difference column
  pulse %>%

    # Extract p-value from sig test
    modify_at("p", ~ ifelse(is.na(.x), NA, round(.x, 3))) %>%
    mutate("Difference" = ifelse(p < 0.05, Current - Previous, NA)) %>%

    # Round values
    modify_at(c("Previous", "Current", "Difference"), ~ round(.x * 100, 1)) %>%

    # Add header
    bind_rows(header, .) %>%

    # Format NAs and NaNs
    modify_at(c("Previous", "Current"), ~ ifelse(is.na(.x) | is.nan(.x), "N/A", .x)) %>%

    # Choose variables to export
    select(rowname, Previous, Current, Difference, p) %>%
    ungroup() %>%
    return()
}
