# Generate dates
#
# This function requires that griftools::generate_dates has been called.  Accepts a data frame,
# a function to convert raw data into 2 x N summaries, the name of the Date column, and options
# to include quarterly, YTD, and previous YE columns.  Returns a pulse report in the usual style.

require(tidyverse)

generate_pulse <- function(dat, sum_func, DateCol = "Date", qtr_columns = TRUE, ytd_column = TRUE, prev_ye_column = FALSE, sig = "chisq") {

  # Check if generate_dates has been called
  checkvars <- c("t_prev_mth", "t_cur_mth", "prev_month", "cur_month")
  if (qtr_columns) checkvars <- c(checkvars, "t_prev_qtr", "t_cur_qtr", "prev_quarter", "cur_quarter")
  if (ytd_column) checkvars <- c(checkvars, "t_yr", "cur_year")
  map(checkvars, ~ if (!exists(.x)) stop(str_glue("object '{.x}' not found")))

  # Suppress inaccurate chi-square warnings
  quiet_sigtest <- purrr::quietly(griftools::sig_test)

  # Generate previous and current month values, difference between months, and p-value
  monthly <- quiet_sigtest(
    dat %>% filter_at(vars(DateCol), all_vars(. >= t_prev_mth & . < t_cur_mth)) %>% sum_func(),
    dat %>% filter_at(vars(DateCol), all_vars(. >= t_cur_mth)) %>% sum_func(),
    type = sig
  )$result %>%
    rename_at("Current", ~cur_month)
  if ("Previous" %in% names(monthly)) {
    monthly <- monthly %>%
      rename_at("Previous", ~prev_month) %>%
      rename("Diff_month" = Difference, "p_month" = p)
  }

  if (qtr_columns) {

    # Generate previous and current quarter values, difference between quarters, and p-value
    quarterly <- quiet_sigtest(
      dat %>% filter_at(vars(DateCol), all_vars(. >= t_prev_qtr & . < t_cur_qtr)) %>% sum_func(),
      dat %>% filter_at(vars(DateCol), all_vars(. >= t_cur_qtr)) %>% sum_func(),
      type = sig
    )$result %>%
      rename_at("Current", ~cur_quarter)
    if ("Previous" %in% names(quarterly)) {
      quarterly <- quarterly %>%
        rename_at("Previous", ~prev_quarter) %>%
        rename("Diff_quarter" = Difference, "p_quarter" = p)
    }
  }

  if (ytd_column) {

    if (prev_ye_column) {

      # Generate previous YE and current YTD values, difference between years, and p-value
      ytd <- quiet_sigtest(
        dat %>% filter_at(vars(DateCol), all_vars(lubridate::year(.) == cur_year - 1)) %>% sum_func(),
        dat %>% filter_at(vars(DateCol), all_vars(lubridate::year(.) == cur_year)) %>% sum_func(),
        type = sig
      )$result %>%
        rename_at("Current", ~paste(cur_year, "YTD"))
      if ("Previous" %in% names(ytd)) {
        ytd <- ytd %>%
          rename_at("Previous", ~paste(cur_year - 1, "YE")) %>%
          rename("Diff_year" = Difference, "p_year" = p)
      }
    } else {

      # Generate YTD columns (sig_test returns only "rowname" and "Current" if passed two identical data frames)
      ytd <- quiet_sigtest(
        dat %>%
          filter_at(vars(DateCol), all_vars(lubridate::year(.) == cur_year)) %>%
          sum_func() %>%
          list(., .),
        type = sig
        )$result %>%
        rename_at("Current", ~paste(cur_year, "YTD"))
    }

    # If N = 0, return only YTD column
    if (ytd[1, 1] == "N/A") {
      return(ytd)
    }

    # If current month has N = 0, generate column with N = 0 and values = "N/A"
    len <- length(ytd[, 1])
    if (length(monthly[, 1]) != len) {
      monthly <- data.frame(rowname = ytd$rowname, "V1" = c(0, rep("N/A", len - 1))) %>%
        rename_all(~ names(monthly))
    }

    if (qtr_columns) {

      # If current quarter has N = 0, generate column with N = 0 and values = "N/A"
      if (length(quarterly[, 1]) != len) {
        quarterly <- data.frame(rowname = ytd$rowname, "V1" = c(0, rep("N/A", len - 1))) %>%
          rename_all(~ names(quarterly))
      }
    }
  } else if (qtr_columns) {

    # If current month has N = 0, generate column with N = 0 and values = "N/A"
    len <- length(quarterly[, 1])
    if (length(monthly[, 1]) != len) {
      monthly <- data.frame(rowname = quarterly$rowname, "V1" = c(0, rep("N/A", len - 1))) %>%
        rename_all(~ names(monthly))
    }
  }

  # Combine monthly, quarterly, and ytd columns
  ret <- list(monthly)
  if (qtr_columns) ret <- c(ret, list(quarterly))
  if (ytd_column) ret <- c(ret, list(ytd))

  reduce(ret, ~ full_join(.x, .y, by = "rowname")) %>%
    return()
}
