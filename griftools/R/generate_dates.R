# Generate dates
#
# This function requires the presence of cur_month and cur_year, which are strings
# containing the current month and year, e.g. "December" and 2019.
# Lubridate-style Date objects and column labels are generated and assigned directly to the global environment.

require(tidyverse)

generate_dates <- function() {
  map(
    c("cur_month", "cur_year"),
    ~ if (!exists(.x)) stop(str_glue("object '{.x}' not found"))
  )

  t_cur_mth <<- lubridate::myd(paste(cur_month, cur_year, 1))
  t_cur_qtr <<- t_cur_mth - months((lubridate::month(t_cur_mth) - 1) %% 3)
  t_prev_mth <<- t_cur_mth - months(1)
  t_prev_qtr <<- t_cur_qtr - months(3)
  t_yr <<- lubridate::ymd(paste(cur_year, 1, 1))
  cur_month <<- lubridate::month(t_cur_mth, label = TRUE, abbr = FALSE) %>% as.character()
  prev_month <<- lubridate::month(t_prev_mth, label = TRUE, abbr = FALSE) %>% as.character()
  cur_year <<- lubridate::year(t_cur_mth)
  cur_quarter <- floor((lubridate::month(t_cur_mth) - 1) / 3) + 1
  prev_quarter <<- sprintf("Q%d %d", (cur_quarter - 2) %% 4 + 1, ifelse(cur_quarter > 1, cur_year, cur_year - 1))
  cur_quarter <<- sprintf("Q%d %d", cur_quarter, cur_year)
  is_eoq <<- lubridate::month(t_cur_mth) %% 3 == 0
}
