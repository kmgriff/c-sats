# Load libraries
library(tidyverse)
library(readxl)
library(xlsx)
library(haven)
library(griftools)

# Style R file
# library(styler)
# style_dir()

# User-defined variables
cur_month <- "December"
cur_year <- 2019
data_filesearch <- "Fld Ops SPSS"
verbs_filesearch <- "Field Ops Verbatim"



# Generate dates and labels: prev_month, cur_quarter, prev_quarter, t_cur_mth, t_cur_qtr, t_prev_mth, t_prev_qtr, t_yr, is_eoq
griftools::generate_dates()

# Pull and clean all data files
rawdat <- map(dir(pattern = data_filesearch), ~ read_sav(.x)) %>%
  bind_rows() %>%

  # Create Date column from Month (Q8) and Year (Q9)
  mutate(Date = lubridate::myd(paste(Q8, Q9, "1"))) %>%
  arrange(Date)



# Create 2 x N summary for use in sig-testing function
summary_from_dat <- function(dat) {
  big_N <- length(dat[[1]])

  # Selection of variables on 0-10 scale
  dat_0_10 <- dat %>%
    select(Q2_1, Q2_2, Q3_1, Q3_2, Q3_3, Q3_4, Q3_5)

  # Expectations question
  dat_expect <- dat %>%
    select(Expect = Q2_3)

  # Calculate Ns
  dat_N <- bind_cols(dat_0_10, dat_expect) %>%
    summarize_all(~ sum(.x %in% 0:10))

  # Satisfied (6-10)
  dat_0_10 <- dat_0_10 %>%
    summarize_all(~ sum(.x %in% 6:10))

  # Exceeds expectations (3)
  dat_expect <- dat_expect %>%
    summarize_all(~ sum(.x == 3, na.rm = TRUE))

  # Combine variables
  bind_cols(dat_0_10, dat_expect) %>%
    bind_rows(dat_N) %>%
    mutate("N" = big_N) %>%
  
    # Move and rename columns
    select("N",
      "Overall Satisfaction with Company's performance" = Q2_1,
      "Exceed your expectations" = Expect,
      "Overall Satisfaction with Field Crew" = Q3_1,
      "Tracking variable 4" = Q3_2,
      "Tracking variable 5" = Q3_3,
      "Tracking variable 6" = Q3_4
    ) %>%
    return()
}



# Generate pulse report with quarterly columns (if end of quarter)
final_results <- griftools::generate_pulse(dat = rawdat,
                                           sum_func = summary_from_dat,
                                           qtr_columns = is_eoq,
                                           ytd_column = FALSE) %>%

  # Insert blank rows to match pulse report format
  add_row(.before = 2, rowname = "Overall Performance") %>%
  add_row(.before = 6) %>%
  add_row(.before = 7, rowname = "Field Crew Personnel")



# Pull results by field tech
crew_dat <- rawdat %>%
  filter(Date == t_cur_mth) %>%
  select(Tech, Q2_1, Q3_1) %>%
  group_by(Tech) %>%
  arrange(Tech)

# Generate Field Crew pulse report
final_crew <- full_join(
  crew_dat %>% summarize("N" = n()),
  crew_dat %>% summarize_all(~ {
    sum(.x %in% 6:10, na.rm = TRUE) * 100 / sum(.x %in% 0:10, na.rm = TRUE)
  }),
  by = "Tech"
) %>%

  # Format NAs/NaNs
  modify_at(c("Q2_1", "Q3_1"), ~ round(.x, 1)) %>%
  modify(~ ifelse(.x == "NaN" | is.na(.x), "N/A", .x)) %>%
  select("Tech",
    "N",
    "Overall Satisfaction with Company's performance" = Q2_1,
    "Overall Satisfaction with Field Crew" = Q3_1
  ) %>%

  # Transpose rows and columns
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%

  # Insert blank rows to match pulse report format
  add_row(.before = 3, rowname = "OVERALL MEASURE") %>%
  add_row(.before = 5) %>%
  add_row(.before = 6, rowname = "FIELD CREW MEASURE")



# Export pulse report and crew results
griftools::export_workbook(
  data = list(final_results, final_crew),
  sheetname = c(paste(cur_month, cur_year), "Field Crew"),
  fileprefix = str_glue("Field Ops Results - {cur_month} {cur_year}")
)



# Pull verbatims from file
verbs <- dir(pattern = verbs_filesearch) %>%
  read_excel() %>%
  filter(FileDate == t_cur_mth) %>%
  select(
    Verbatim = Q4_OE,
    "Company Satisfaction" = Q2_1,
    "Field Crew Satisfaction" = Q3_1,
    Expectations = Q2_3,
    Date, "Field Order", Street, City, Zip, Month, Year
  ) %>%
  filter(!is.na(Verbatim)) %>%
  mutate_at("Expectations", ~case_when(
    .x == 1 ~ "Fall short",
    .x == 2 ~ "Meet expectations", 
    .x == 3 ~ "Exceed expectations",
    .x == 4 ~ "Don't know",
    .x == 5 ~ "No answer",
    TRUE ~ as.character(NA)
    )) %>%
  arrange(`Company Satisfaction`)


# Export verbatims
griftools::export_workbook(
  data = verbs,
  sheetname = "Field Ops",
  fileprefix = str_glue("Field Ops - {cur_month} {cur_year} Verbatims")
)
