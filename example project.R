# Load libraries
library(tidyverse)
library(readxl)
library(xlsx)
library(haven)
library(griftools)

# Style R files
# library(styler)
# style_dir()

# User-defined variables
prev_column_list <- c("Q1_OSat", "Q2_Expectation", "Q3_Info", "Q4_Courteous")
cur_column_list <- prev_column_list
cur_monthyear <- "May 2019"
cur_quarter <- "Q2 2019"
prev_file_search <- "File_April"
cur_file_search <- "File_May"
prev_quarter_file_search <- c("File_Jan", "File_Feb", "File_Mar")
cur_quarter_file_search <- c("File_April", "File_May")



# Create summary from trimmed data frame
summary_from_dat <- function(dat) {

  # Scale 1-5 (positive: 4-5):
  # [1] Q1_OSat
  # [3] Q3_Info
  # [4] Q4_Courteous
  dat_1_5 <- dat %>%
    select(V1, V3, V4) %>%
    summarize_all(function(x) sum(x %in% 4:5, na.rm = TRUE))

  # Special:
  # [2] Q2_Expectation: "Exceed expectations" or "Exceed your expectations", exclude "Don't know"
  dat_special <- dat %>%
    summarize(
      V2 = sum(str_detect(V3, "Exceed"), na.rm = TRUE)
    )

  dat_N <- dat %>%
    summarize_all(function(x) sum(!str_detect(x, "on't know") & !is.na(x)))

  bind_cols(dat_1_5, dat_special) %>%
    bind_rows(dat_N) %>%
    mutate("N" = length(dat[[1]])) %>%
    select("N",
      "Overall Satisfaction" = V1,
      "Exceed your expectations" = V2,
      "Information provided" = V3,
      "Treating you with courtesy" = V4
    )
}

# Insert blank rows to match report format
format_report <- function(dat) {
  dat %>%
    add_row(.before = 2, rowname = "Overall Measures") %>%
    add_row(.before = 4) %>%
    add_row(.before = 5, rowname = "Expectation Measure") %>%
    add_row(.before = 7) %>%
    add_row(.before = 8, rowname = "Attributes") %>%

    # Convert to dataframe
    as.data.frame()
}




# Pull monthly data
prev_dat <- griftools::pull_rawdata(prev_file_search)
cur_dat <- griftools::pull_rawdata(cur_file_search)

# Check for changed columns
cur_column_list <- griftools::check_col_changes(prev_dat, cur_dat, prev_column_list, cur_column_list)

# Create summaries
prev_sum <- prev_dat %>%
  griftools::trim_rawdata(prev_column_list) %>%
  summary_from_dat()
cur_sum <- cur_dat %>%
  griftools::trim_rawdata(cur_column_list) %>%
  summary_from_dat()

# Create pulse report using chi-square sig test without correction
final_month <- griftools::sig_test(prev_sum, cur_sum) %>%
  format_report()



# Pull quarterly data
q1_dat <- map(prev_quarter_file_search, griftools::pull_rawdata)
q2_dat <- map(cur_quarter_file_search, griftools::pull_rawdata)

# Create summaries
q1_sum <- q1_dat %>%
  bind_rows() %>%
  griftools::trim_rawdata(prev_column_list) %>%
  summary_from_dat()
q2_sum <- q2_dat %>%
  bind_rows() %>%
  griftools::trim_rawdata(prev_column_list) %>%
  summary_from_dat()

# Modify and use if columns change between months
# q2_sum <- bind_rows(
#   q2_dat[1:2] %>%
#     map(griftools::trim_rawdata(prev_column_list)),
#   q2_dat[3] %>%
#     griftools::trim_rawdata(cur_column_list)
#   ) %>%
#   summary_from_dat()

# Create pulse report using chi-square sig test without correction
final_quarter <- griftools::sig_test(q1_sum, q2_sum) %>%
  format_report()

griftools::export_workbook(
  list(final_month, final_quarter), list(cur_monthyear, cur_quarter),
  str_glue("R Export_Satisfaction Report_{cur_monthyear}")
)
