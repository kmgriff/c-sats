# Export workbook
#
# This function accepts a list of data frames, a list of sheetnames, and a filename prefix,
# then creates an Excel workbook. The filename is in the format "[fileprefix]_YYYYMMDD_HHmmss".



export_workbook <- function(data, sheetname = "Sheet1", fileprefix = "R Export") {
  if (is.data.frame(data)) {
    data <- list(data)
  }

  if (length(data) != length(sheetname)) {
    stop("\'data\' and \'sheetname\' lengths are not equal")
  }

  wb <- createWorkbook()
  walk2(data, sheetname, function(dat, shtname) {
    sht <- createSheet(wb, shtname)
    addDataFrame(dat %>% as.data.frame(), sheet = sht, row.names = FALSE)
  })

  timestamp <- lubridate::now() %>%
    str_replace_all(., "-", "") %>%
    str_replace_all(., ":", "") %>%
    str_replace(., " ", "_")

  outfile <- str_glue("{fileprefix}_{timestamp}.xlsx")

  saveWorkbook(wb, outfile)
}
