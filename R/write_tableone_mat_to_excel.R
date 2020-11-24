#' Write tableone mat to excel file
#'
#' @param tableone_mat a tableone-object
#' @param file the name of the file.xlsx
#'
#' @return
#' @export
#'
#' @references developed & published in a gist from Kazuki Yoshida
#' https://gist.github.com/kaz-yos/7b4e97b633d61fa4d1d27f46f2534ae3
#'
write_tableone_mat_to_xlsx <- function(tableone_mat, file) {
  ## Create a workbook object with one sheet
  ## https://rdrr.io/cran/openxlsx/man/setColWidths.html
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = "1")

  ## Write data frame data to the workbook object
  openxlsx::writeData(wb, sheet = 1, x = tableone_mat_to_data_frame(tableone_mat))

  ## Fix column width automatically
  openxlsx::setColWidths(wb, sheet = 1, cols = base::seq_len(base::ncol(tableone_mat)), widths = "auto")

  ## Format the variable name column
  ## https://rdrr.io/cran/openxlsx/man/createStyle.html
  varname_style <- openxlsx::createStyle(halign = "left", valign = "center")
  openxlsx::addStyle(wb, sheet = 1, style = varname_style, rows = base::seq_len(base::nrow(tableone_mat) + 1), cols = 1, gridExpand = TRUE)

  ## Format all other columns
  varval_style <- openxlsx::createStyle(halign = "center", valign = "center")
  openxlsx::addStyle(wb, sheet = 1, style = varval_style,  rows = base::seq_len(base::nrow(tableone_mat) + 1),
                     cols = base::seq_len(base::ncol(tableone_mat))[-1], gridExpand = TRUE)

  ## Save to a file
  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

  }
