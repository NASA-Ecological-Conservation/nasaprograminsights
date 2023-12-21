#' #'@description lskdfj
#' #'@docType data
#' #'
#' #'
#' # Directories, Filenames --------------------------------------------------
#' (dir <- list.files(path="data-raw", pattern="public", full.names=TRUE))
#' 
#' ccefn <- list.files(dir, pattern = "bef_projects", full.names = TRUE)
#' 
#' # the CCE webpage database may be out of date, so this is used to update it  (we need to send the resulting file to CCE GSFC contacts (as of 20230130, that's Amy Hodkins))
#' ### unfortunately, this exports with unecessary lines at the top, so you may want to preview the file before importing and
#' ### ensure you skip the correct num lines
#' # browseURL(ccefn)
#' webpagelookup <-
#'   readxl::read_excel(ccefn, trim_ws = TRUE, skip = 3) |>
#'   data.table::as.data.table()
#' 
#' # munge the date cols
#' date_cols <- c("Start Date", "End Date")
#' # convert date columns
#' data.table::setDT(webpagelookup)[, (date_cols) := lapply(.SD, anytime::anydate), .SDcols = date_cols]
#' 
#' # Export the lookup table to package --------------------------------------------------
#' usethis::use_data(webpagelookup, overwrite = TRUE)
