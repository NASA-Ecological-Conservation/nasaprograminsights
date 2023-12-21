#' NASA Grant Status Form as a Flat Table
#'
#' @name grantstatusform
#' @docType data
#' @author Jessica Burnett \email{jessica.burnett@nasa.gov}
#' @references \url{https://www.nasa.gov/centers/nssc/forms/grant-status-form}
#' @keywords data grants
#' @description
#' This datatable contains multiple, hand-corrected and munged files from a public-facing source, Grant Status Form, for NASA grant award IDs.
#' Unfortunately, this data table is made by downloading Grant Status Form exports one at a time (as of 2023).
NULL

# specify the directory containing data and the NSPIRES imported information
dir <- list.files("/Users/jlburne3/Library/CloudStorage/OneDrive-NASA/bdecprogrameval/data-raw", pattern="grantstatusform",  full.names = TRUE)

# These files were __hand-munged__ b/c the grant status form only exports as .csv and many of the rows are riddled with errors.
fns <- list.files(dir, pattern="awarddates", full.names = TRUE, recursive = TRUE)

grantstatusform <- lapply(
  fns,
  data.table::fread, quote='\"',
  na = c(NA_character_, "", NA, "(N/A)", "N/A"),
  colClasses = c(
    "Purchase Requisition Number" = "character",
    "Grant Number" = "character",
    "Institution" = "character",
    "NASA Center" = "character",
    "Proposal Title" = "character",
    "Program Title" = "character",
    "Principal Investigator Name" = "character",
    "Technical Officer" = "character",
    "Award Date" = "character",
    "Performance Start Date" = "character",
    "Performance End Date" = "character",
    "Status" = "character",
    "Description of Action" = "character"
  )
) |>
  data.table::rbindlist()

# munge the date cols
date_cols <- c("Award Date", "Performance Start Date", "Performance End Date")
# convert date columns (takes ~20-30 seconds)
data.table::setDT(grantstatusform)[, (date_cols) := lapply(.SD, anytime::anydate), .SDcols = date_cols]

grantstatusform = as.data.frame(grantstatusform)

# Export Data to Package --------------------------------------------------
usethis::use_data(grantstatusform, overwrite=TRUE)