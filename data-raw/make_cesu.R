#' @name cesu
#' @description A hand-made lookup table listing CESU partnerships
#' @docType data
#' @export
#' A lookup table containing (as of 2024-02-24) all members of Cooperative Ecosystems Studies Units. 
#' The original file is wide, here is converted to long.  

# Import Data -------------------------------------------------------------
fn <- list.files(pattern="cesu-partner", full.names=TRUE, recursive=TRUE, ignore.case = TRUE)
stopifnot(length(fn)==1)
(sheets=readxl::excel_sheets(fn))

members = readxl::read_excel(fn, sheet = "members")
funders = readxl::read_excel(fn, sheet = "funders") 

members <-
  tidyr::pivot_longer(
    members,
    cols = 1:ncol(members) ,
    names_to = "unit",
    values_to = "member"
  ) |> 
  dplyr::filter(!is.na(member))
funders <-
  tidyr::pivot_longer(
    funders,
    cols = 1:ncol(funders) ,
    names_to = "unit",
    values_to = "funder"
  ) |> 
  dplyr::filter(!is.na(funder))

cesu <- merge(funders, members, all = TRUE)

# Export Data to Pkg ------------------------------------------------------
usethis::use_data(cesu, overwrite=TRUE)
