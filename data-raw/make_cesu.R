#' @name cesu
#' @description A hand-made lookup table listing CESU partnerships
#' @docType data
#' @export
#' A lookup table containing (as of 2024-02-24) all members of Cooperative Ecosystems Studies Units. 
#' The original file is wide, here is converted to long.  

# Import Data -------------------------------------------------------------
fn <- list.files(pattern="cesu-partner", full.names=TRUE, recursive=TRUE, ignore.case = TRUE)
stopifnot(length(fn)==1)
cesu <- read.csv(fn, strip.white = TRUE, check.names=FALSE) |> dplyr::select(-Notes)
cesu <- tidyr::pivot_longer(cesu, cols=1:ncol(cesu) ,names_to = "unit", values_to = "member") 

#remove blanks, nas
cesu <- cesu[cesu$member!="",]

cesu$unit <- as.factor(cesu$unit)
cesu$member <- as.factor(cesu$member)

# Export Data to Pkg ------------------------------------------------------
usethis::use_data(cesu, overwrite=TRUE)
