## helper functions for de-duplicate columns and rows by coalescing those with same names, checking duplicate proposals (since we have two data sources)


# deduplicate_cols --------------------------------------------------------
#' @title Deduplicate columns in NSPIRES internal dataframes
#' @param dat a data.frame comprising NSPIRES proposal_master files
#' @export
deduplicate_cols <- function(dat){
  if(!any(duplicated(colnames(dat)))){
    # message("No duplicate column names found. Not running this function. ");
    return(dat)
  }
 n <- colnames(dat)
  colsremove <- NULL
  for(i in seq_along(n)){
    inds <- which(tolower(n) %in% tolower(n)[i])
    if(length(inds)<=1)next()
    colsremove <- c(colsremove, inds[2:length(inds)])
    dat[, inds[1]] <-  as.data.frame(dplyr::coalesce(!!!dat[, inds]))
  }
  if(length(colsremove)>0)  dat <- dat[,-colsremove]

## deal with the 'status' columns
  ## force "status" to "proposal status"
 if(any(duplicated(colnames(dat))))warning("not all duplications were addressed. run 'any(duplicated(dat))' to check.")

  return(dat)


} # END FUN

# deduplicate_rows --------------------------------------------------------
#' @title Deduplicate rows (people, proposals) in NSPIRES internal dataframes
#' @param dat a data.frame comprising NSPIRES people_master files
#' @export
#' @param dat a data.frame comprising NSPIRES either proposal_master or people_master files
#' 
deduplicate_rows <- function(dat){
  
  dat <- dat |> dplyr::distinct(`proposal number`, `pi suid`, .keep_all = TRUE)
  dat <- dat |> dplyr::group_by(title) |> dplyr::arrange(title, desc(`proposal number`)) |> dplyr::distinct(title, `pi suid` , .keep_all = TRUE) |> dplyr::ungroup()

  return(dat)  
}
