## thos helper function attempts to de-duplicate columns by coalescing those with same names

deduplicate <- function(dat){
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

if(any(duplicated(colnames(dat))))warning("not all duplications were addressed. run 'any(duplicated(dat))' to check.")

  return(dat)


} # END FUN}
