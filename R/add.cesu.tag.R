#' @title Add a Tag for CESU Member or Not
#' 
#' @description This is a minimal helper because it needs work. 
#' @export add.cesu.tag
#' 
#' 
add.cesu.tag <- function(dat.nspires, cesu=nasaprograminsights::cesu) {
  nasa.cesu <-
    cesu[cesu$funder == "National Aeronautics and Space Administration", ]
  
  # dat.nspires$`pi company name` <-
  #   toupper(dat.nspires$`pi company name`)
  
  # add index tos kip any NA or blank compnay names
  is.blank <- which(dat.nspires$`pi company name` %in% c("NA", NA, "N/A", ""))
  
  dat.nspires$nasa.member.cesu = FALSE
  dat.nspires$member.cesu = FALSE
  
  for (i in 1:nrow(dat.nspires)) {
    if(i %in% is.blank) next()
    
    ## for cesus that nasa is already in
    if(any(grepl(tolower(dat.nspires$`pi company name`[i]), x=nasa.cesu$member))){
      dat.nspires$nasa.member.cesu[i]=TRUE
    }
    
    ## for all cesu members
    if(any(grepl(tolower(dat.nspires$`pi company name`[i]), x=cesu$member))){
      dat.nspires$member.cesu[i]=TRUE
    }
    
  }#end loop
  
  return(dat.nspires)
  
}
