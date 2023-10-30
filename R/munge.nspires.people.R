# munge.nspires.people ----------------------------------------
#' @name munge.nspires.people
#' @title Munge internal NSPIRES people databases
#' @description returns an object of class data.frame
#' @param df Data from the internal-access-only NSPIRES. Currently this function only really supports the "people" worksheets.
#' @export

munge.nspires.people <- function(df){
      df <- as.data.frame(df)
      # for some reason the "committed" date doesnt parse, no clue why because even the clearly "dmy" fields wont parse. whatever. drop the POS
      # the fields "PI Last Name and PI First Name" are redundant with the "Member Last and First, bevause they also have their own rows and their roles == PI
      # dropcols <- which(tolower(colnames(df)) %in% c("pi last name", "pi first name", "committed", "response seq number"))

      ## why the hell arent these named the same thing!?!?!?!? THIS MAKSE ZERO SENSE!!!!
      # in proposal data, prop number =="Proposal Number"
      # in people data, prop number =="Response Number"
      colnames(df)[which(tolower(colnames(df))=="response number")] <- "Proposal Number"
      colnames(df) <- tolower(colnames(df))
      
      

      
 return(df)
}
