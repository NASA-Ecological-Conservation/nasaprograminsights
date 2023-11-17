#' @title Make a list of munged NSPIRES data using local data sources. 
#' @export
#' @param tokeep Character vector. Will keep only the proposals with proposal stus in those listed in the vector. IF NULL, will not remove any proposals.
#' @param removeppl Logical. Default TRUE will keep ONLY the people who are associated with proposals in the data frame. FALSE will keep all people.
#' @param dir Directory for where the internal data is stored. This can contain subdirectories, as this function attempts to import data as recursive=TRUE.
#' @param N Parameter used to facilitate data import. Can ignore. A # b/w 100-300 is ideal. Default 200.
#' @param returnclean Logical. Default TRUE will return the "people" data frame (list element) with reduced infomration. Columns removed include related proposal Titles, 
#' @param addprogramname Logical. If TRUE, will append the program name (`program name`) from the internal lookup table, `lookup`
#' @param dealwithspecialcases Logical. Default TRUE will handle some of the special-case NRA number/solicitation lookup id has (e.g., "ECOSTRES" versus "ECOSTRESS).
 
make_nspires <- function(dir="nspires-data", # where is the internal data stored
                         N=200,
                         tokeep=c("selected", "declined", "submitted","selectable","invited","awarded","rejected"), 
                         removeppl=TRUE, 
                         returnclean=TRUE, 
                         dealwithspecialcases = TRUE,
                         addprogramname=TRUE){
# light helper funs...
not_any_na <- function(x) all(!is.na(x))
not_all_na <- function(x) any(!is.na(x))

# Create temporary filename objs for import --------------------------------------------------
### this assumes that there are two types of files, one with the name "people" in filename and one with the word "master". 
allpropfiles <- list.files(dir, pattern="proposal", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
propfns  <- allpropfiles[grepl(x = allpropfiles, pattern = "master", ignore.case = TRUE)]
pplfns  <- allpropfiles[grepl(x = allpropfiles, pattern = "people", ignore.case = TRUE)]

# Import & munge proposal master data ------------------------------------------------------------
## so, using data.table is nice, because it removes the periods in colnames
## however, when propfns len >>500, i have issues with data.table::fread...
## therefore, i've created a loop to pull in N fns at a time
proposals <- NULL
for(i in 1:round(length(propfns)/N)){
  if(i==1){ temp <- proposals <- NULL}
  low <- i*N-N
  high <- i*N
  if(high > length(propfns))high=length(propfns)
  tempfns <- propfns[low:high]
  # temp[[i]] <- lapply(tempfns, data.table::fread) |> ## fread keeps crashing even when ram use is very low...annoying 
  temp[[i]] <- lapply(tempfns, read.csv) |> #, fileEncoding = "UTF-16LE") |> ## fread keeps crashing even when ram use is very low...annoying 
    data.table::rbindlist(fill=TRUE)
  if(i == max(round(length(propfns) / N))) {
    #remove column if all fields == NA
    # remove duplicate rows (incase of duplicate files)
    proposals <- data.table::rbindlist(temp, fill = TRUE) |> dplyr::select_if(not_all_na) |> dplyr::distinct() 
    rm(temp, low, high, i)
  }
}

# clean the colnames....
proposals <- munge.nspires.proposals(df=proposals) 

## HANDLE SPECIAL CASES
# "Decisions/05" # but maybe keep because thats how ASP mapper has it..
# 2-step proposals
# ecostres vs ecostress
if(dealwithspecialcases){
  # need to figure otu the regex way but this will suffice for now
# grepl('.*ECOSTRES*(\\d+)', proposals$`solicitation id`) |> which() |> length()
proposals$`solicitation id` <-  
  stringr::str_replace_all(
    string = proposals$`solicitation id`,
    pattern = "ECOSTRES1",
    replacement = "ECOSTRESS1"
  )
proposals$`solicitation id` <-  
  stringr::str_replace_all(
    string = proposals$`solicitation id`,
    pattern = "ECOSTRES2",
    replacement = "ECOSTRESS2"
  )

proposals$`solicitation id` <-  
  stringr::str_replace_all(
    string = proposals$`solicitation id`,
    pattern = "DISASTER1",
    replacement = "DISASTERS1"
  )
proposals$`solicitation id` <-  
  stringr::str_replace_all(
    string = proposals$`solicitation id`,
    pattern = "DISASTER2",
    replacement = "DISASTERS2"
  )

} # end special cases


# If indicated, filter the proposals using tokeep
if(!is.null(tokeep)){
 proposals <- proposals |> 
  dplyr::filter(tolower(`proposal status`) %in% tolower(tokeep))
}

if(addprogramname){
  proposals <- merge(x = proposals, y = nasaprograminsights::lookup, 
                     all = TRUE, #keep all proposals and solicitations
                     by = "solicitation id")
  stopifnot("program name" %in% tolower(colnames(proposals)))
  }

# Import and Munge People Data --------------------------------------------
# i should probably move out of data.table
people <- lapply(pplfns, data.table::fread) |>
  data.table::rbindlist(fill=TRUE) |> 
  as.data.frame() |> 
  munge.nspires.people()

# Resolve argument
if(removeppl){
  people <- people |> dplyr::filter(`proposal number` %in% proposals$`proposal number`)
}

# if(!all(people$`member suid` %in% proposals$`pi suid`))
#   warning("FYI. -- not all PI SUIDs from 'people' are in 'proposals'"
# )

# Export Data Together to Package as "nspires"  -----------------------------------------------------------
# to be safe
if(dplyr::is_grouped_df(proposals)) proposals <- dplyr::ungroup(proposals) 
if(dplyr::is_grouped_df(people)) people <- dplyr::ungroup(people) 

nspires <- list()
nspires$proposals <- as.data.frame(proposals)
nspires$people <- as.data.frame(people)
nspires$lookup <- as.data.frame(nasaprograminsights::lookup)



rm(proposals, people)

return(nspires)
}

