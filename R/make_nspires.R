#' @title Make a list of munged NSPIRES data using local data sources. 
#' @export
#' @name dir Directory for where the internal data is stored. This can contain subdirectories, as this function attempts to import data as recursive=TRUE.
#' @name N Parameter used to facilitate data import. Can ignore. A # b/w 100-300 is ideal.


make_nspires <- function(dir="data-raw/data-raw-internal/nspires-internal", N=200){
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

for(i in 1:round(length(propfns)/N)){
  # print("i")
  if(i==1){ temp <- proposals <- NULL}
  low <- i*N-N
  high <- i*N
  if(high > length(propfns))high=length(propfns)
  tempfns <- propfns[low:high]
  # temp[[i]] <- lapply(tempfns, data.table::fread) |> ## fread keeps crashing even when ram use is very low...annoying 
  temp[[i]] <- lapply(tempfns, read.csv) |> ## fread keeps crashing even when ram use is very low...annoying 
    data.table::rbindlist(fill=TRUE)
  if(i == max(round(length(propfns) / N))) {
    proposals <- data.table::rbindlist(temp, fill = TRUE) |> dplyr::select_if(not_all_na)#remove column if all fields == NA
    rm(temp, low, high, i, N)
  }
}

# clean the colnames....ugh so crazy that the fields are so shitty
proposals <- munge.nspires.proposals(df=proposals)


# Import and Munge People Data --------------------------------------------
# i should probably move out of data.table
people <- lapply(pplfns, data.table::fread) |>
  data.table::rbindlist(fill=TRUE) |> 
  as.data.frame() 

if(!all(people$`pi suid` %in% proposals$`pi suid`))
  warning("FYI. -- not all PI SUIDs from 'people' are in 'proposals'"
  )

# Export Data Together to Package as "nspires"  -----------------------------------------------------------
nspires <- list()
nspires$proposals <- proposals
nspires$people <- people

rm(proposals, people)

return(nspires)
}

