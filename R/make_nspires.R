#' @title Make a list of munged NSPIRES data using local data sources. 
#' @export
#' @param tokeep Character vector. Will keep only the proposals with proposal stus in those listed in the vector. IF NULL, will not remove any proposals.
#' @param removeppl Logical. Default TRUE will keep ONLY the people who are associated with proposals in the data frame. FALSE will keep all people.
#' @param dir Directory for where the internal data is stored. This can contain subdirectories, as this function attempts to import data as recursive=TRUE.
#' @param N Parameter used to facilitate data import. Can ignore. A # b/w 100-300 is ideal. Default 200.
#' @param returnclean Logical. Default TRUE will return the "people" data frame (list element) with reduced infomration. Columns removed include related proposal Titles, 
#' @param addprogramname Logical. If TRUE, will append the program name (`program name`) from the internal lookup table, `lookup`. Note that some proposals/solicitations will not have a value for `program name` (i.e. NA).

make_nspires <- function(dir="nspires-data", # where is the internal data stored
                         N=200,
                         tokeep=c("selected", "declined", "submitted","selectable","invited","awarded"), 
                         removeppl=TRUE, 
                         returnclean=TRUE, 
                         addprogramname=TRUE){
  
# FOR DEV only
# N=200;tokeep=c("selected", "declined", "submitted","selectable","invited","awarded");removeppl=TRUE;returnclean=TRUE; dealwithspecialcases = TRUE;  addprogramname=TRUE
# light helper funs...
not_any_na <- function(x) all(!is.na(x))
not_all_na <- function(x) any(!is.na(x))

# Create temporary filename objs for import --------------------------------------------------
### this assumes that there are two types of files, one with the name "people" in filename and one with the word "master". 
allfns <- list.files(dir, recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
#ignore anything that isnt a .txt. xlsx or csv
allfns <- which(stringr::str_detect(allfns, pattern = ".*\\.(txt|xlsx|csv)$"))
allpropfns <- list.files(dir, pattern="proposal", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)

propfns  <- allpropfns[grepl(x = allpropfns, pattern = "master|msater|mastr", ignore.case = TRUE)]
pplfns  <- allpropfns[grepl(x = allpropfns, pattern = "peopl|people|poeple|poeple", ignore.case = TRUE)]
aorfns  <- allpropfns[grepl(x = allpropfns, pattern = "aor", ignore.case = TRUE)]

# Import & munge proposal master data ------------------------------------------------------------
## so, using data.table is nice, because it removes the periods in colnames
## however, when propfns len >>500, i have issues with data.table::fread...
## therefore, i've created a loop to pull in N fns at a time

proposals <- NULL
for(i in 1:round(length(propfns)/N)){
  if(i==1){ temp <- proposals <- NULL;
  print(paste("Importing proposals data from local machine... (ignore the 'embedded nul(s) warning for now...)"))
  }else{print(paste("Importing proposals data from local machine..."))}
  if(i==10) print("still importing, hold your horses!")
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
print("...done importing proposals data.")

# Munge some of the messy stuff in background ----
proposals <- munge.nspires.proposals(df=proposals) 

# Add inferred or actual NRA number  -------------------------------------------------
toinfer=which(!proposals$`solicitation id` %in% lookup$`solicitation id`)
toadd=which(proposals$`solicitation id` %in% lookup$`solicitation id`)

print("For solicitations not in nasaprograminsights::lookup table, I am inferring the solicitation number...")
for(i in seq_along(toadd)){ # shoudl reduce for comp time.
  if(i==1 & !"solicitation number" %in% tolower(colnames(proposals))) proposals$`solicitation number` <- NA
  new <- unique(lookup$`solicitation number`[which(lookup$`solicitation id` == proposals$`solicitation id`[toadd[i]])])
  proposals[toadd[i],"solicitation number"] <- new
}
rm(toinfer, toadd)

tochg=which(is.na(proposals$`solicitation number`))
proposals[tochg, "solicitation number"] <- substr(x = proposals[tochg, "solicitation id"],
              start = 1,
              stop = nchar(proposals[tochg, "solicitation id"]) - 2)

letters=stringr::str_extract(proposals[tochg, "solicitation number"], "(?<=-).*")
yy=stringr::str_extract(proposals[tochg, "solicitation number"], "[0-9]+")
proposals[tochg, "solicitation number"]<- paste0("NNH", yy, "ZDA001N-",letters)
rm(yy, letters, tochg)


#!!# HANDLE SPECIAL CASES
# "Decisions/05" # but maybe keep because thats how ASP mapper has it..
# 2-step proposals
# ecostres vs ecostress


if ("sequence number" %in% colnames(proposals))
  proposals <- proposals |> dplyr::select(-`sequence number`)

# organize columns for easy use
proposals <- proposals |> dplyr::relocate("solicitation number", "solicitation id", "proposal number")

# Troubleshooting ---------------------------------------------------------
temp <- lookup |> dplyr::filter(`was solicited`  %in% c( TRUE, NA)); 
tofix=temp$`solicitation number`[which(!temp$`solicitation number` %in%  proposals$`solicitation number`)]#not in the proposals dataframe or is incorrectly mapped
x=allfns[sort(unlist(lapply(X =tofix, FUN= function(x){grepl(x, allfns) |> which()})))]
y=!(grepl(pattern = "noi" ,x=x, ignore.case=TRUE))

if(length(y > 0))
  warning(
    "the following solicitations may not have been imported to the ",
    paste(allfns[y], collapse = "\n")
  )
rm(temp, tofix,x,y)


# Filter Proposals --------------------------------------------------------
# If indicated, filter the proposals using tokeep
if(!is.null(tokeep)){
  print("Filtering proposals to include only:")
  print(tokeep)
  proposals <- proposals |> 
    dplyr::filter(tolower(`proposal status`) %in% tolower(tokeep))
}

# Add Program Name  -------------------------------------------------------
if(addprogramname){
print("adding program name ")
  proposals <-
    dplyr::full_join(
      x = proposals,
      y = nasaprograminsights::lookup,
      # by = c("solicitation number"),
      relationship = "many-to-many"
    ) |> 
    dplyr::relocate("program name")
  
  stopifnot("program name" %in% tolower(colnames(proposals)))
  }

# Import People Data --------------------------------------------
# i should probably move out of data.table
print("importing people data...")
people <- lapply(pplfns, data.table::fread) |>
  data.table::rbindlist(fill=TRUE) |> 
  as.data.frame() |> 
  dplyr::select(-`Response seq number`)
colnames(people) <- tolower(colnames(people))
if(any(duplicated(colnames(people)))){
  people <- people |> dplyr::distinct(`pi suid`,`member suid`,`response number`, .keep_all = TRUE)#, `pi first`, `pi last`)
}

# Munge People Data -------------------------------------------------------
## just ensure proposal status and proposal number are in df, since that's what theyre called in the propsoals df (above)
if(("proposal status" %in% names(people)) &
   ("status" %in% names(people)))
  people <- people |> dplyr::select(-'proposal status') |> dplyr::rename("proposal status" = "status")
if (("proposal number" %in% names(people)) &
    ("response number" %in% names(people)))
  people <- people |> dplyr::select(-'proposal number') |> dplyr::rename('proposal number' = 'response number')
if(removeppl){
  people <- people |> dplyr::filter(`proposal number` %in% proposals$`proposal number`)
}

# people$'proposal number'[which(!people$`proposal number` %in% proposals$`proposal number`)] ## THIS SHOUDL BE ZERO...

# add solicitation id to people
people <- dplyr::left_join(people, proposals |> dplyr::select(`solicitation id`, `proposal number`),by="proposal number", 
                         relationship="many-to-many") |> 
  dplyr::filter(!is.na("proposal number")) |> dplyr::distinct(`proposal number`, `member suid`)


## slightly munge colnames of people
if(!all(proposals$`member suid` %in% proposals$`pi suid`))
  warning("FYI: not all PI SUIDs from `proposals` are in the `people` dataframe as part of output."
)


# Export Data Together to Package as "nspires"  -----------------------------------------------------------
# to be safe
if(dplyr::is_grouped_df(proposals)) proposals <- dplyr::ungroup(proposals) 
if(dplyr::is_grouped_df(people)) people <- dplyr::ungroup(people) 

nspires <- list()
nspires$proposals <- as.data.frame(proposals)
nspires$people <- as.data.frame(people)
nspires$lookup <- as.data.frame(nasaprograminsights::lookup)

print("Exporting NSPIRES proposals, people, and a solicitations lookup table in a single list. ")

return(nspires)
}

