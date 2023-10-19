# this script is draft to grab the grant ID numbers from individual
# searches conducted in the NASA grant status form
## LAST UPDATE: sometime around ....December 2022?
## wegbpage: https://www.nasa.gov/centers/nssc/forms/grant-status-form
rm(list=ls())


# Import Grant Status Form Search Results ---------------------------------
fns <-
  list.files(
    "grantstatus",
    full.names = TRUE,
    recursive = TRUE
  )
list.files(fns, recursive = TRUE, full.names=TRUE)
fns
##note: this data is so shitty because its exported as a fucking csv and THEN
##whoever designed this thought, "hey, let's make a csv with a bunch of fucking commas in the fields"
## so basically, I am going to import all the files into a list to diagnose the issues. 
## then, i will grab those elements and ignore on the rows upon a second import....
## yes, this is gross, but whatever

## Diagnose File Issues (csv errors)
for (i in seq_along(fns)) {
  if (i == 1) mylist <- list()
  mylist[[i]] <- read_csv(fns[i], quote="\"'")
  ## convert the dates to characters just to be safe....
  
  if(i==1)issues <-  problems(mylist[[i]]) |> mutate( "element"=i) |> select(element,everything())
  if(i>1) issues <- dplyr::bind_rows(issues, problems(mylist[[i]]) |> mutate( "element"=i) |> select(element,everything()))
}

datecols <- c("Award Date", "Performance End Date", "Performance Start Date")
skipped <- rowstoremove <- NULL
for(i in 1:nrow(issues)){
  c=as.numeric(issues[i, "col"])
  r=as.numeric(issues[i, "row"])
  e=as.numeric(issues[i, "element"])
  if(c > ncol(mylist[[e]])){
    skipped <- c(i, skipped)
    next()
  } 
  val=mylist[[e]][r,c]
  name=names(val)
  # check to see if issue resolved itself. if so, we will remove from our list of issues. 
  if(names(val) %in%  datecols){
  if(eval(parse(text=paste0("class(val$'", name,"')")))=="Date") rowstoremove <- c(i, rowstoremove)
  } 
}

mylist[[issues$element[1]]][issues$row[1],] |> View()
skipped
# if(!is.null(rowstoremove)) issues <- issues[-c(rowstoremove, skipped), ] # keep outside loop

## Munge the date columns
for(i in seq_along(mylist)){
  for(j in seq_along(datecols)){
   # print(paste0(i, ",",j))
    ind <- which(names(mylist[[i]]) == datecols[j])
    # olddat <- 
    mylist[[i]][,ind] <- as.Date(as.data.frame(mylist[[i]])[,ind], "%Y-%m-%d") 
   }
}

grantstatusform <- bind_rows(mylist)
View(grantstatusform)
## Next, append information to all of Keith's grants. 

## Munge the column data a little
ind.col <-
  c(
    "Proposal Title",
    "Program Title",
    "NASA Center",
    "Principal Investigator Name",
    "Technical Officer"
  )
for(i in seq_along(ind.col)){
  temp <- which(names(grantstatusform) %in% ind.col[i])
  grantstatusform[temp] <- toupper(eval(parse(text=paste0("grantstatusform$'", ind.col[i], "'"))))
  grantstatusform[temp] <- toupper(eval(parse(text=paste0("grantstatusform$'", ind.col[i], "'"))))
  rm(temp)
}
# dont remember why i put this but whatever.
any(grepl("esat",x = grantstatusform$`Grant Number`, ignore.case=TRUE))

##defkeep==a hand-curated list of grants definitely managed by BD/EC
 ## so, things in here we KNOW  we want to track...
grantslookup <- read.csv(list.files(pattern="grantnumber_lookup", recursive = TRUE))

wossearch <- paste0("ALL=('", paste(as.vector(grantslookup$grant.number[1:50]), collapse = "' OR '"),"')")
writeClipboard(wossearch)
wossearch <- paste0("ALL=('", paste(as.vector(grantslookup$grant.number[51:100]), collapse = "' OR '"),"')")
writeClipboard(wossearch)
wossearch <- paste0("ALL=('", paste(as.vector(grantslookup$grant.number[101:150]), collapse = "' OR '"),"')")
writeClipboard(wossearch)
wossearch <- paste0("ALL=('", paste(as.vector(grantslookup$grant.number[151:200]), collapse = "' OR '"),"')")
writeClipboard(wossearch)

setdiff(grantstatusform$`Grant Number`,grantslookup$grant.number)
setdiff(grantslookup$grant.number, grantstatusform$`Grant Number`)


# Filter Awards by Tech. Officer, Programs, Proposal Titles ----------------------------------------------------
key.to <- c("KEITH GADDIS", "WOODY TURNER", "WILLIAM W TURNER", "WILLIAM WOODY TURNER")
key.program <- paste("ECOLOGICAL FORECASTING", "ECOLOGICAL CONSERVATION", "BIODIVERSITY", "BIOLOGICAL DIVERSITY", "ECOSTRESS", sep="|")


temp <- grantstatusform |>
  dplyr::filter('Technical Officer' %in% key.to |
                  stringr::str_detect('Program Title', key.program)) |>
  dplyr::select('Grant Number')
grantnumstokeep <- unique(c(grantslookup$grant.number, temp$`Grant Number`))


grantnumstokeep <- c(which(!tokeep$grant.number %in% grantnumstokeep),grantnumstokeep)
stopifnot(all(grantnumstokeep %in% grants$grant.number))
setdiff(grantnumstokeep,grants$grant.number)
setdiff(grants$grant.number,grantnumstokeep)

## tofilter are ones I need to ensure should/not be included in the filtering.
### tofilter also has quite a few "William Turners," which we need to figure out how to remove...
tofilter <- grants |>
  filter(!grant.number %in% grantnumstokeep) |> 
  filter(!str_detect(proposal.title, "AAAS SCIENCE")) #remove AAAS STP fellowship

# grants$grant.number
t=paste0(tempfile(), "tofilter.csv")
write.csv(tofilter, t)
browseURL(t)

# grants$grant.number
t2=paste0(tempfile(), "tokeep.csv")
write.csv(tokeep, t2)
browseURL(t2)


# .bib
## pull in wos searches
bibfns <- list.files(pattern = ".bib", recursive = TRUE, full.names=TRUE)
bib <- lapply(bibfns, revtools::read_bibliography) |> bind_rows()
write.csv(bib, "data/")
bib #|> filter(year > 2017) |> distinct(doi) |> nrow()

## note, before importing, on 20230106 i had to go into the cce .bib file and edit the "Future of Bluefin Tunas' book author field to Barabara Block
ccebib <-
  bibliometrix::convert2df(list.files(
    pattern = "cce",
    recursive = TRUE,
    full.names = TRUE
  )[2])

