#' @name lookup
#' @description
#' @docType data
#' @export
#' A lookup table for ESD and ASP solicitations to easily index solicitations and proposals according to program name.
#' Data obtained from the ASP mapper and a hand-made table.  

# Import & Munge ASP Mapper Data -------------------------------------------------------------
## Read in the file(s) from the EA/ASP mapper website (URL tba)
fn <- list.files(pattern="asp-solicitation", full.names=TRUE, recursive=TRUE)
stopifnot(length(fn)>0)
names <- readxl::excel_sheets(fn)
for(i in seq_along(names)){
  if(i == 1) mapper <- NULL
  (if(tolower(names[i]) %in% c("readme", "read me")) next())
  temp <- readxl::read_xlsx(fn, sheet = names[i]) |> 
    dplyr::mutate(`Program Name` = names[i])
  mapper <- dplyr::bind_rows(temp, mapper)
  print(names[i])
  rm(temp)
}
names(mapper) <- tolower(names(mapper))
mapper <- mapper |>
  dplyr::rename("solicitation number" = 'nra number') |> 
  as.data.frame() |> 
  dplyr::select("solicitation title", "solicitation number", "nra year", "program name")
rm(fn, names)

# Make a Solicitation Identifier on `mapper`  to Merge with NSPIRES Data -----------------------
## the NSPIRES exported data does NOT include the NRA number, so make an id to mesh with the solicitaitons lookup -- anoying
# we need this because we need this to link from programs to proposals
# this solution is terrible but whatever for now
prog <-
  trimws(sub(".*\\-", "", x = mapper$`solicitation number`)) # extract portion of new var
yr <-
  trimws(substr(
    mapper$`nra year`,
    nchar(mapper$`nra year`) - 2 + 1,
    nchar(mapper$`nra year`)
  )) # extract last two digits of yr
# Make  a solicitation identifier variable
mapper$'solicitation id' <- paste0(yr, "-", prog, yr)
rm(yr, prog)

# Import Handmade Lookup Table -------------------------------------------------
## Read in handmade lookup table (because ASP mapper data is only for ASP/EA)
fn <-list.files(pattern = "handmade-solicitation-lookup", full.names=TRUE, recursive=TRUE); stopifnot(length(fn)>0)
supp <- read.csv(fn, na.strings = c(NA,"", "NA"))
rm(fn)
names(supp) <- tolower(gsub("\\.", " ", names(supp)))
supp <- supp[,c('nra year','solicitation id', 'solicitation id corrected','program name','was solicited','solicitation number')]

# Correct Special Case Inferred Solicitaiton IDs ----- 
## the supp data frame has the 'solicitation id corrected', so we need to change 
## `solicitation id` in `mapper` where theres a correction
tofix <- supp[which(nchar(supp$`solicitation id corrected`)>0),c("solicitation id", "solicitation id corrected")]
for(i in 1:nrow(tofix)){ # yes, this is a rachet way but whatever for now i just need a solution
  old=tofix[i,"solicitation id"]
  new=supp$`solicitation id corrected`[which(supp$`solicitation id` %in% old)]
  rowtochg=which(mapper$`solicitation id` %in% old)
  for(j in seq_along(rowtochg)){
    mapper$`solicitation id`[rowtochg[j]] <- new
  }
  rm(old,new,rowtochg)
} # end nasty for loop 

# Move anything over to the 'corrected column for join below:
tochg=which(is.na(supp$`solicitation id corrected`))
supp$`solicitation id corrected`[tochg] <- supp$`solicitation id`[tochg]

# Combine the Two DFs into a single Data Frame  ---------------------------------------------------------
lookup <-
  dplyr::full_join(mapper, supp, 
                   by = c("solicitation id"="solicitation id corrected"), 
                   keep=FALSE,
                   na_matches="na") |> 
  dplyr::mutate("solicitation number" = dplyr::coalesce(`solicitation number.x`, `solicitation number.y`)) |> 
  dplyr::mutate("program name" = dplyr::coalesce(`program name.x`, `program name.y`)) |> 
  dplyr::mutate("nra year" = dplyr::coalesce(`nra year.x`, `nra year.y`)) |> 
  dplyr::select(-c(`solicitation id.y`,
                  `solicitation number.x`,`solicitation number.y`,
                    `program name.x`, `program name.y`, 
                  `nra year.x`, `nra year.y`)) |> 
  dplyr::relocate(`solicitation title`, .after=last_col())


# Export Data to Pkg ------------------------------------------------------
usethis::use_data(lookup, overwrite=TRUE)


