#' @name lookup
#' @description
#' @docType data
#' @export
#' A lookup table for ESD and ASP solicitations to easily index solicitations and proposals according to program name.
#' Data obtained from the ASP mapper and a hand-made table.  

# Import & Munge ASP Mapper Data -------------------------------------------------------------
## Read in the file(s) from the EA/ASP mapper website (download and import data from source: https://cce-dev.gsfc.nasa.gov/mapper/dev/index.html?progid=15)
### that is, go to https://cce-dev.gsfc.nasa.gov/mapper/dev/index.html?progid=15 (i asked developers for a static url to avoid manual download)
### select "All Rows", then click  (lol) the "Download button" (down arrow) on RHS
# list.files("data-raw")
(fn <- list.files(pattern="esd_asp_solicitations", full.names=TRUE, recursive=TRUE))
stopifnot(length(fn)>0)

## THE IMPORT NEEDS TO BE FIXED:
### curently (20240617) the original data file has a spurious and unnecessary first row. need to skip based on a function, NOT saying,
### "import rows 2:N" -- e.g., any row where "Questions?" exists, skip it or read anything after;
## IE FIX LINE 21-23

mapper <- readxl::read_excel(fn)
colnames(mapper) <- mapper[1,]
mapper <- mapper[-1, ] # removing shitty rownames row
  # dplyr::mutate(`Program` = names[i])

names(mapper) <- tolower(names(mapper)) # i like to force UPPER or force lower for convenience and avoid capitalization errors.

## I guess they deleted NRA number...
# mapper <- mapper |>
#   dplyr::rename("solicitation number" = 'nra number') |> 
#   as.data.frame() |> 
#   dplyr::select("solicitation title", "solicitation number", "nra year", "program name")
# rm(fn, names)

# Make a Solicitation Identifier on `mapper`  to Merge with NSPIRES Data -----------------------
## the NSPIRES exported data does NOT include the NRA number, so make an id to mesh with the solicitations lookup -- anoying
# we need this because we need this to link from programs to proposals
# this solution is terrible but whatever for now
### they also removed nra number (i.e. solicitation number) such that we cannot easily index. This data was vital to required for downstream code. Therefore, for now, I applied a workaround below
# prog <-
#   trimws(sub(".*\\-", "", x = mapper$`solicitation number`)) # extract portion of new var
# 43- is the workaround to 40-41

(prog <- sub(".*\\(", "", mapper$`project`) |> stringr::str_replace_all("[^[:alpha:]]", "")) 
yr <- gsub(pattern = "\\D",replacement =  "",x = mapper$project); yr <- stringr::str_sub(yr, -2)

mapper$'solicitation id' <- paste0(yr, "-", prog, yr)
rm(yr, prog)

# Import Handmade Lookup Table -------------------------------------------------
## Read in handmade lookup table (because ASP mapper data is only for ASP/EA)
fn <-list.files(pattern = "handmade-solicitation-lookup", full.names=TRUE, recursive=TRUE); stopifnot(length(fn)>0)
supp <- read.csv(fn, na.strings = c(NA,"", "NA"))
rm(fn)
names(supp) <- tolower(gsub("\\.", " ", names(supp)))
# supp <- supp[,c('nra year','solicitation id', 'solicitation id corrected','program name','was solicited','solicitation number')]

# Correct Special Case Inferred Solicitation IDs ---------
## the supp data frame has the 'solicitation id corrected', so we need to change 
## `solicitation id` in `mapper` where theres a correction
temp <- supp[which(nchar(supp$`solicitation id corrected`)>0),c("solicitation id", "solicitation id corrected")]
tofix <- temp[which(!temp$`solicitation id`==temp$`solicitation id corrected`),] 
receiver <- which(supp$`solicitation id` %in% tofix$`solicitation id`)
supp$`solicitation id`[receiver] <- tofix$`solicitation id corrected`
rm(receiver, temp, tofix)

# Combine the Handmade lookup (supp) + ASP mapper (mapper) data frames---------------------------------------------------------
# View(merge(mapper, supp))
names(mapper)[which(names(mapper) %in% names(supp))]
names(supp)[which(names(supp) %in% names(mapper))]
merge(mapper |> dplyr::select(data), supp)
lookup <-
  dplyr::full_join(x = mapper, by = supp, join_by = c("solicitation id", "nra year"))
  dplyr::full_join(mapper, supp,
                   # by = c("solicitation id"="solicitation id corrected"), 
                   keep=FALSE,
                   na_matches="na")

setdiff(mapper$`solicitation id` ,supp$`solicitation id corrected`) |> sort()
setdiff(supp$`solicitation id corrected`, mapper$`solicitation id` ) |> sort()


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


