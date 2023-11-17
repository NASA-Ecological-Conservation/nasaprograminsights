#' @name solicitationslookup
#' @description
#' @docType data
#' @export
#' A lookup table for ESD and ASP solicitations to easily index solicitations and proposals according to program name.
#' 
#' There is a lookup table obtained from the public-facing NASA ASP mapper. Projects beyond the scope of the Earth Action/Applied Sciences Program mapper/webiste or projects not included in that looku are supplemented via a handmade lookup table, avoiding duplication or overwriting.

### Read in the file(s) from the EA/ASP mapper website (URL tba)
fn <- list.files(pattern="asp-solicitation", full.names=TRUE, recursive=TRUE)
stopifnot(length(fn)>0)
names <- readxl::excel_sheets(fn)

for(i in seq_along(names)){
  if(i == 1) lookup <- NULL
  (if(tolower(names[i]) %in% c("readme", "read me")) next())
  temp <- readxl::read_xlsx(fn, sheet = names[i]) |> 
    dplyr::mutate(`Program Name` = names[i])
  lookup <- dplyr::bind_rows(temp, lookup)
  print(names[i])
  rm(temp)
}
names(lookup) <- tolower(names(lookup))

lookup <- lookup |>
  dplyr::rename("solicitation number" = 'nra number') |> 
  as.data.frame() |> 
  dplyr::select("solicitation title", "solicitation number", "nra year", "program name")

## Supplement this,where necessary, with a hand-made lookup table...
fn <-list.files(pattern = "handmade-solicitation-lookup", full.names=TRUE, recursive=TRUE); stopifnot(length(fn)>0)
supp <- read.csv(fn)


## the NSPIRES exported data does NOT include the NRA number, so make an id to mesh with the solicitaitons lookup -- anoying
# we need this because we need this to link from programs to proposals
# this solution is terrible but whatever for now
# needs to be 21-SERVIR21
prog <- trimws(sub(".*\\-", "", x = lookup$`solicitation number`)) # extract portion of new var

# Extract last two digits of yr 
yr <- trimws(substr(lookup$`nra year`, nchar(lookup$`nra year`) - 2 + 1, nchar(lookup$`nra year`)))

lookup$'solicitation id' <- paste0(yr, "-", prog, yr)

## export to package
usethis::use_data(lookup, overwrite=TRUE)
