#' @name solicitationslookup
#' @description
#' @docType data
#' A lookup table for ESD and ASP solicitations to easily index solicitations and proposals according to program name.

fn <- list.files(pattern="asp-solicitation", full.names=TRUE, recursive=TRUE)
stopifnot(length(fn)>0)
names <- readxl::excel_sheets(fn)

for(i in seq_along(names)){
  if(i == 1) sols_lookup <- NULL
  (if(tolower(names[i]) %in% c("readme", "read me")) next())
  temp <- readxl::read_xlsx(fn, sheet = names[i]) |> 
    dplyr::mutate(`Program Name` = names[i])
  sols_lookup <- dplyr::bind_rows(temp, sols_lookup)
  print(names[i])
  rm(temp)
}

## munge colnames
names(sols_lookup) <- tolower(names(sols_lookup))

sols_lookup <- sols_lookup |>
  dplyr::rename("solicitation number" = 'nra number') |> 
  as.data.frame() |> 
  dplyr::select("solicitation title", "solicitation number", "nra year", "program name")


## export to package
usethis::use_data(sols_lookup, overwrite=TRUE)

