#' @name solicitationslookup
#' @description
#' A lookup table for ESD and ASP solicitations to easily index solicitations and proposals according to program name.
#' 
#' @docType data
#' 

fn <- list.files(path="data-raw", pattern="ESD-ASP-solicitations-lookup", full.names=TRUE, recursive=TRUE)
names <- readxl::excel_sheets(fn)

for(i in seq_along(names)){
  temp <- readxl::read_xlsx(fn, sheet = names[i]) |> 
    dplyr::mutate(`Program Name` = names[i])
  if(i == 1) lookup <- temp
  lookup <- dplyr::bind_rows(temp)
  rm(temp)
}

## munge colnames
names(lookup) <- tolower(names(lookup))

looku <- lookup |>
  dplyr::rename("proposal number" = 'proposal no')


usethis::use_data(lookup)


