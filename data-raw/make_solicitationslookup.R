#' @name solicitationslookup
#' @description
#' A lookup table for ESD and ASP solicitations to easily index solicitations and proposals according to program name.
#' 
#' @docType data
#'
#'
#'

fn <- list.files(pattern="asp-solicitation", full.names=TRUE, recursive=TRUE)
stopifnot(length(fn)>0)
names <- readxl::excel_sheets(fn)

for(i in seq_along(names)){
  temp <- readxl::read_xlsx(fn, sheet = names[i]) |> 
    dplyr::mutate(`Program Name` = names[i])
  if(i == 1) sols_lookup <- temp
  sols_lookup <- dplyr::bind_rows(temp)
  rm(temp)
}

## munge colnames
names(sols_lookup) <- tolower(names(sols_lookup))

sols_lookup <- sols_lookup |>
  dplyr::rename("proposal number" = 'proposal no')


usethis::use_data(sols_lookup)


