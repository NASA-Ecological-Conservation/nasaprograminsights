#' @name solicitationslookup
#' @description
#' A 
#' 
#' @docType data
#' 

flookup <- list.files(pattern = "solicitations_lookup",  full.names = TRUE, recursive=TRUE); stopifnot(length(flookup)==1)

solslookup <- read.csv(flookup) |> 
  dplyr::mutate(total_funds_avail = as.numeric(total_funds_avail)) |> 
  dplyr::mutate(annual_funds_avail = as.numeric(annual_funds_avail)) |> 
  dplyr::mutate(acronym = as.factor(acronym)) |> 
  dplyr::mutate(solicitation.number = as.factor(solicitation.number)) |> 
  dplyr::mutate(max_performance_years = as.integer(max_performance_years))  |> 
  dplyr::mutate(num_selected = as.integer(num_selected)) 

