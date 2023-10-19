# Setup -------------------------------------------------------------------
devtools::install_github(repo="NASA-Ecological-Conservation/nasaprograminsights")

# Make df from NSPIRES Internal Data ---------------------------------------------------------------
datadir <- "/Users/jlburne3/Library/CloudStorage/OneDrive-NASA/bdecprogrameval/data-raw/" # where is the internal data stored
nspires <- make_nspires(dir=datadir)

# Explore proposal data ------------------------------------------------------------
cols<-c("year", "proposal number", "status", "solicitation id")
nspires$proposals$`budget year   1`
which(cols %in% tolower(names(nspires$proposals)))
plotdf <- nspires$proposals |> 
  dplyr::select(year, proposal.number, 
                title, summary, proposal.status,solicitation.number,
                proposed.amount.total, agency.name, other.agency) |> 
  dplyr::group_by(year, proposal.status) |>
  dplyr::mutate(ndecisions=dplyr::n_distinct(proposal.number)) |>
  ungroup() |> 
  dplyr::group_by(year) |>
  dplyr::mutate(nsubmissions=dplyr::n_distinct(proposal.number)) |> 
  ungroup()
