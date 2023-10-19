# Setup -------------------------------------------------------------------
devtools::install_github(repo="NASA-Ecological-Conservation/nasaprograminsights")

# Make df from NSPIRES Internal Data ---------------------------------------------------------------
datadir <- "/Users/jlburne3/Library/CloudStorage/OneDrive-NASA/bdecprogrameval/data-raw/"
nspires <- nasaprograminsights::make_nspires(dir=datadir)


# Explore proposal data ------------------------------------------------------------
# nspires$proposals[1:10,1:10]

# solicitations lookup table
list.files(pattern="lookup", recursive=TRUE)
flookup <- list.files(datadir, pattern = "solicitations_lookup",  full.names = TRUE, recursive=TRUE); stopifnot(length(flookup)==1)
list.files(datadir, recursive=T, full.names=T, pattern="lookup")
solslookup <- read.csv(flookup) |> 
  mutate(total_funds_avail = as.numeric(total_funds_avail)) |> 
  mutate(annual_funds_avail = as.numeric(annual_funds_avail)) |> 
  mutate(filescaptured = as.logical(filescaptured)) |> 
  mutate(acronym = as.factor(acronym)) |> 
  mutate(solicitation.number = as.factor(solicitation.number)) |> 
  mutate(max_performance_years = as.integer(max_performance_years))  |> 
  mutate(num_selected = as.integer(num_selected)) 