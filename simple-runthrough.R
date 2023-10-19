# Setup -------------------------------------------------------------------
devtools::install_github(repo="NASA-Ecological-Conservation/nasaprograminsights/")

# Make df from NSPIRES Internal Data ---------------------------------------------------------------
nspires <- make_nspires(dir="data-raw/data-raw-internal/nspires-internal")

# Explore proposal data ------------------------------------------------------------
