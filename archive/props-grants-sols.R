# THIS IS DEPRECATED -- THERE ARE INTERNAL FUNCTIONS THAT CREATE THE  DATASETS..

rm(list=ls())
devtools::load_all() # i.e. source all the pkg functions, munged data

# Setup -------------------------------------------------------------------
## RAW DATA DIR
dirdata <- list.files(pattern = "data-raw", full.names = TRUE)#, recursive=TRUE)#dir
dirdata.internal <-
stopifnot(length(dirdata) > 0)

## LOOKUP TABLES DIR
dirdata.lookups <- list.files(path=dirdata, pattern="data-raw-interla", recursive=TRUE)
dirdata.public <- list.files(path=dirdata, pattern="public")


## INTERNAL DATA DIRS
## this data is protected, never share or export off of NASA networks.
# specify the directory containing the .csv data imported from Internal NSPIRES.
dirdata.internal <- list.files(path=dirdata, pattern="-internal", full.names = TRUE)
dirdata.nspires  <- list.files(path=dirdata.internal, pattern="nspires", full.names=TRUE)


# Data: Grant Status Form (public-facing) -------------------------------------------------------------
## These files were hand-munged b/c the grant status form only exports as .csv and the exports are totally fuked.

grants <-
  lapply(
    fns, ### NEED TO FIX THIS
    data.table::fread,
    na.strings = c("N/A", NA_character_, "NA", "(N/A)"),
    colClasses = c("Award Date" = "character")
  ) |>
  data.table::rbindlist()


# Data: BDEC Grant Numbers Lookup ------------------------------------------------------------------
bdecgrantnos <-
  data.table::fread(
    list.files(
      dirdata,
      pattern = "bdec_grantnumber_lookup",
      recursive = TRUE,
      full.names = TRUE
    )
  ) # not sure why this is failing
# the following are in our lookup table but are NOT in the grant status form
## this is likely because they were grants that went TO NASA centers...
## this is what KG said, but not sure that's right because
## there are plenty of center-awarded grants (e.g., Fatoyinbo/GSFC) that ARE in the grant status form..
bdecgrantnos[which(!bdecgrantnos$grant.number %in% grants$`Grant Number`)] |> View()

# awardsinbdecprojectsworksheet <- read.table("clipboard",sep=",")
missing <- awardsinbdecprojectsworksheet[which(!as.character(awardsinbdecprojectsworksheet$V1) %in% c(grants$`Grant Number`, bdecgrantnos$grant.number)),]

# these are NSF co-funded awards that will be listed as NSF grants.
nsfawards <- c("1342787", "1342787", "1342872", "1343578", "1240804", "1241066")

# These are RTOPs assoc. with BDEC projects but without award ids. In case we find a way to track
# RTOPs <- c("281945.02.03.09.34",  "389018.02.12.01.92")
