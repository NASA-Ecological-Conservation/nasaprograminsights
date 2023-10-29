# Setup -------------------------------------------------------------------
devtools::install_github(repo="NASA-Ecological-Conservation/nasaprograminsights")

# Make df from NSPIRES Internal Data ---------------------------------------------------------------
datadir <- "/Users/jlburne3/Library/CloudStorage/OneDrive-NASA/bdecprogrameval/data-raw/" # where is the internal data stored
nspires <- nasaprograminsights::make_nspires(dir=datadir, removeppl = TRUE, tokeep=c("selected", "declined", "submitted","selectable","invited","awarded"))
    # nspires contains 3 elements (data.frames): 
    ##    lookup table to link program names and NOFOs
    ##    people: people listed on proposals
    ##    proposals: 

nspires$proposals <- nspires$proposals |> dplyr::filter(tolower(`proposal status`) %in% tolower(tokeep))



# Inspect Data ------------------------------------------------------------
# here is a list of EA programs
(programs <- nspires$lookup$`program name` |> unique() |> sort())
# here is a list of the programs for which we have proposal data:
nspires$proposals[!nspires$proposals$`solicitation id` %in% nspires$lookup$`solciitation id`,] 

## STOPPED HERE
  
trimws(nspires$proposals$`solicitation id`)
# target <-
#   nspires$lookup$`solciitation id`[which(grepl(
#     nspires$lookup$`program name`,
#     pattern = "conservation|ec",
#     ignore.case = TRUE
#   )
#   

# Subset Data -------------------------------------------------------------
## let's build a simple function to return  a reduced data set that can (a) be used for export or table generation purposees and (b) to input quickly into plotting features.

# Explore proposal data ------------------------------------------------------------
# head(nspires$proposals, 2)[1:10]
# head(nspires$people, 2)
# head(nspires$lookup, 2)

