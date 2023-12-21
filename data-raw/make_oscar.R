#' @name oscar
#' @description Code to prepare a lookup table comprising Oscar data for satellites and instruments. 
#' @docType data
#' @export
# Import Oscar Satellites and Instrument Downloads ------------------------------------------------------------
fns <- tolower(list.files(path="data-raw", pattern= "oscar", ignore.case = TRUE, full.names = TRUE))
stopifnot(length(fns)>0)

sats <- fns[grepl(x=fns,"satellite", ignore.case = TRUE)]
instruments <- fns[grepl(x=fns,"instruments", ignore.case = TRUE)]
stopifnot(length(sats)>0 | length(instruments)>0)

## grab the most recent DL of each sat and instruments
satfn  <- sats[which.max(lubridate::ymd(basename(sats)))]
instfn <- instruments[which.max(lubridate::ymd(basename(instruments)))]


instruments <- readxl::read_excel(instfn)
sats        <- readxl::read_excel(satfn)

rm(satfn, instfn, fns)

# Munge the Satellites DF --------
agencies <- sort(unique(instruments$`Space Agency`))
newcols <- paste0("agency",1:length(agencies))

## this is still experimental, so fingers crossed that it stays around...
sats.temp <- tidyr::separate_wider_delim(data = sats, cols = "Agencies", delim = "\n",
                                   names = newcols,
                                   too_few="align_start")
sats.long <- sats.temp |> tidyr::pivot_longer(cols=newcols, names_to="temp", values_to = "Space Agency") |> 
  dplyr::select(-"temp") |> 
  dplyr::relocate("Space Agency") |> 
  dplyr::filter(!`Space Agency` %in% c("NA", NA, "", " "))
# i changed colname to Space Agency because that's what the instruments df has.

# Munge the Instruments ---------------------------------------------------
## doesn't need any , thank F



# Save Data to Pkg --------------------------------------------------------
oscar<-list()
oscar$satellites <- sats
oscar$instruments <- instruments
usethis::use_data(oscar, overwrite = TRUE)
