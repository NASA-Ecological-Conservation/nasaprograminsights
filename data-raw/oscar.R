## code to prepare `oscar` instruments and satellites lookup table here



# Import Oscar x2 ------------------------------------------------------------
fns <- tolower(list.files(path="data-raw/data-raw-public", pattern= "oscar", ignore.case = TRUE, full.names = TRUE))


instruments <- readxl::read_excel(fns[grepl(pattern="instrument", x = fns)])
sats        <- readxl::read_excel(fns[grepl(pattern="satel", x = fns)])
## TODO: split the sats$Agenies into multiples (separated by "\n")
## TODO: split the instruments$`Space Agency` into multiples (separated by "\n")

nasasats <- strd[sats$`Space Agency`=="NASA", ]
nasainst <- instruments[instruments$`Space Agency`=="NASA", ]

instrtemp <- unlist(strsplit(nasainst$Satellites[263], "\n"))
instrtemp <- instrtemp[instrtemp != ""]


# Save Data to Pkg --------------------------------------------------------
usethis::use_data(oscar, overwrite = TRUE)
