# munge.nspires.internal.peopledata ---------------------------------------
#' @name munge.nspires.internal.peopledata
#' @description returns an object of class data.frame
#' @title Munge NSPIRES Internal People Data
#' @param df Data from the internal-access-only NSPIRES. Currently this function only really supports the "people" worksheets.
munge.nspires.internal.peopledata <- function(df){
      # df <- people ## for dev purposes
      df <- as.data.frame(df)
        # for some reason the "committed" date doesnt parse, no clue why because even the clearly "dmy" fields wont parse. whatever. drop the POS
        # the fields "PI Last Name and PI First Name" are redundant with the "Member Last and First, bevause they also have their own rows and their roles == PI
      dropcols <- which(tolower(colnames(df)) %in% c("pi last name", "pi first name", "committed", "response seq number"))


      ## why the hell arent these named the same thing!?!?!?!? THIS MAKSE ZERO SENSE!!!!
      # in proposal data, prop number =="Proposal Number"
      # in people data, prop number =="Response Number"
      colnames(df)[which(tolower(colnames(df))=="response number")] <- "Proposal Number"

}


# munge.nspires.internal.propsdata ----------------------------------------
#' @name munge.nspires.internal.propsdata
#' @title Munge NSPIRES Internal Proposal Data
#' @description returns an object of class data.frame
#' @param df Data from the internal-access-only NSPIRES. Currently this function only really supports the "propals" worksheets.
munge.nspires.internal.propsdata <- function(df) {
  # df=props # for dev
  df <- as.data.frame(df)

  ###handle the fields named "Question N:". Many of them are the same, but have differnet question numbers. So let's deal with that first.
  colnames(df) <-
    trimws(
      gsub(
        pattern = "\\.$|\\:$|\\?$|^question [0-9]:|^question [0-9][0-9][0-9]:|^question [0-9][0-9]:|\\()$|:$|\\(Check.*|\\(maximum.*|\\(including specific.*|\\(Select.*|\\(which.*",
        x = colnames(df),
        replacement = "",
        ignore.case = TRUE
      )
    )

df <- deduplicate(df)

##### ADD UNCTION RUN HERE this adds a lot of redundancy
  # View(as.data.frame(sort(colnames(df))))
  ## fix some one-off minor bullshit export controlled
  colnames(df) <- gsub(x=colnames(df), pattern="\n", ignore.case=TRUE, replacement= "")
  colnames(df) <- gsub(x=colnames(df), pattern="territoriesexcluding", ignore.case=TRUE, replacement= "territories excluding")
  colnames(df) <- gsub(x=colnames(df), pattern="includes:a", ignore.case=TRUE, replacement= "includes: a")
  colnames(df) <- gsub(x=colnames(df), pattern="would:a", ignore.case=TRUE, replacement= "would: a")
  colnames(df) <- gsub(x=colnames(df), pattern="intothe", ignore.case=TRUE, replacement= "into the")
  colnames(df) <- gsub(x=colnames(df), pattern=", orb.", ignore.case=TRUE, replacement= ", or b.")
  colnames(df) <- gsub(x=colnames(df), pattern="orc.Outdoor d", ignore.case=TRUE, replacement= "or c.Outdoor d")
  colnames(df) <- gsub(x=colnames(df), pattern="would:a.R", ignore.case=TRUE, replacement= "would: a.R")
  colnames(df) <- gsub(x=colnames(df), pattern="oxidizers,solid", ignore.case=TRUE, replacement= "oxidizers, solid")
  colnames(df) <- gsub(x=colnames(df), pattern="launchvehicle", ignore.case=TRUE, replacement= "launch vehicle")
  colnames(df) <- gsub(x=colnames(df), pattern="export control", ignore.case=TRUE, replacement= "export-control")
  colnames(df) <- gsub(x=colnames(df), pattern="Areas of Support", ignore.case=TRUE, replacement= "Area of Support")
  colnames(df) <- gsub(x=colnames(df), pattern=" us ", ignore.case=TRUE, replacement= " U.S. ")
  colnames(df) <- gsub(x=colnames(df), pattern="Total Budget Request", ignore.case=TRUE, replacement= "Total requested")


  df <- deduplicate(df)

  ## deal with the "nearly similar" column names...
  old <-
    c(
      "NASA-provided high end computing",
      "When did you receive your Ph.D. Degree",
      "Would the proposal involve any field activity that would: a.Release equipment",
      "short title",
      "gender",
      "will any funding be provided to a federal government",
      "Name the NASA Earth science research results that the project",
      "When did you receive your Ph.D. Degree? (Month/Year)",
      "Would the proposal involve the launch of a payload, equipment, or instrument",
      "Would the proposal involve any activity that includes: a.Construction",
      "This proposal contains information and/or data that are subject to U.S. export-control laws and regulations including Export Administration Regulations"    )
  new <-
    c(
      "Does this proposal request use of NASA-provided high end computing (HEC)",
      "When did you receive your Ph.D.",
      "Would the proposal involve any field activity that would: a.Release equipment (e.g., dropsondes, sensors, etc.) or chemicals (e.g., dyes, tracers, etc.) into the air, bodies of water or on the ground, or b.Release a parachute or use equipment that would not be recovered, or c.Involve equipment or a payload that contains hazardous (e.g., petroleum, hypergols, oxidizers, solid propellants, etc.) or radioactive materials",
      "Short Title",
      "Reported gender",
      "Will any funding be provided to a federal government organization including NASA Centers, JPL, other Federal agencies, government laboratories, or Federally Funded Research and Development Centers (FFRDCs)?",
      "Name the NASA Earth science research results that the project will utilize",
      "When did you receive your Ph.D. Degree?",
      "Would the proposal involve the launch of a payload, equipment, or instrument",
      "Would the proposal involve any activity that includes: a.Construction of new facilities or modification to the footprint of an existing-facility, or b.Ground disturbance (e.g., excavation, clearing of trees, installation of equipment, etc.), or c.Outdoor discharges of water (e.g., waste water runoff), air emissions (e.g., ozone-depleting substances) or generation of noise exceeding 115 dBA (excluding those associated with aircraft operations)",
      "This proposal contains information and/or data that are subject to U.S. export-control laws and regulations including Export Administration Regulations (EAR) and International Traffic in Arms Regulations (ITAR)"
    )
  for(i in seq_along(new)){
# print(i)
        cols <- names(df)[which(grepl(pattern = tolower(old[i]), x=tolower(names(df))))]
    if(length(cols)==0)next()
    newcol <- trimws(dplyr::coalesce(!!!df |> dplyr::select(cols)))
    colstoremove <- which(colnames(df) %in% cols)
    df[,colstoremove] <- list(NULL)
    df[,eval(parse(text="new[i]"))] <- newcol
  }

df <- deduplicate(df)
# View(as.data.frame(sort(colnames(df), decreasing = TRUE)))

return(df)

} # END FUN


