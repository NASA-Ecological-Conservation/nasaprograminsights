#' @title Program Analysis Page UI
#' @description
#' Base UI function for Program Analysis page calling other UI functions
#' @usage
#' ui <- fluidPage(mainPanel(tabsetPanel(analysisUI())))
analysisUI <- function() {
  tabPanel("Program Analysis",
           analysis_intro(), # intro message
           analysis_dropdown()) # drop down to select a metric to evaluate the proposals by
}

#' @title Analysis Page Intro Text
#' @description
#' Output text introducing the user to how to use the drop down menu in the Program Analysis section of the app.
#' @usage
#' tabPanel("Program Analysis", analysis_intro())
analysis_intro <- function() {
  HTML(r"(
          <br>
          <h1>Program Analysis</h1>
          <p>This tab summarizes all NASA Earth Action program investments between 2006-2022. </p>
          <br>
          <p>The metric by which the proposals can be analyzed by can be selected from the dropdown menu below. These are the metrics: </p>
          <ol>
            <li>Proposal count</li>
            <li>Proposal award amount</li>
            <li>PI and member participation</li>
            <li>Linked organizations</li>
            <li>Proposal participation</li>
          </ol>
          )")
}

#' @title Program Analysis Drop Down and Plots UI
#' @description
#' Output drop down menu for user to select a metric to view and the associated calculations and plots for the metric.
#' @usage
#' tabPanel("Introduction & Setup", analysis_dropdown())
analysis_dropdown <- function() {
  metrics <- c("Proposal count", "Proposal award amount", "PI and member participation", "Linked organizations", "Proposal participation")
  fluidPage (
    selectInput("all_dropdown", "Select a view", metrics),
    htmlOutput("all_result"),
    plotlyOutput("filtered_vs_other"),
    plotlyOutput("filtered_vs_other2"),
    plotlyOutput("filtered_vs_other3"),
    plotlyOutput("filtered_vs_other4")
  )
}


#' @title Add a Tag for CESU Member or Not
#' @description This is in helper because it's of limited value. 
add.cesu.tag <- function(dat.nspires, cesu=nasaprograminsights::cesu) {
  nasa.cesu <-
    cesu[cesu$funder == "National Aeronautics and Space Administration", ]

    # dat.nspires$`pi company name` <-
  #   toupper(dat.nspires$`pi company name`)

  # add index tos kip any NA or blank compnay names
  is.blank <- which(dat.nspires$`pi company name` %in% c("NA", NA, "N/A", ""))
  
  dat.nspires$nasa.member.cesu = FALSE
  dat.nspires$member.cesu = FALSE
  
  for (i in 1:nrow(dat.nspires)) {
    if(i %in% is.blank) next()
    
    ## for cesus that nasa is already in
   if(any(grepl(tolower(dat.nspires$`pi company name`[i]), x=nasa.cesu$member))){
     dat.nspires$nasa.member.cesu[i]=TRUE
   }
    
    ## for all cesu members
    if(any(grepl(tolower(dat.nspires$`pi company name`[i]), x=cesu$member))){
      dat.nspires$member.cesu[i]=TRUE
    }
    
  }#end loop

  return(dat.nspires)
  
  }
