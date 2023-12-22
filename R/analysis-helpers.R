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
  HTML(
    r"(
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
          )"
  )
}

#' @title Program Analysis Drop Down and Plots UI
#' @description
#' Output drop down menu for user to select a metric to view and the associated calculations and plots for the metric.
#' @usage
#' tabPanel("Introduction & Setup", analysis_dropdown())
# Can change name of metrics in dropdown here
metric1 <- "Solicitations & Proposals"
metric2 <- "Award Amount Summaries"
metric3 <- "PI & Member Participation"
metric4 <- "Linked organizations"
metric5 <- "Proposal participation"
analysis_dropdown <- function() {
  metrics <- c(metric1, metric2, metric3, metric4, metric5)
  fluidPage (
    selectInput("all_dropdown", "Select a view", metrics),
    htmlOutput("all_result"),
    plotlyOutput("filtered_vs_other"),
    plotlyOutput("filtered_vs_other2"),
    plotlyOutput("filtered_vs_other3"),
    plotlyOutput("filtered_vs_other4")
  )
}
