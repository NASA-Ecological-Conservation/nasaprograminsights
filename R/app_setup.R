#' @title Setup Page UI
#' @description
#' Base UI function for Introduction & Setup page calling other UI functions
#' @usage
#' ui <- fluidPage(mainPanel(tabsetPanel(setupUI())))
setupUI <- function() {
  tabPanel("Introduction & Setup",
           setup_intro(), # intro message
           filter_programs(), # checkboxes to select programs to filter proposals by
           display_dfs()) # display nrow and download option for dataframes
}

#' @title Setup Page Intro Text
#' @description
#' Output text introducing the user to the app and how it works.
#' @usage
#' tabPanel("Introduction & Setup", setup_intro())
setup_intro <- function() {
  HTML(r"(
          <style>
            .red {
              color: red;
            }
            .blue {
              color: blue;
            }
            .green {
              color: green;
            }
            .purple {
              color: purple;
            }
          </style>
          <br>
          <h1>NASA Earth Action Program Insights</h1>
          <p>This tab documents the setup process to analyze the program data. The <span class="blue">Program Analysis</span> tab will display the statistics of all of the proposals using the specified metrics.</p>
          <br>
          <p>The NASA Program Insights application aims to holistically analyze NASA program investments in the Earth Action between 2006-2022 through the following 5 lenses: proposal count; proposal award amount; PI and member info; linked organization; and international and NASA participation.</p>
          <p>These notebooks use the <span class="red">proposals</span class="red"> and <span class="red">people</span class="red"> data frames from the <span class="red">nasaprograminsights</span class="red"> package. <span class="red">proposals</span class="red"> and <span class="red">people</span class="red"> summarize NASA ROSES solicitations using (as of 2020-11-18) NSPIRES exported data.</p>
          <p>The insights will be calculated for 3 sets of data frames: 1) <span class="blue">All</span>, which contain data on every proposal from the <span class="red">nasaprograminsights</span class="red"> package; 2) <span class="blue">Filtered</span>, which only contain data for solicitations from the specified Earth Action program; and 3) <span class="blue">Other</span>, which only contain data for the solicitations from the non-specified programs.</p>
          <p>Each set of data frames will be further divided into 2 categories to be compared with each other: <span class="green">submitted</span> (all proposals submitted within the set) and <span class="green">selected</span> (only proposals within the set whose status is "SELECTED").</p>
          <p>Each insight in the program analysis tab will have 2 sections: printed calculations, and a set of plots visualizing the created data frames.</p>
          <p class="purple"><strong>The goal of this application is to provide a lightweight, easy-to-use solution for NASA Earth Action program managers to analyze past program investments to inform future investment decisions.</b></strong>
       )")
}

#' @title Filter Programs UI
#' @description
#' Output check box element for user to select program names to filter proposals by and view their selections.
#' @usage
#' tabPanel("Introduction & Setup", filter_programs())
filter_programs <- function() {
  programs <- c("Atmospheric Composition", "BD", "Cryo", "Disasters", "Earth Surface & Interior (ESI)", "Ecological Conservation", "Environmental Justice", "ESD", "HAQ", "LCLUC", "MAP", "OBB", "Physical Oceanography (PO)", "SERVIR", "TE", "Water Resources", "Weather", "WEC Terrestrial Hydrology (THP)", "Wildfires")
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("programs", "Select the Earth Action program names that you want to filter the proposals by:",
                           programs)),
    mainPanel(
      h3("Selected programs:"),
      textOutput("selected_programs")
    )
  )
  )
}

#' @title Display Data Frame Text UI
#' @description
#' Output text with instructions for the user to view and download the proposal and people data frames from the app.
#' @usage
#' fluidPage(display_dfs_text())
display_dfs_text <- function() {
  HTML(r"(
          <br><br>
          <h2>View and Download data frames</h2>
          <ol>
            <li>props contains data on all proposals in the nasaprograminsights package</li>
            <li>ppl contains data on all PIs and members in the nasaprograminsights package</li>
            <li>filtered_props is a subset of props containing data on all proposals whose solicitation number was entered by the user to filter the proposals</li>
            <li>filtered_ppl is a subset of ppl containing data on all PIs and members whose proposals' solicitation number was entered by the user to filter the proposals</li>
            <li>other_props contains all of the proposals not in filtered_props</li>
            <li>other_ppl contains all of the people not in filtered_ppl</li>
            </ol>
       )")
}

#' @title Display Data Frames UI
#' @description
#' Output drop down menu for user to select a data frame to view, a Verbatim-text output for the user to see how many rows are in the selected data frame,
#' and a download handler for the user to download the selected data frame.
#' @usage
#' tabPanel("Introduction & Setup", display_dfs())
display_dfs <- function() {
  fluidPage(
    display_dfs_text(),
    selectInput("selected_df", "Select a data frame:",
                choices = c("proposals", "people", "filtered proposals", "filtered people", "other proposals", "other people")),
    HTML(r"(<p>Number of unique proposals in the selected data frame:</p>)"),
    verbatimTextOutput("summary"),
    downloadButton("download_df", "Download Data")
  )
}
