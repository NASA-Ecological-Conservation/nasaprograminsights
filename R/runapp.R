#' @title Build and Open the R Shiny Application
#' @description Builds and opens the R Shiny application on local machine. Features include generation and visualization of simple statistics.
#' @param data A list containing NSPIRES data. The list should have at least the two following elements (data frames or data tables): c(`people`, `proposals`)
#' @param removeNonEA logical Default TRUE will remove any data that is not currently defined as a program under Earth Action.
#' @param apptheme characater Default "lux" that specifies the bs_theme of the app
#' @param autodownload logical Default FALSE will automatically download proposals and people data frames if TRUE
#' @importFrom bslib bs_theme
#' @importFrom thematic thematic_shiny
#' @usage
#' nspires = make_nspires("local file directory with NSPIRES proposals")
#' runapp(data = nspires, removeNonEA = TRUE)
#' @details Developed by Anand Krishnan using Jessica Burnett's existing nasaprograminsights package.
#' @export runapp

runapp <-
  function(data = NULL,
           removeNonEA = TRUE,
           apptheme = "lux",
           autodownload = FALSE) {
    ## add a helper  fun here to run quick tests on the DATA file.
    # e.g., force names to lower, force STATUS to lower or UPPER and ensure downstream commands match case style.
    stopifnot(is.list(data) &
                (all(
                  c("people", "proposals") %in% tolower(names(data))
                )))
    names(data) <- tolower(names(data))
    data$`proposal status` <- toupper(data$`proposal status`)
    ui <-
      shiny::fluidPage(theme = bslib::bs_theme(bootswatch = "lux"),
                       # can change theme if you want
                       # UI for top menu bar
                       mainPanel(tabsetPanel(setupUI(),
                                             analysisUI())))
    
    server <- function(input, output, session) {
      thematic::thematic_shiny()
      # Remove all non Earth Action proposals if flag is set to true
      program_list <-
        c(
          "Atmospheric Composition",
          "BD",
          "Cryo",
          "Disasters",
          "Earth Surface & Interior (ESI)",
          "Ecological Conservation",
          "Environmental Justice",
          "ESD",
          "HAQ",
          "LCLUC",
          "MAP",
          "OBB",
          "Physical Oceanography (PO)",
          "SERVIR",
          "TE",
          "Water Resources",
          "Weather",
          "WEC Terrestrial Hydrology (THP)",
          "Wildfires"
        )
      if (removeNonEA) {
        data$proposals <-
          data$proposals[data$proposals$`program name` %in% program_list,]
        data$people <-
          data$people[data$people$`program name` %in% program_list,]
      }
      
      ## code for auto download - not working
      if (autodownload) {
        output$download_df <- shiny::downloadHandler(
          filename = function() {
            paste("proposals", ".csv", sep = "")
          },
          content = function(file) {
            write.csv(data$proposals, file)
          }
        )
        output$download_df <- shiny::downloadHandler(
          filename = function() {
            paste("people", ".csv", sep = "")
          },
          content = function(file) {
            write.csv(data$people, file)
          }
        )
      }
      
      # get list of program names to filter proposals by from the user check box input
      selected_programs <- shiny::reactiveValues(value = NULL)
      observe({
        selected_programs$value <- input$programs
      })
      # save selected program names for future prints
      output$selected_programs <- shiny::renderText({
        paste(selected_programs$value, collapse = ", ")
      })
      # save program names in reactive variable filtered_programs() for future use
      # future use: filter data frames and used in plots of proposals to differentiate between filtered programs and other programs
      filtered_programs <- shiny::reactive({
        selected_programs$value
      })
      
      # allow user to select a data frame to preview and download
      # these are the names user can select via a drop down mechanism
      selected_df <- shiny::reactive({
        switch(
          input$selected_df,
          "proposals" = data$proposals,
          "people" = data$people,
          "filtered proposals" = data$proposals[data$proposals$`program name` %in% filtered_programs(),],
          "filtered people" = data$people[data$people$`program name` %in% filtered_programs(),],
          "other proposals" = data$proposals[!data$proposals$`program name` %in% filtered_programs(),],
          "other people" = data$people[!data$people$`program name` %in% filtered_programs(),]
        )
      })
      # print number of rows for each data frame
      output$summary <- shiny::renderPrint(nrow(selected_df()))
      # download handler code for user to download dataframe as csv
      output$download_df <- shiny::downloadHandler(
        filename = function() {
          paste(input$selected_df, ".csv", sep = "")
        },
        content = function(file) {
          ## to do wish list: also add option to auto DL, with arg = outputfile directory--if that is specified, maybe force a DL
          write.csv(selected_df(), file)
        }
      )
      
      # server code for printing calculations for each metric
      output$all_result <- renderText({
        result <- input$all_dropdown
        if (result == metric1) {
          print_proposal_count(data$proposals, data$proposals[data$proposals$`program name` %in% filtered_programs(),])
        } else if (result == metric2) {
          print_proposal_award_amount(data$proposals, data$proposals[data$proposals$`program name` %in% filtered_programs(),])
        } else if (result == metric3) {
          print_pi(
            data.frame(data$proposals),
            data.frame(data$people),
            data.frame(data$proposals[data$proposals$`program name` %in% filtered_programs(),]),
            data.frame(data$people[data$people$`program name` %in% filtered_programs(),])
          )
        } else if (result == metric4) {
          print_org(data.frame(data$proposals),
                    data.frame(data$proposals[data$proposals$`program name` %in% filtered_programs(),]))
        } else if (result == metric5) {
          print_participation(props = data$proposals,
                              filtered_props = data$proposals[data$proposals$`program name` %in% filtered_programs(), ])
        }
      })
      
      # server code for plots
      # Each view has at most 4 plots, so there are 4 plot output functions
      # Depending on which view the user selected, a relevant plot is shown
      # tried gridextra to just have one function for plotting, but didn't work - maybe you could try?
      #pdf("plots.pdf") # save plots in pdf "plots.pdf"
      output$filtered_vs_other <- renderPlotly({
        result <- input$all_dropdown
        if (result == metric1) {
          plot_program_name(
            data$proposals[data$proposals$`program name` %in% filtered_programs(),],
            data$proposals[!data$proposals$`program name` %in% filtered_programs(),],
            "solicitation id",
            "Solicitation Count by Program Name"
          )
        } else if (result == metric2) {
          plot_pie(
            subset(
              data$proposals,
              `program name` %in% filtered_programs() &
                `proposal status` == "SELECTED"
            ),
            subset(
              data$proposals,
              !(`program name` %in% filtered_programs()) &
                `proposal status` == "SELECTED"
            ),
            "proposed award amount",
            "% Share of Total Proposal Award Amount"
          )
        } else if (result == metric3) {
          plot_program_name(data$proposals[data$proposals$`program name` %in% filtered_programs(),],
                            data$proposals[!data$proposals$`program name` %in% filtered_programs(),],
                            "pi",
                            "Submitted PI Count by Program Name")
        } else if (result == metric4) {
          plot_hist(data$proposals,
                    20,
                    "Linked Org Frequency in Earth Action Submissions")
        } else if (result == metric5) {
          plot_program_name(
            subset(
              data$proposals,
              `program name` %in% filtered_programs() &
                `proposal status` == "SELECTED"
            ),
            subset(
              data$proposals,
              !(`program name` %in% filtered_programs()) &
                `proposal status` == "SELECTED"
            ),
            "international",
            "Selected Proposal Int'l Participation by Program Name"
          )
        }
      })
      output$filtered_vs_other2 <- renderPlotly({
        result <- input$all_dropdown
        if (result == metric1) {
          plot_program_name(
            data$proposals[data$proposals$`program name` %in% filtered_programs(),],
            data$proposals[!data$proposals$`program name` %in% filtered_programs(),],
            "proposal count",
            "Submitted Proposal Count by Program Name"
          )
        } else if (result == metric2) {
          plot_program_name(
            subset(
              data$proposals,
              `program name` %in% filtered_programs() &
                `proposal status` == "SELECTED"
            ),
            subset(
              data$proposals,
              !(`program name` %in% filtered_programs()) &
                `proposal status` == "SELECTED"
            ),
            "total proposed award amount",
            "Total Proposal Award Amount by Program Name"
          )
        } else if (result == metric3) {
          plot_program_name(
            subset(
              data$people,
              (`program name` %in% filtered_programs()) &
                `status` == "SELECTED"
            ),
            subset(
              data$people,
              !(`program name` %in% filtered_programs()) &
                `status` == "SELECTED"
            ),
            "pi",
            "Selected PI Count by Program Name"
          )
        } else if (result == metric4) {
          plot_hist(
            subset(data$proposals,
                   `proposal status` == "SELECTED"),
            20,
            "Linked Org Frequency in Earth Action Selections"
          )
        } else if (result == metric5) {
          plot_program_name(
            subset(
              data$proposals,
              `program name` %in% filtered_programs() &
                `proposal status` == "SELECTED"
            ),
            subset(
              data$proposals,
              !(`program name` %in% filtered_programs()) &
                `proposal status` == "SELECTED"
            ),
            "nasa",
            "Selected Proposal NASA Participation by Program Name"
          )
        }
      })
      output$filtered_vs_other3 <- renderPlotly({
        result <- input$all_dropdown
        if (result == metric1) {
          plot_program_name(
            subset(
              data$proposals,
              `program name` %in% filtered_programs() &
                `proposal status` == "SELECTED"
            ),
            subset(
              data$proposals,
              !(`program name` %in% filtered_programs()) &
                `proposal status` == "SELECTED"
            ),
            "proposal count",
            "Selected Proposal Count by Program Name"
          )
        } else if (result == metric2) {
          plot_program_name(
            subset(
              data$proposals,
              `program name` %in% filtered_programs() &
                `proposal status` == "SELECTED"
            ),
            subset(
              data$proposals,
              !(`program name` %in% filtered_programs()) &
                `proposal status` == "SELECTED"
            ),
            "average proposed award amount",
            "Mean Proposal Award Amount by Program Name"
          )
        } else if (result == metric3) {
          plot_pie(
            subset(
              data$people,
              (`program name` %in% filtered_programs()) &
                `status` == "SELECTED"
            ),
            subset(
              data$people,
              !(`program name` %in% filtered_programs()) &
                `status` == "SELECTED"
            ),
            "pi count",
            "% Share of Total Selected PI Count"
          )
        } else if (result == metric4) {
          plot_hist(as.data.frame(
            subset(
              data$proposals,
              `program name` %in% filtered_programs()
            )
          ),
          20,
          "Linked Org Frequency in Filtered Submissions")
        }
      })
      output$filtered_vs_other4 <- renderPlotly({
        result <- input$all_dropdown
        if (result == metric1) {
          #This plot is not working for some reason, not sure why, need to look more into this
          plot_program_name(
            data$proposals[data$proposals$`program name` %in% filtered_programs(),],
            data$proposals[!data$proposals$`program name` %in% filtered_programs(),],
            "selection percentage",
            "Proposal Selection Percentage by Program Name"
          )
        } else if (result == metric4) {
          plot_hist(
            subset(
              data$proposals,
              `program name` %in% filtered_programs() &
                `proposal status` == "SELECTED"
            ),
            20,
            "Linked Org Frequency in Filtered Selections"
          )
        }
      })
    }
    shinyApp(ui = ui, server = server)
  }
