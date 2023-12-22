#' @title Run NASA Earth Action Program Analysis App
#' @description
#' Function to be called to the run the app to view statistics and plots visualizing past Earth Action program investments.
#' @param data NPSIRES data frame containing data about Earth Action proposals and people
#' @param removeNonEA logical value that determines whether proposals not belonging to an Earth Action program will be ignored
#' @usage
#' nspires = make_nspires("local file directory with NSPIRES proposals")
#' runapp(data = nspires, removeNonEA = TRUE)
#' @export
runapp <- function(data = NULL, removeNonEA = TRUE) {
  ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "lux"), # can change theme if you want
    # UI for top menu bar
    mainPanel(
      tabsetPanel(
        setupUI(),
        analysisUI()
      )
    )
  )
  
  server <- function(input, output, session) {
    thematic::thematic_shiny()
    # Remove all non Earth Action proposals if flag is set to true
    program_list <- c("Atmospheric Composition", "BD", "Cryo", "Disasters", "Earth Surface & Interior (ESI)", "Ecological Conservation", "Environmental Justice", "ESD", "HAQ", "LCLUC", "MAP", "OBB", "Physical Oceanography (PO)", "SERVIR", "TE", "Water Resources", "Weather", "WEC Terrestrial Hydrology (THP)", "Wildfires")
    if (removeNonEA) {
      data$proposals <- data$proposals[data$proposals$`program name` %in% program_list, ]
      data$people <- data$people[data$people$`program name` %in% program_list, ]
    }
    # get list of program names to filter proposals by from the user check box input
    selected_programs <- reactiveValues(value = NULL)
    observe({
      selected_programs$value <- input$programs
    })
    # print selected program names
    output$selected_programs <- renderText({
      paste(selected_programs$value, collapse = ", ")
    })
    # save program names in reactive variable filtered_programs() for future use
    # future use: filter data frames and used in plots of proposals to differentiate between filtered programs and other programs
    filtered_programs <- reactive({
      selected_programs$value
    })
    
    # allow user to select a data frame to preview and download
    # drop down mechanism
    selected_df <- reactive({
      switch(input$selected_df,
             "proposals" = data$proposals,
             "people" = data$people,
             "filtered proposals" = data$proposals[data$proposals$`program name` %in% filtered_programs(), ],
             "filtered people" = data$people[data$people$`program name` %in% filtered_programs(), ],
             "other proposals" = data$proposals[!data$proposals$`program name` %in% filtered_programs(), ],
             "other people" = data$people[!data$people$`program name` %in% filtered_programs(), ])
    })
    # print number of rows for each data frame
    output$summary <- renderPrint(nrow(selected_df())) 
    # download handler code for user to download dataframe as csv
    output$download_df <- downloadHandler(
      filename = function() {
        paste(input$selected_df, ".csv", sep="")
      },
      content = function(file) {
        write.csv(selected_df(), file)
      }
    )
    
    # server code for printing calculations for each metric
    output$all_result <- renderText({
      result <- input$all_dropdown
      if (result == "Proposal count") {
        print_proposal_count(data$proposals, data$proposals[data$proposals$`program name` %in% filtered_programs(), ])
      } else if (result == "Proposal award amount") {
        print_proposal_award_amount(data$proposals, data$proposals[data$proposals$`program name` %in% filtered_programs(), ])
      } else if (result == "PI and member participation") {
        print_pi(data.frame(data$proposals), data.frame(data$people), data.frame(data$proposals[data$proposals$`program name` %in% filtered_programs(), ]), data.frame(data$people[data$people$`program name` %in% filtered_programs(), ]))
      } else if (result == "Linked organizations") {
        print_org(data.frame(data$proposals), data.frame(data$proposals[data$proposals$`program name` %in% filtered_programs(), ]))
      } else if (result == "Proposal participation") {
        print_participation(data$proposals, data$proposals[data$proposals$`program name` %in% filtered_programs(), ])
      }
    })
    
    # server code for plots
    # Each view has at most 4 plots, so there are 4 plot output functions
    # Depending on which view the user selected, a relevant plot is shown 
    # tried gridextra to just have one function for plotting, but didn't work - maybe you could try?
    #pdf("plots.pdf") # save plots in pdf "plots.pdf"
    output$filtered_vs_other <- renderPlotly({
      result <- input$all_dropdown
      if (result == "Proposal count") {
        plot_program_name(data$proposals[data$proposals$`program name` %in% filtered_programs(), ], data$proposals[!data$proposals$`program name` %in% filtered_programs(), ], "solicitation id", "Solicitation Count by Program Name")
      } else if (result == "Proposal award amount") {
        plot_pie(subset(data$proposals, `program name` %in% filtered_programs() & `proposal status` == "SELECTED"), subset(data$proposals, !(`program name` %in% filtered_programs()) & `proposal status` == "SELECTED"), "proposed award amount", "% Share of Total Proposal Award Amount")
      } else if (result == "PI and member participation") {
        plot_program_name(data$proposals[data$proposals$`program name` %in% filtered_programs(), ], data$proposals[!data$proposals$`program name` %in% filtered_programs(), ], "pi", "Submitted PI Count by Program Name")
      } else if (result == "Proposal participation") {
        plot_program_name(subset(data$proposals, `program name` %in% filtered_programs() & `proposal status` == "SELECTED"), subset(data$proposals, !(`program name` %in% filtered_programs()) & `proposal status` == "SELECTED"), "international", "Selected Proposal Int'l Participation by Program Name")
      }
    })
    output$filtered_vs_other2 <- renderPlotly({
      result <- input$all_dropdown
      if (result == "Proposal count") {
        plot_program_name(data$proposals[data$proposals$`program name` %in% filtered_programs(), ], data$proposals[!data$proposals$`program name` %in% filtered_programs(), ], "proposal count", "Submitted Proposal Count by Program Name")
      } else if (result == "Proposal award amount") {
        plot_program_name(subset(data$proposals, `program name` %in% filtered_programs() & `proposal status` == "SELECTED"), subset(data$proposals, !(`program name` %in% filtered_programs()) & `proposal status` == "SELECTED"), "total proposed award amount", "Total Proposal Award Amount by Program Name")
      } else if (result == "PI and member participation") {
        plot_program_name(subset(data$people, (`program name` %in% filtered_programs()) & `status` == "SELECTED"), subset(data$people, !(`program name` %in% filtered_programs()) & `status` == "SELECTED"), "pi", "Selected PI Count by Program Name")
      } else if (result == "Proposal participation") {
        plot_program_name(subset(data$proposals, `program name` %in% filtered_programs() & `proposal status` == "SELECTED"), subset(data$proposals, !(`program name` %in% filtered_programs()) & `proposal status` == "SELECTED"), "nasa", "Selected Proposal NASA Participation by Program Name")
      }
    })
    output$filtered_vs_other3 <- renderPlotly({
      result <- input$all_dropdown
      if (result == "Proposal count") {
        plot_program_name(subset(data$proposals, `program name` %in% filtered_programs() & `proposal status` == "SELECTED"), subset(data$proposals, !(`program name` %in% filtered_programs()) & `proposal status` == "SELECTED"), "proposal count", "Selected Proposal Count by Program Name")
      } else if (result == "Proposal award amount") {
        plot_program_name(subset(data$proposals, `program name` %in% filtered_programs() & `proposal status` == "SELECTED"), subset(data$proposals, !(`program name` %in% filtered_programs()) & `proposal status` == "SELECTED"), "average proposed award amount", "Mean Proposal Award Amount by Program Name")
      } else if (result == "PI and member participation") {
        plot_pie(subset(data$people, (`program name` %in% filtered_programs()) & `status` == "SELECTED"), subset(data$people, !(`program name` %in% filtered_programs()) & `status` == "SELECTED"), "pi count", "% Share of Total Selected PI Count")
      }
    })
    output$filtered_vs_other4 <- renderPlotly({
      result <- input$all_dropdown
      if (result == "Proposal count") {
        # This plot is not working for some reason, not sure why, need to look more into this
        #plot_program_name(data$proposals[data$proposals$`program name` %in% filtered_programs(), ], data$proposals[!data$proposals$`program name` %in% filtered_programs(), ], "selection percentage", "Proposal Selection Percentage by Program Name")
      }
    })
    #dev.off()
  }
  
  shinyApp(ui = ui, server = server)
}
