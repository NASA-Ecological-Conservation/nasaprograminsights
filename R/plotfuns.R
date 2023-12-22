# the following palettes are colorblind friendly according to https://davidmathlogic.com/colorblind/
cb_palette1 <- c("#ffc20a", "#0c7bdc") 
cb_palette2 <- c("#994f00", "#006cd1") 
cb_palette3 <- c("#e1be6a", "#40b0a6") 
cb_palette4 <- c("#e66100", "#5d3a9b") 
cb_palette5 <- c("#1aff1a", "#4b0092") 
cb_palette6 <- c("#fefe62", "#d35f7b") 
cb_palette7 <- c("#005ab5", "#dc3220")
cb_palette8 <- c("#1a85ff", "#d41159") 

#' @title Plot Bar Plot by Program Name
#' @description
#' Function plotting a horizontal bar plot grouping a given data frame by program name by a passed in metric. Compares filtered program names to other program names.
#' @param df1 data frame containing filtered proposals
#' @param df2 data frame containing other proposals
#' @param metric string specifying what the program names should be plotted by
#' @param plot_title string specifying the title of the plot
#' @returns A plotly object to be printed in the Proposal Analysis tab.
#' @importFrom plotly plot_ly 
#' @importFrom tidyr gather
#' @usage plot_program_name(nspires$proposals[nspires$proposals$`program name` %in% filtered_programs(), ], nspires$proposals[!nspires$proposals$`program name` %in% filtered_programs(), ], "international", "Submitted Proposal Int'l Participation by Program Name")
#' @keywords internal
#' @details
#' The following metrics can be passed into `plot_program_name`:
#' 1. "solicitation id"
#' 2. "proposal count"
#' 3. "selection percentage"
#' 4. "total proposed award amount"
#' 5. "average proposed award amount"
#' 6. "pi"
#' 7. "international"
#' 8. "nasa"
plot_program_name <- function(df1, df2, metric, plot_title) {
  # Count unique solicitation ids per program name for each data frame
  x_axis <- ""
  ## wish list--fix this entire if else bulk to be one or two funs.
  if (metric == "solicitation id") {
    df1_counts <- df1 |> group_by(`program name`) |> summarize(Filtered_Programs = n_distinct(`solicitation id`))
    df2_counts <- df2 |> group_by(`program name`) |> summarize(Other_Programs = n_distinct(`solicitation id`))
  } else if (metric == "proposal count") {
    df1_counts <- df1 |> group_by(`program name`) |> summarize(Filtered_Programs = n_distinct(`proposal number`))
    df2_counts <- df2 |> group_by(`program name`) |> summarize(Other_Programs = n_distinct(`proposal number`))
  } else if (metric == "total proposed award amount") {
    df1_counts <- df1 |> group_by(`program name`) |> summarize(Filtered_Programs = sum(`proposed amount total`, na.rm = TRUE))
    df2_counts <- df2 |> group_by(`program name`) |> summarize(Other_Programs = sum(`proposed amount total`, na.rm = TRUE))
  } else if (metric == "average proposed award amount") {
    df1_counts <- df1 |> group_by(`program name`) |> summarize(Filtered_Programs = mean(`proposed amount total`, na.rm = TRUE))
    df2_counts <- df2 |> group_by(`program name`) |> summarize(Other_Programs = mean(`proposed amount total`, na.rm = TRUE))
  } else if (metric == "pi") {
    df1_counts <- df1 |> group_by(`program name`) |> summarize(Filtered_Programs = n_distinct(`pi suid`))
    df2_counts <- df2 |> group_by(`program name`) |> summarize(Other_Programs = n_distinct(`pi suid`))
  } else if (metric == "international") {
    df1_counts <- df1 |> group_by(`program name`) |> summarize(Filtered_Programs = sum(`international participation` == "true"))
    df2_counts <- df2 |> group_by(`program name`) |> summarize(Other_Programs = sum(`international participation` == "true"))
  } else if (metric == "nasa") {
    df1_counts <- df1 |> group_by(`program name`) |> summarize(Filtered_Programs = sum(`nasa staff` == "true"))
    df2_counts <- df2 |> group_by(`program name`) |> summarize(Other_Programs = sum(`nasa staff` == "true"))
  } else if (metric == "selection percentage") {
    df1_counts <- df1 |> group_by(`program name`) |> summarize(Filtered_Programs = round(sum(`proposal status` == "SELECTED") / n_distinct(`proposal number`) * 100, 0))
    df2_counts <- df2 |> group_by(`program name`) |> summarize(Other_Programs = round(sum(`proposal status` == "SELECTED") / n_distinct(`proposal number`) * 100, 0))
  }
  # Merge the counts into a single dataframe
  merged_df <- merge(df2_counts, df1_counts, by = "program name", all = TRUE)
  # Replace missing counts with 0 
  merged_df[is.na(merged_df)] <- 0
  # Reshape the data for plotting
  plot_df <- tidyr::gather(merged_df, "Dataframe", "Count", -`program name`) ### might need to change to pivot_X
  # Create the bar plot
  plot <- plotly::plot_ly(plot_df, x = ~Count, y = ~`program name`, color = ~Dataframe, type = 'bar', colors = cb_palette8) |>
    layout(title = plot_title, xaxis = list(title = 'Count'), yaxis = list(title = 'Program Name'))
  return(plot)
}

#' @title Create a Pie Chart by Program Name
#' @description
#' Function plotting a pie plot grouping a given data frame by program name by a passed in metric. Compares filtered program names to other program names.
#' @param df1 data frame containing filtered proposals
#' @param df2 data frame containing other proposals
#' @param metric string specifying what the program names should be plotted by
#' @param plot_title string specifying the title of the plot
#' @returns A plotly object to be printed in the Proposal Analysis tab.
#' @keywords internal
#' @usage plot_program_name(nspires$proposals[nspires$proposals$`program name` %in% filtered_programs(), ], nspires$proposals[!nspires$proposals$`program name` %in% filtered_programs(), ], "international", "Submitted Proposal Int'l Participation by Program Name")
#' @details
#' The following metrics can be passed into `plot_program_name`:
#' 1. "proposed award amount"
#' 2. "pi count"
plot_pie <- function(df1, df2, metric, plot_title) {
  if (metric == "proposed award amount") {
    nrow_df1 <- nrow(df1)
    nrow_df2 <- nrow(df2)
  } else if (metric == "pi count") {
    nrow_df1 <- length(unique(df1$`pi suid`))
    nrow_df2 <- length(unique(df2$`pi suid`))
  }
  df <- data.frame(Category = c("Filtered proposals", "Other proposals"), Count = c(nrow_df1, nrow_df2))
  plot <- plot_ly(df, labels = ~Category, values = ~Count,
                  type = "pie",
                  textinfo = "label+percent",
                  marker = list(colors = cb_palette8)) |>
    layout(title = plot_title, showlegend = TRUE)
  return(plot)
}

#' @title Plot a Histogram Showing Linked Org Frequencies
#' @description
#' Function plotting a histogram displaying the frequencies of the top n linked orgs appearing in the passed in data frame.
#' @param df data frame containing proposals
#' @param n numeric specifying how many linked orgs should be plotted
#' @param plot_title string specifying the title of the plot
#' @returns A plotly object to be printed in the Proposal Analysis tab.
#' @keywords internal
#' @usage plot_hist(data$proposals, 20, "Linked Org Frequency in Earth Action Submissions")
plot_hist <- function(df, n, plot_title) {
  value_counts <- table(df$`linked org`)
  top_values <- names(sort(value_counts, decreasing = TRUE)[1:n])
  df_top <- df[df$`linked org` %in% top_values, ]
  df_top$`linked org` <- factor(df_top$`linked org`, levels = (top_values))
  plot <- plot_ly(df_top, x = ~`linked org`, type = "histogram") |> 
  layout(title = plot_title)
  return(plot)
}
