#' @title Print Proposal Count Statistics
#' @description
#' Function printing information related to all program and filtered programs' solicitation and proposal counts.
#' @param props data frame containing all proposals
#' @param filtered_props data frame containing only proposals from filtered programs
#' @returns A concatenated string to be printed in the Proposal Analysis tab when the `Proposal count` option is selected in the view drop down.
#' @usage print_proposal_count(nspires$proposals, nspires$proposals[nspires$proposals$`program name` %in% filtered_programs(), ])
#' @details
#' The following information is printed for both all proposals and filtered proposals:\n
#' 1. Number of solicitations\n
#' 2. Number of submitted proposals\n
#' 3. Number of selected proposals\n
#' 4. Proposal selection percentage (number selected / submitted * 100)
print_proposal_count <- function(props, filtered_props) {
  paste0(
    "<h5>All Programs Proposal Count Statistics</h5>There were ", length(unique(props$`solicitation id`)), " Earth Action solicitations between 2006-2022.<br>",
    nrow(props), " proposals were submitted.<br>",
    nrow(props[props$`proposal status` == "SELECTED", ]), " were selected.<br>",
    "The selection percentage was ", round(nrow(props[props$`proposal status` == "SELECTED", ])/nrow(props)*100, 2), "%.<br><br><h5>Filtered Programs Proposal Count Statistics</h5>There were ",
    length(unique(filtered_props$`solicitation id`)), " filtered program solicitations between 2006-2022.<br>",
    nrow(filtered_props), " proposals were submitted.<br>",
    nrow(filtered_props[filtered_props$`proposal status` == "SELECTED", ]), " were selected.<br>",
    "The selection percentage was ", round(nrow(filtered_props[filtered_props$`proposal status` == "SELECTED", ])/nrow(filtered_props)*100, 2), "%.<br><br>"
  )
}

#' @title Print Proposal Award Amount Statistics
#' @description
#' Function printing information related to all program and filtered programs' proposal award amounts.
#' @param props data frame containing all proposals
#' @param filtered_props data frame containing only proposals from filtered programs
#' @returns A concatenated string to be printed in the Proposal Analysis tab when the `Proposal award amount` option is selected in the view drop down.
#' @usage print_proposal_award_amount(nspires$proposals, nspires$proposals[nspires$proposals$`program name` %in% filtered_programs(), ])
#' @details
#' The following information is printed for both all proposals and filtered proposals:\n
#' 1. The total amount awarded by NASA to selected proposals\n
#' 2. The average amount per proposal awarded by NASA to selected proposals
print_proposal_award_amount <- function(props, filtered_props) {
  paste0(
    "<h5>All Programs Proposal Award Amount Statistics</h5>The total amount awarded by NASA to selected Earth Action proposals between 2006-2022 was $", sum(props[props$`proposal status` == "SELECTED", ]$`proposed amount total`, na.rm = TRUE),
    ".<br>The average amount per proposal awarded by NASA to selected Earth Action proposals during this time was $", round(mean(props[props$`proposal status` == "SELECTED", ]$`proposed amount total`, na.rm = TRUE), 2),
    "<br><br><h5>Filtered Programs Proposal Award Statistics</h5>The total amount awarded by NASA to selected filtered program proposals between 2006-2022 was $", sum(filtered_props[filtered_props$`proposal status` == "SELECTED", ]$`proposed amount total`, na.rm = TRUE),
    ".<br>The average amount per proposal awarded by NASA to selected filtered program proposals during this time was $", round(mean(filtered_props[filtered_props$`proposal status` == "SELECTED", ]$`proposed amount total`, na.rm = TRUE), 2), ".<br><br>"
  )
}

#' @title Print PI and Member Statistics
#' @description
#' Function printing information related to all program and filtered programs' pi and member enrollment.
#' @param props data frame containing all proposals
#' @param ppl data frame containing all people
#' @param filtered_props data frame containing only proposals from filtered programs
#' @param filtered_ppl data frame containing only people from filtered programs
#' @returns A concatenated string to be printed in the Proposal Analysis tab when the `PI and member participation` option is selected in the view drop down.
#' @usage print_pi(data.frame(nspires$proposals), data.frame(nspires$people), data.frame(nspires$proposals[nspires$proposals$`program name` %in% filtered_programs(), ]), data.frame(nspires$people[nspires$people$`program name` %in% filtered_programs(), ]))
#' @details
#' The following information is printed for both all proposals and filtered proposals:\n
#' 1. Number of PIs who submitted a proposal\n
#' 2. Number of PIs who had a selected proposal\n
#' 3. Number of members who were listed on a submitted proposal\n
#' 4. Number of members who were listed on a selected proposal\n
#' 5. Number of members per proposal on average\n
#' 6. The average selection percentage for PIs (number of selections/number of submission for each PI)\n
#' 7. Number of PIs who submitted multiple proposals\n
#' 8. Percentage of PIs involved in multiple submissions\n
#' 9. Number of PIs who were selected multiple times\n
#' 10. Out of PIs who submitted multiple times, percentage of PIs were selected at once.\n
#' 11. The average selection percentage for PIs who submitted multiple times
print_pi <- function(props, ppl, filtered_props, filtered_ppl) {
  pis <- get_pi_df(props)
  multiple_apps <- pis[pis$`submission.count` > 1, ]
  num_props <- length(unique(ppl$`proposal.number`))
  ppl_selected <- ppl[ppl$`status` == "SELECTED", ]
  num_selected_props <- length(unique(ppl_selected$`proposal.number`))
  selected_props <- props[props$`proposal.status` == "SELECTED", ]
  num_pis <- length(unique(ppl$`pi.suid`))
  num_members <- length(unique(ppl$`member.suid`))
  num_multiple <- length(unique(multiple_apps$`pi.suid`))
  #rejected_before_selected <- get_rejected_before_selected(props, "pi.suid")
  #rejected_after_selected <- get_rejected_after_selected(props, "pi.suid")
  
  if (nrow(filtered_props) == 0) {
    paste0(
      "<h5>All Programs PI and Member Statistics</h5>",
      num_pis, " PIs submitted proposals.<br>",
      length(unique(ppl_selected$`pi.suid`)), " PIs were selected.<br>",
      num_members, " members were listed on submitted proposals.<br>On average, each submitted proposal had ", 
      ceiling(length(ppl$`member.suid`) / length(table(ppl$`pi.suid`))), " members on the team.<br>",
      length(unique(ppl_selected$`member.suid`)), " members were listed on selected proposals.<br>On average, each selected proposal had ", 
      ceiling(length(ppl_selected$`member.suid`) / num_selected_props), " members on the team.<br>The average selection percentage for PIs (number of selections/number of submission for each PI) was ",
      round(mean(pis$selection.percentage), 0), "%.<br><br>",
      num_multiple, " PIs submitted multiple proposals.<br>",
      round(num_multiple / num_pis * 100, 0), "% of PIs submitted a proposal multiple times.<br>",
      nrow(multiple_apps[multiple_apps$`selection.count` > 0, ]), " PIs were selected multiple times.<br>Out of PIs who submitted multiple times, ",
      round(nrow(multiple_apps[multiple_apps$`selection.count` > 0, ]) / nrow(multiple_apps) * 100, 0), "% were selected at once. <br>The average selection percentage for PIs who submitted multiple times was ",
      round(mean(multiple_apps$selection.percentage), 0), "%.<br><br>"
    )
  } else {
    filtered_pis <- get_pi_df(filtered_props)
    filtered_multiple_apps <- filtered_pis[filtered_pis$`submission.count` > 1, ]
    filtered_num_props <- length(unique(filtered_ppl$`proposal.number`))
    filtered_ppl_selected <- filtered_ppl[filtered_ppl$`status` == "SELECTED", ]
    filtered_num_selected_props <- length(unique(filtered_ppl_selected$`proposal.number`))
    filtered_selected_props <- filtered_props[filtered_props$`proposal.status` == "SELECTED", ]
    filtered_num_pis <- length(unique(filtered_ppl$`pi.suid`))
    filtered_num_members <- length(unique(filtered_ppl$`member.suid`))
    filtered_num_multiple <- length(unique(filtered_multiple_apps$`pi.suid`))
    paste0(
      "<h5>All Programs PI and Member Statistics</h5>",
      num_pis, " PIs submitted proposals.<br>",
      length(unique(ppl_selected$`pi.suid`)), " PIs were selected.<br>",
      num_members, " members were listed on submitted proposals.<br>On average, each submitted proposal had ", 
      ceiling(length(ppl$`member.suid`) / length(table(ppl$`pi.suid`))), " members on the team.<br>",
      length(unique(ppl_selected$`member.suid`)), " members were listed on selected proposals.<br>On average, each selected proposal had ", 
      ceiling(length(ppl_selected$`member.suid`) / num_selected_props), " members on the team.<br>The average selection percentage for PIs (number of selections/number of submission for each PI) was ",
      round(mean(pis$selection.percentage), 0), "%.<br><br>",
      num_multiple, " PIs submitted multiple proposals.<br>Out of all PIs who submitted a proposal, ",
      round(num_multiple / num_pis * 100, 0), "% of PIs submitted a proposal multiple times.<br>",
      nrow(multiple_apps[multiple_apps$`selection.count` > 0, ]), " PIs were selected multiple times.<br>Out of PIs who submitted multiple times, ",
      round(nrow(multiple_apps[multiple_apps$`selection.count` > 0, ]) / nrow(multiple_apps) * 100, 0), "% were selected at once. <br>The average selection percentage for PIs who submitted multiple times was ",
      round(mean(multiple_apps$selection.percentage), 0), "%.<br><br><h5>Filtered Programs PI and Member Statistics</h5>",
      
      filtered_num_pis, " PIs submitted filtered proposals.<br>",
      length(unique(filtered_ppl_selected$`pi.suid`)), " PIs who submitted filtered proposals were selected.<br>",
      filtered_num_members, " members were listed on submitted proposals.<br>On average, each submitted proposal had ", 
      ceiling(length(filtered_ppl$`member.suid`) / length(table(filtered_ppl$`pi.suid`))), " members on the team.<br>",
      length(unique(filtered_ppl_selected$`member.suid`)), " members were listed on selected proposals.<br>On average, each selected proposal had ", 
      ceiling(length(filtered_ppl_selected$`member.suid`) / filtered_num_selected_props), " members on the team.<br>The average selection percentage for PIs (number of selections/number of submission for each PI) was ",
      round(mean(filtered_pis$selection.percentage), 0), "%.<br><br>",
      filtered_num_multiple, " PIs submitted multiple proposals.<br>Out of all PIs who submitted a proposal, ",
      round(filtered_num_multiple / filtered_num_pis * 100, 0), "% of PIs submitted a proposal multiple times.<br>",
      nrow(filtered_multiple_apps[filtered_multiple_apps$`selection.count` > 0, ]), " PIs were selected multiple times.<br>Out of PIs who submitted multiple times, ",
      round(nrow(filtered_multiple_apps[filtered_multiple_apps$`selection.count` > 0, ]) / nrow(filtered_multiple_apps) * 100, 0), "% were selected at once. <br>The average selection percentage for PIs who submitted multiple times was ",
      round(mean(filtered_multiple_apps$selection.percentage), 0), "%.<br><br>"
    )
  }
}

#' @title Print Linked Organization Statistics
#' @description
#' Function printing information related to all program and filtered programs' linked organizations.
#' @param props data frame containing all proposals
#' @param filtered_props data frame containing only proposals from filtered programs
#' @returns A concatenated string to be printed in the Proposal Analysis tab when the `Linked organizations` option is selected in the view drop down.
#' @usage print_org(data.frame(nspires$proposals), data.frame(nspires$proposals[nspires$proposals$`program name` %in% filtered_programs(), ]))
#' @details
#' The following information is printed for both all proposals and filtered proposals:\n
#' 1. Number of organizations who submitted a proposal\n
#' 2. Number of organizations who had a selected proposal\n
#' 3. Number of organizations who submitted multiple proposals\n
#' 4. Percentage of organizations who submitted a proposal multiple times\n
#' 5. Number of organizations who were selected multiple times\n
#' 5. Percentage of organizations selected multiple times
print_org <- function(props, filtered_props) {
  orgs <- get_org_df(props)
  multiple_apps <- orgs[orgs$submission.count > 1, ]
  selected_props <- props[props$`proposal.status` == "SELECTED", ]
  
  if (nrow(filtered_props) == 0) {
    paste0(
      "<h5>All Programs Linked Organization Statistics</h5>",
      length(unique(props$linked.org)), " organizations were involved in proposal submissions.<br>",
      length(unique(selected_props$linked.org)), " organizations were involved in proposal selections.<br><br>",
      nrow(multiple_apps), " organizations were involved in multiple submissions.<br>",
      round(sum(orgs$submission.count > 1) / nrow(orgs) * 100, 2), "% of organizations were involved in multiple submissions.<br>",
      nrow(multiple_apps[multiple_apps$`selection.count` > 0, ]), " organizations were involved in multiple selections<br>",
      round(nrow(multiple_apps[multiple_apps$`selection.count` > 0, ]) / nrow(multiple_apps) * 100, 2), "% of organizations were involved in multiple selections.<br><br>"
    )
  } else {
    filtered_orgs <- get_org_df(filtered_props)
    filtered_multiple_apps <- filtered_orgs[filtered_orgs$submission.count > 1, ]
    filtered_selected_props <- filtered_props[filtered_props$`proposal.status` == "SELECTED", ]
    paste0(
      "<h5>All Programs Linked Organization Statistics</h5>",
      length(unique(props$linked.org)), " organizations were involved in proposal submissions.<br>",
      length(unique(selected_props$linked.org)), " organizations were involved in proposal selections.<br><br>",
      nrow(multiple_apps), " organizations were involved in multiple submissions.<br>",
      round(sum(orgs$submission.count > 1) / nrow(orgs) * 100, 2), "% of organizations were involved in multiple submissions.<br>",
      nrow(multiple_apps[multiple_apps$`selection.count` > 0, ]), " organizations were involved in multiple selections<br>",
      round(nrow(multiple_apps[multiple_apps$`selection.count` > 0, ]) / nrow(multiple_apps) * 100, 2), "% of organizations were involved in multiple selections.<br><br><h5>Filtered Programs Linked Organization Statistics</h5>",
      length(unique(filtered_props$linked.org)), " organizations were involved in proposal submissions.<br>",
      length(unique(filtered_selected_props$linked.org)), " organizations were involved in proposal selections.<br><br>",
      nrow(filtered_multiple_apps), " organizations were involved in multiple submissions.<br>",
      round(sum(filtered_orgs$submission.count > 1) / nrow(filtered_orgs) * 100, 2), "% of organizations were involved in multiple submissions.<br>",
      nrow(filtered_multiple_apps[filtered_multiple_apps$`selection.count` > 0, ]), " organizations were involved in multiple selections<br>",
      round(nrow(filtered_multiple_apps[filtered_multiple_apps$`selection.count` > 0, ]) / nrow(filtered_multiple_apps) * 100, 2), "% of organizations were involved in multiple selections.<br><br>"
    )
  }
}

#' @title Print Proposal Participation Statistics
#' @description
#' Function printing information related to all program and filtered programs' NASA and internatiobal proposal participation.
#' @param props data frame containing all proposals
#' @param filtered_props data frame containing only proposals from filtered programs
#' @returns A concatenated string to be printed in the Proposal Analysis tab when the `Proposal Participation` option is selected in the view drop down.
#' @usage print_participation(nspires$proposals, nspires$proposals[nspires$proposals$`program name` %in% filtered_programs(), ])
#' @details
#' The following information is printed for both all proposals and filtered proposals:\n
#' 1. Number of submitted proposals with NASA staff participation\n
#' 2. Percentage of submitted proposals with NASA staff participation\n
#' 3. Number of selected proposals with NASA staff participation\n
#' 4. Percentage of selected proposals with NASA staff participation\n
#' 5. Number of submitted proposals with international participation\n
#' 6. Percentage of submitted proposals with international participation\n
#' 7. Number of selected proposals with international participation\n
#' 8. Percentage of selected proposals with international participation\n
print_participation <- function(props, filtered_props) {
  selected_props <- props[props$`proposal status` == "SELECTED", ]
  international_submitted <- nrow(props[props$`international participation` == "true", ])
  international_selected <- nrow(selected_props[selected_props$`international participation` == "true", ])
  nasa_submitted <- nrow(props[props$`nasa staff` == "true", ])
  nasa_selected <- nrow(selected_props[selected_props$`nasa staff` == "true", ])
  
  selected_filtered_props <- filtered_props[filtered_props$`proposal status` == "SELECTED", ]
  filtered_international_submitted <- nrow(filtered_props[filtered_props$`international participation` == "true", ])
  filtered_international_selected <- nrow(selected_filtered_props[selected_filtered_props$`international participation` == "true", ])
  filtered_nasa_submitted <- nrow(filtered_props[filtered_props$`nasa staff` == "true", ])
  filtered_nasa_selected <- nrow(selected_filtered_props[selected_filtered_props$`nasa staff` == "true", ])
  
  paste0(
    "<h5>All Programs Participation Statistics</h5>",
    nasa_submitted, " submitted proposals had NASA Staff participation.<br>",
    round(nasa_submitted / nrow(props) * 100, 0), "% of submitted proposals had NASA staff participation.<br>",
    nasa_selected, " selected proposals had NASA Staff participation.<br>",
    round(nasa_selected / nrow(selected_props) * 100, 0), "% of selected proposals had NASA staff participation.<br><br>",
    international_submitted, " submitted proposals had international participation.<br>",
    round(international_submitted / nrow(props) * 100, 2), "% of submitted proposals had international participation.<br>",
    international_selected, " selected proposals had international participation.<br>",
    round(international_selected / nrow(selected_props) * 100, 2), "% of selected proposals had international participation.<br><br><h5>Filtered Programs Participation Statistics</h5>",
  
    filtered_nasa_submitted, " submitted proposals had NASA Staff participation.<br>",
    round(filtered_nasa_submitted / nrow(filtered_props) * 100, 0), "% of submitted proposals had NASA staff participation.<br>",
    filtered_nasa_selected, " selected proposals had NASA Staff participation.<br>",
    round(filtered_nasa_selected / nrow(selected_filtered_props) * 100, 0), "% of selected proposals had NASA staff participation.<br><br>",
    filtered_international_submitted, " submitted proposals had international participation.<br>",
    round(filtered_international_submitted / nrow(filtered_props) * 100, 2), "% of submitted proposals had international participation.<br>",
    filtered_international_selected, " selected proposals had international participation.<br>",
    round(filtered_international_selected / nrow(selected_filtered_props) * 100, 2), "% of selected proposals had international participation.<br><br>"
  )
}
