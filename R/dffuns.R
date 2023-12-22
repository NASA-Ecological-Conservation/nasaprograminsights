#' @title Get PI DF
#' @description
#' Function aggregating PI SUIDs by proposal number.
#' @param df data frame containing PI SUIDs
#' @returns A data frame containing information about every individual PI passed into the function
#' @usage get_pi_df(nspires$proposals)
#' @details
#' For each PI, the proposal numbers of the proposals the PI has submitted are stored, the number of proposals they have submitted, the number of proposals of theirs that have been selected, and their selection percentage (number of selections / submissions * 100).
get_pi_df <- function(df) {
  pi_df <- aggregate(`proposal.number` ~ `pi.suid`, df, FUN = function(x) paste(x, collapse = ", "))
  names(pi_df)[2] <- "submitted.proposals"
  pi_submission_count <- aggregate(`proposal.number` ~ `pi.suid`, df, FUN = function(x) length(x))
  pi_df <- merge(pi_df, pi_submission_count, by = "pi.suid")
  names(pi_df)[3] <- "submission.count"
  pi_selection_count <- aggregate(`proposal.status` ~ `pi.suid`, df, FUN = function(x) sum(x == "SELECTED"))
  pi_df <- merge(pi_df, pi_selection_count, by = "pi.suid")
  names(pi_df)[4] <- "selection.count"
  pi_df$`selection.percentage` = round(pi_df$`selection.count` / pi_df$`submission.count` * 100, 0)
  return(pi_df)
}

#' @title Get Org DF
#' @description
#' Function aggregating linked organizations by proposal number.
#' @param df data frame containing linked org names
#' @returns A data frame containing information about every individual linked organization passed into the function
#' @usage get_org_df(nspires$proposals)
#' @details
#' Similar to the `get_pi_df` function. For each organization, the proposal numbers of the proposals the organization has submitted are stored, the number of proposals it has submitted, the number of proposals that have been selected, and the selection percentage (number of selections / submissions * 100).
get_org_df <- function(df) {
  org_df <- aggregate(`proposal.number` ~ linked.org, df, FUN = function(x) paste(x, collapse = ", "))
  names(org_df)[2] <- "submitted.proposals"
  org_submission_count <- aggregate(`proposal.number` ~ linked.org, df, FUN = function(x) length(x))
  org_df <- merge(org_df, org_submission_count, by = "linked.org")
  names(org_df)[3] <- "submission.count"
  org_selection_count <- aggregate(`proposal.status` ~ linked.org, df, FUN = function(x) sum(x == "SELECTED"))
  org_df <- merge(org_df, org_selection_count, by = "linked.org")
  names(org_df)[4] <- "selection.count"
  org_df$selection.percentage = round(org_df$selection.count / org_df$submission.count * 100, 2)
  org_pi_count <- aggregate(pi.suid ~ linked.org, df, FUN = function(x) length(unique(x)))
  org_df <- merge(org_df, org_pi_count, by = "linked.org")
  names(org_df)[6] <- "pi.count"
  return(org_df)
}
