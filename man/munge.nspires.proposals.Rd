% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/munge.nspires.proposals.R
\name{munge.nspires.proposals}
\alias{munge.nspires.proposals}
\title{Munge internal NSPIRES proposal database (mostly the column names....)}
\usage{
munge.nspires.proposals(df)
}
\arguments{
\item{df}{Data from the internal-access-only NSPIRES. Currently this function only really supports the "proposal_master" files}
}
\description{
returns an object of class data.frame. Removes all the "LINKED"   "PENDING"  "REJECTED" "LOCKED" proposals.
}
