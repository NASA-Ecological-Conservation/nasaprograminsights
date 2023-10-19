# Setup -------------------------------------------------------------------
pkgs <- c("dplyr", "ggplot2", "readr", "pdftools", "xlsx", "stringr")
new.packages <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(dplyr, quietly=TRUE)
library(ggplot2, quietly=TRUE)


# Identify Proposal PDF filenames -----------------------------------------
# files/dirs
library(tm)
pdfs <- tolower(list.files(path = "data-raw/data-raw-internal/nspires-internal/proposals/", pattern=".pdf", recursive = TRUE, full.names = TRUE))

bdecprops <- pdfs[which(stringr::str_detect(string =pdfs , pattern=paste("-econ", "-ecof", "-biodiv", "-eco4cast", sep="|")))]


# import pdf(s)
PDF <- list()
for(i in seq_along(bdecprops)){
  PDF[[i]] <-   pdftools::pdf_text(bdecprops[i]) %>%
                  readr::read_lines()  |> #open the PDF inside your project folder
                  stringr::str_squish() |>
                  strsplit(split = "\n")
}
PDF <- lapply(PDF, unlist)

# Get props with 'endangered' or FWS terms...
fwsprops <- which(grepl("fws|usfws|fish and wildlife service", PDF, ignore.case = TRUE))
esprops <- which(grepl("endangered", PDF, ignore.case = TRUE))

snippets <- list()
for(i in seq_along(esprops)){
  temp <- which(grepl("endangered", PDF[[esprops[i]]], ignore.case = TRUE))

  snips<-NULL
  for(j in seq_along(temp)){
    snips <- c(PDF[[esprops[i]]][temp[j]],
    PDF[[esprops[i]]][temp[j-1]],
    PDF[[esprops[i]]][temp[j+1]], snips)
    snips <- as.vector(na.omit(snips))
  }

snippets[[i]] <- c(snippets, snips)

}


# TRASH? ------------------------------------------------------------------
# # ## build corpus
# library(tm)
# b <- a1  |>  VectorSource()  |> Corpus()
# # build term doc matrix for PROJET SUMMARY
# a1[(which(stringr::str_detect(tolower(a1), "project summary"))+1):(which(stringr::str_detect(tolower(a1), "other project information"))-1)]
#
# ## build term document matrix
# m <- b %>% TermDocumentMatrix(control=list(wordLengths=c(1, Inf)))
# m %>% inspect()
# ## various term weighting schemes
# m %>% weightBin() %>% inspect() ## binary weighting
# m %>% weightTf() %>% inspect() ## term frequency
# m %>% weightTfIdf(normalize=F) %>% inspect() ## TF-IDF
# m %>% weightTfIdf(normalize=T) %>% inspect() ## normalized TF-IDF
#
# b
