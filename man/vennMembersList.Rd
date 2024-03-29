% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vennMembers.R
\name{vennMembersList}
\alias{vennMembersList}
\title{Extract members of each region in Venn diagrams in to a list}
\usage{
vennMembersList(venn, removeNULL = TRUE)
}
\arguments{
\item{venn}{A Venn object}

\item{removeNULL}{Logical, whether NULL elements should be removed}
}
\value{
A named list of members, each list item corresponding to a region in 
  Venn diagrams
}
\description{
Extract members of each region in Venn diagrams in to a list
}
\examples{
if(requireNamespace("Vennerable")) {
  myList <- list(A=LETTERS[1:5], B=LETTERS[2:7], C=LETTERS[seq(2,9,2)])
  myVenn <- Vennerable::Venn(myList)
  myVennList <- vennMembersList(myVenn)
}
}
