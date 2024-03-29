% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expVar.R
\name{expVar}
\alias{expVar}
\alias{expVar.prcomp}
\alias{expVar.PCAScoreMatrix}
\title{S3 function expVar to extract explained variance from prcomp and
PCAScoreMatrix objects}
\usage{
expVar(x, choices)

\method{expVar}{prcomp}(x, choices)

\method{expVar}{PCAScoreMatrix}(x, choices)
}
\arguments{
\item{x}{A \code{prcomp} or \code{PCAScoreMatrix} object.}

\item{choices}{Either missing, or an integer vector of indices, indicating
which PCs should be returned.}
}
\value{
A numeric vector of variance explained
}
\description{
S3 function expVar to extract explained variance from prcomp and
PCAScoreMatrix objects
}
\section{Methods (by class)}{
\itemize{
\item \code{expVar(prcomp)}: Extract explained variance from a prcomp object

\item \code{expVar(PCAScoreMatrix)}: Extract explained variance from a PCAScoreMatrix object

}}
\section{Methods (by class)}{
 \itemize{ \item \code{prcomp}: Extract
explained variance from a prcomp object

\item \code{PCAScoreMatrix}: Extract explained variance from a
PCAScoreMatrix object }
}

\examples{

myMat <- matrix(rnorm(100), ncol=10)
myPrcomp <- prcomp(myMat)
myPcaScoreMatrix <- pcaScores(myPrcomp, choices=NULL) 
expVar(myPrcomp)
expVar(myPcaScoreMatrix)

expVar(myPrcomp, 1:5)
expVar(myPcaScoreMatrix, 1:5)

}
