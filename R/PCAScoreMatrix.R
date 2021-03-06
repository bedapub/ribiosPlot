##------------##
## S3: PCAScoreMatrix
##------------##

#' Construct a S3-class PCAScoreMatrix object
#' 
#' 
#' @param scoreMatrix Numeric matrix, objects in rows and PCs in columns
#' @param expVar Numeric vector, length must equal the number of columns of
#' \code{scoreMatrix}, explained variance by respective PCs
#' @return A S3-class \code{PCAScoreMatrix} object, which is a score matrix
#' with explained variances (expVar) as attribute.
#' @seealso \code{as.matrix.PCAScoreMatrix}, \code{expVar.PCAScoreMatrix},
#' \code{print.PCAScoreMatrix}. This function is usually not called by the end
#' user; instead, it is used by the function \code{\link{pcaScores}}
#' @examples
#' 
#' myPCmat <- PCAScoreMatrix(matrix(rnorm(15),ncol=3), c(0.25, 0.15, 0.1))
#' myPCmat
#' 
#' @export PCAScoreMatrix
PCAScoreMatrix <- function(scoreMatrix, expVar) {
  stopifnot(ncol(scoreMatrix)==length(expVar))
  attr(scoreMatrix, "expVar") <- expVar
  class(scoreMatrix) <- "PCAScoreMatrix"
  return(scoreMatrix)
}



#' Coerece a PCAScoreMatrix into score matrix
#' 
#' 
#' @param x A \code{PCAScoreMatrix} S3 object
#' @param ... Currently ignored
#' @return A numeric matrix, the score matrix
#' @examples
#' 
#' myPCmat <- PCAScoreMatrix(matrix(rnorm(15),ncol=3), c(0.25, 0.15, 0.1))
#' as.matrix(myPCmat)
#' 
#' @export
as.matrix.PCAScoreMatrix <- function(x, ...) {
  attr(x, "expVar") <- NULL
  class(x) <- "matrix"
  return(x)
}

#' Subsetting PCAScoreMatrix while keeping the expVar attribute
#' @param x A \code{PCAScoreMatrix} object
#' @param i Integer or logical vector, subsetting rows
#' @param j Integer or logical vector, subsetting columns
#' @param ... Not used
#' @param drop Logical, whether to drop dimensions if only one column is left
#' @return A \code{PCAScoreMatrix} object
#' @export
`[.PCAScoreMatrix` <- function(x, i, j, ..., drop=TRUE) {
  res <- as.matrix(x)[i,j,...,drop=drop]
  if(missing(j)) {
    attr(res, "expVar") <- attr(x, "expVar")
  } else {
    attr(res, "expVar") <- attr(x, "expVar")[j]
  }
  class(res) <- "PCAScoreMatrix"
  return(res)
}

#' Coerece a PCAScoreMatrix into data.frame
#' 
#' 
#' @param x A \code{PCAScoreMatrix} S3 object.
#' @param row.names See \code{as.data.frame}.
#' @param optional See \code{as.data.frame}.
#' @param ... See \code{as.data.frame}
#' @return A data.frame consisting of the score matrix
#' @seealso \code{\link{as.data.frame}}
#' @examples
#' 
#' myPCmat <- PCAScoreMatrix(matrix(rnorm(15),ncol=3), c(0.25, 0.15, 0.1))
#' as.matrix(myPCmat)
#' 
#' @export
as.data.frame.PCAScoreMatrix <- function(x,
                                         row.names=NULL,
                                         optional=FALSE,
                                         ...) {
  res <- as.data.frame(as.matrix(x), 
                       row.names=row.names, optional=optional, ...)
  return(res)
}


#' Print PCAScoreMatrix
#' 
#' 
#' @param x A \code{PCAScoreMatrix} S3-object
#' @param ... Ignored
#' @return NULL, side effect is used
#' @examples
#' 
#' myPCmat <- PCAScoreMatrix(matrix(rnorm(15),ncol=3), c(0.25, 0.15, 0.1))
#' myPCmat
#' 
#' @export
print.PCAScoreMatrix <- function(x, ...) {
  expVar <- attr(x, "expVar")
  cat(sprintf("PCAScoreMatrix with %d dimensions\n", length(expVar)))
  print(as.matrix(x))
  cat("Explained variances:", paste(ribiosUtils::percentage(expVar), 
                                    collapse=","),
      sprintf("(%s in total)\n", ribiosUtils::percentage(sum(expVar))))
  cat("Options\n")
  cat("-- Use 'as.matrix' to turn this object into a simple matrix\n")
  cat("-- Use 'expVar' to extract explained variances\n")
  cat("-- Use 'expVarLabel' to generate labels of explained variances")
}
