#' Robust distance
#' 
#' A wrapper function for the \code{dist} function in the \code{stats} package.
#' It replaces NA values in the distance matrix by the maximum distance in the
#' same matrix, therefore prevents cases where hclust fails because of
#' \code{NA} distances.
#' 
#' In the rare case of all-NA distance matrices, all values are assigned
#' arbitrarily to one.
#' 
#' @param x a numeric matrix, data frame or \sQuote{dist} object
#' @param \dots Other parameters passed to the \code{dist} function.
#' @return The same as \code{\link{dist}}, however without \code{NA}s.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' mymat <- matrix(c(3,2,1,NA,NA,NA,
#'                 4,1,2,NA,NA,NA,
#'                 NA,NA,NA,5,1,2), ncol=6, byrow=TRUE)
#' dist(mymat)
#' robustDist(mymat)
#' 
#' @export robustDist
robustDist <- function(x,...) {
  res <- dist(x,...)
  isBad <- is.na(res) | is.nan(res) | is.infinite(res)
  
  res.max <- max(res[!isBad], na.rm=TRUE)
  if(is.na(res.max)) res.max <- 1L

  res[isBad] <- res.max
  return(res)
}
