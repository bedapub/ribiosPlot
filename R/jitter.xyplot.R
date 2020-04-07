#' Make boxplots or dotplots with sample-size proportional jitters
#' 
#' Make boxplots or dotplots with jitters of the size proportional to the
#' sample size. See examples.
#' 
#' 
#' @param x X-axis variable, numeric or factor
#' @param y Y-axis variable, numeric
#' @param N Number of groups that y-axis should be cut
#' @param factor Jitter factor, passed to \link{jitter}
#' @param \dots Other parameters passed to \code{panel.xyplot}
#' @return Side effects are used.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' library(lattice)
#' 
#' testX <- gl(8,5)
#' testY <- rnorm(40)
#' xyplot(testY ~ testX)
#' xyplot(testY ~ testX, panel=jitter.xyplot)
#' 
#' (xyBw <- bwplot(testY ~ testX))
#' (xyBwJitter <- update(xyBw, panel=jitter.xyplot))
#' 
#' testXnum <- rep(1:8, 5)
#' xyplot(testY ~ testXnum, panel=jitter.xyplot)
#' 
#' @export jitter.xyplot
jitter.xyplot <- function(x,y, N=20, factor=1, ...) {
  if(!is.factor(x)) {
    xfac <- factor(x, levels=unique(x))
  } else {
    xfac <- x
  }
  facs.raw <- tapply(y,xfac, function(y0) {
    cuts <- cut(y0, breaks=pmin(as.integer(length(y)/5),N))
    clen <- ave(seq(along=cuts), cuts, FUN=length)
    clen/max(clen, na.rm=TRUE)*factor
  })
  facOrd <- match(1:length(y), unlist(split(1:length(y), xfac)))
  facs <- unlist(facs.raw)[facOrd]
  facs[is.na(facs)] <- 0
  if(!is.numeric(x))
    x <- as.numeric(x)
  xnew <- jitter(x, factor=facs)
  panel.xyplot(xnew, y, ...)
}
