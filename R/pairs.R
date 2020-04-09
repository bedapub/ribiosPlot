#' Correlation panel for pairs
#'
#' @param x A numeric vector
#' @param y A numeric vector, must be of the same length as \code{x}.
#' @param digits Integer, number of digits to show
#' @param prefix Prefix of the label
#' @param cex.cor Numeric, if missing, automatically guessed
#' @param ... Passed to \code{cor}.
#'
#' This function can be used with \code{pairs} to display correlations.
#' @seealso \code{\link[graphics]{pairs}}. 
#' @importFrom stats cor
#' @export
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  corr <- cor(x,y, ...)
  r <- abs(corr)
  scale.factor <- ifelse(r<0.2, 0.2, r)
  col <- ifelse(corr>0, "red2", "royalblue")
  txt <- format(c(corr, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * scale.factor, col=col)
}

#' Correlation panel
#' @param x A numeric vector
#' @param y A numeric vector, must be of the same length as \code{x}.
#' @param col Color
#' @param bg Background
#' @param pch Ponit symbol
#' @param cex Font size
#' @param method Correlation method, passed to \code{\link[stats]{cor}}
#' @param use passed to \code{cor}
#' @param ... Passed to \code{\link[graphics]{panel.smooth}}
#'
#' This function can be used with \code{pairs} to display correlations.
#'
#' @seealso \code{\link[graphics]{pairs}}. 
#' @importFrom stats cor lm
#' @importFrom graphics panel.smooth legend abline
#' @export
panel.lmSmooth <- function(x,y, col = par("col"), bg = NA, pch = par("pch"), 
                      cex = 0.8, method="spearman", use="complete", ...) {

  corr <- cor(x,y, method=method, use=use)
  corr.col <-ifelse(corr<0, "royalblue", "red2")

  abline(h=0, v=0, col="lightgray")
  abline(lm(y~x))
  
  panel.smooth(x,y, col=col, bg=bg, pch=pch,
               cex=cex, col.smooth=corr.col, ...)

  legend("topleft",
         sprintf("r=%1.2f", corr), bty="n",
         text.col=corr.col)
}
