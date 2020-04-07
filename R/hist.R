#' Histogram with quantile line(s) and text(s)
#' 
#' A handy function to plot histogram with display elements of quantile.
#' 
#' The appends vertical lines and texts to histograms produced by \code{hist}.
#' This can be useful in unspecific filtering of expression data.
#' 
#' @param x Value to draw the histogram
#' @param quantiles Numeric values or \code{NULL}; in case of numeric values,
#' at the corresponding quantile values vertical lines and text labels are
#' drawn; if set to \code{NULL}, no extra items will display. See examples
#' below.
#' @param breaks Integer, number of breaks
#' @param qlty Type of vertical quantile lines
#' @param qlwd Width of vertical quantile lines
#' @param qcol Color of vertical quantile lines
#' @param \dots Other parameters that are passed to \code{hist}
#' @return The object returned by the \code{hist} function, with an extra item
#' named \code{quantiles}.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{hist}
#' @examples
#' 
#' testVal <- rnorm(1000)
#' hist(testVal)
#' qHist(testVal, quantiles=c(0.25, 0.75), border="lightgray")
#' 
#' @export qHist
qHist <- function(x,quantiles=0.25, breaks=100,
                  qlty=2, qlwd=2, qcol="red", ...) {
  res <- hist(x, breaks=breaks, ...)
  if(!is.null(quantiles)) {
    qs <- quantile(x, quantiles, na.rm=TRUE)
    abline(v=qs, lty=qlty, lwd=qlwd, col=qcol)
    qsLabels <- sprintf("Q%d=%2.2f",
                        as.integer(quantiles*100),
                        qs)
    qsY <- par("usr")[4]
    text(qs+strwidth("M", "user"), qsY, qsLabels,
         srt=90, adj=c(1,1), col=qcol, font=2)
    res$quantiles <- qs
  }
  return(invisible(res))
}

## xclip hist: histogram with x-axis clipped with quantiles


#' Internal function to re-calculate breaks of histograms when x-axis is
#' clipped
#' 
#' The function calculates the new break numbers caused by the clipping of x
#' axis. This is usally larger than the original number of breaks .
#' 
#' @param x Value to draw histgrams with
#' @param quantiles Quantiles of x that determine the clip boundary of x-axis
#' @param breaks Integer, number of breaks applied to original data
#' @return Integer: number of breaks
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso This function is directly used by \code{qHist}
#' @examples
#' 
#' \dontrun{
#' testVal <- rnorm(1000)
#' qBreaks(testVal, quantiles=c(0.25, 0.75), breaks=100) ## should be about
#' 400
#' }
#' 
qBreaks <- function(x,quantiles=c(0,0.99), breaks=100) {
  haltifnot(length(quantiles)==2 & quantiles[2]>quantiles[1],
            msg="quantiles must be a vector of two numbers with quantiles[2]>quantiles[1]")
  qts <- quantile(x, quantiles, na.rm=TRUE)
  subbreak <- as.integer((max(x, na.rm=TRUE)-min(x, na.rm=TRUE))/(qts[2]-qts[1]))*breaks
  return(subbreak)
}



#' Histogram with clipped x axis
#' 
#' Draw histograms with clipped x axis; the clipping is determined by quantiles
#' of x values.
#' 
#' The function clips (subsets) x-axis and recalcualte the breaks so that the
#' clipped image looks like a real subset of the original data.
#' 
#' @param x Value to draw the histogram
#' @param xclip Quantiles of x-values that should be displayed; values outside
#' of this range are not shown in the histogram
#' @param breaks A integer number indicating how many breaks should the
#' \emph{original unclipped} data have; the function will automatically
#' re-calculate the breaks of the clipped data so that they look consistent.
#' @param quantiles Numeric values or \code{NULL}; in case of numeric values,
#' at the corresponding quantile values vertical lines and text labels are
#' drawn; if set to \code{NULL}, no extra items will display. See examples
#' below.
#' @param qlty Type of vertical quantile lines
#' @param qlwd Width of vertical quantile lines
#' @param qcol Color of vertical quantile lines
#' @param \dots Other parameters that are passed to \code{hist}
#' @return The object returned by the \code{hist} function, with an extra item
#' named \code{quantiles}.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{qHist}, which draws quantile line and texts onto histograms.
#' @examples
#' 
#' testVal <- c(rnorm(1000),10)
#' hist(testVal, breaks=100)
#' xclipRes <- xclipHist(testVal, xclip=c(0.001, 0.999), quantiles=0.50)
#' 
#' xclipRes$quantiles
#' 
#' @export xclipHist
xclipHist <- function(x, xclip=c(0.01, 0.99), breaks=100,
                     quantiles=0.25, qlty=2, qlwd=2, qcol="red",...) {
  haltifnot(length(xclip)==2 & xclip[2]>xclip[1],
            msg="xclip must be a vector of two numbers with xclip[2]>xclip[1]")
  xBreaks <- qBreaks(x, quantiles=xclip, breaks=breaks)
  xlim <- quantile(x, xclip, na.rm=TRUE)
  qHist(x, breaks=xBreaks, xlim=xlim, quantiles=quantiles,
        qlty=qlty, qlwd=qlwd, qcol=qcol, ...)
}

## histogram of matrix


#' Make histograms for matrix
#' 
#' Make histograms for matrix
#' 
#' 
#' @param mat A numerical matrix
#' @param linesOpt Line options
#' @param main Title text
#' @param xlab Xlab
#' @param xlim Xlim
#' @param \dots Other parameters passed to \code{hist}
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{hist}}
#' @examples
#' 
#' testMat <- matrix(rnorm(1000), nrow=100)
#' histMat(testMat)
#' 
#' @export histMat
histMat <- function(mat,
                    linesOpt=list(lwd=NULL, col=NULL,lty=NULL, type=NULL, pch=NULL),
                    main=NULL, xlab=NULL, xlim=NULL,
                    ...) {

  if(!is.matrix(mat)) {
    stop("input must be a numeric matrix")
  }
  xrange <- symrange(mat, mid=0)
  mat.dens <- apply(mat, 2, density, na.rm=TRUE)
  
  if(missing(main)) 
    main <- as.character(substitute(mat))
  if(missing(xlab))
    xlab <- ""
  if(missing(xlim))
    xlim <- xrange
  
  oh <- hist(mat, freq=FALSE, xlab=xlab, main=main, xlim=xlim, ...)

  lines.col <- nonNull(linesOpt$col, palette(), ncol(mat))
  lines.lwd <- nonNull(linesOpt$lwd, 1L, ncol(mat))
  lines.type <- nonNull(linesOpt$type, "l", ncol(mat))
  lines.lty <- nonNull(linesOpt$lty, 1L, ncol(mat))
  lines.pch <- nonNull(linesOpt$pch, 1L, ncol(mat))
  
  for(i in 1:ncol(mat)) {
    lines(mat.dens[[i]], col=lines.col[i], lwd=lines.lwd[i],
          type=lines.type[i], lty=lines.lty[i], pch=lines.pch[i])
  }
  lopt <- list(col=lines.col, lwd=lines.lwd, type=lines.type,
               lty=lines.lty, pch=lines.pch)

  oh$xlim <- xlim
  oh$linesOpt <- lopt
  return(invisible(oh))
}
