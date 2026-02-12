#' @importFrom grDevices blues9 col2rgb colorRampPalette
#' @importFrom grDevices dev.print gray palette pdf rgb xy.coords
#' @importFrom ribiosUtils assertFile basefilename haltifnot
#' @importFrom RColorBrewer brewer.pal brewer.pal.info 
#' @importFrom grid grid.newpage grid.text unit gpar
#' @importFrom lattice panel.xyplot
#' @import ggplot2
#' @import graphics
#' @import stats
NULL

#' Compact par setting
#' 
#' For compact figures
#' 
#' @param mar marginal option passed to \code{par}
#' @param mgp margin line option passed to \code{par}
#' @param ... other parameters passed to \code{par}
#' @author Jitao David Zhang
#' @seealso \code{\link{par}}
#' @examples
#' 
#'   compactPar()
#'   plot(1:4)
#' 
#' @export compactPar
compactPar<- function(mar=c(3,3,1.5,1.5), mgp=c(2,1,0),...) return(par(mar=mar, mgp=mgp, ...))

#' Plan a square/matrix layout of plots
#' 
#' 
#' @param n Number of plots
#' @return A vector of integers of length 2. Can be passed to \code{layout} or
#' \code{mfrow} in \code{par} to make the layout.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#'   \dontrun{
#'     op <- par(mfrow=squareLayout(7))
#'     plot(1:5)
#'     plot(2:6)
#'     plot(3:7)
#'     plot(-9:-4)
#'     plot(8:5)
#'     plot(5:1)
#'     plot(1:9)
#'     par(op)
#'   }
#' 
#' @export squareLayout
squareLayout <- function(n) {
  ncol <- ceiling(sqrt(n))
  nrow <- n %/% ncol+ ifelse(n %% ncol >0, 1, 0)
  return(c(nrow, ncol))
}

#' Return a symmetric range
#' @param x A numeric vector
#' @param mid Number, the mid point
#' @return A vector of two numbers, a symmetric range with \code{mid} in the middle
#' @export
symrange <- function(x, mid=0) {
  xrange <- range(x[!is.infinite(x)], na.rm=TRUE)
  maxabs <- max(abs(xrange-mid))
  return(c(mid-maxabs, mid+maxabs))
}

#' Make sure that x is assigned a reasonable value
#' @param x Any vector
#' @param default A default value
#' @param length Desired length
#' @param defaultNULL.ok Logical, whether the default can be \code{NULL} or not
#' 
#' @return non-null values
#' @export
nonNull <- function(x, default, length=NULL, defaultNULL.ok=FALSE) {
  if(is.null(default) & !defaultNULL.ok)
    stop("'default' is not allowed to be NULL")
  if(is.null(x)) {
    res <- default
  } else {
    res <- x
  }
  if(!missing(length))
    res <- rep(res, length.out=length)
  return(res)
}

isInvalid <- function (x) {
    if (missing(x) || is.null(x) || length(x) == 0) 
        return(TRUE)
    if (is.list(x)) 
        return(all(sapply(x, isInvalid)))
    else if (is.vector(x)) 
        return(all(is.na(x)))
    else return(FALSE)
}



#' Interative dev.print and pdf print
#' 
#' Execute dev.print only if R session is interactive.
#' 
#' \code{ipdf} is a shortcut in case PDF is used as the device, with the twist
#' that \code{useDingbats} is set to \code{FALSE} by default. See NOTE.
#' 
#' \code{dev.print} will make a R-script fail if the session is not interactive
#' (e.g. when the script is excuted with the \code{-f} option from \code{R}
#' command line). Function \code{idev} checks first whether the session is
#' interative, and executes \code{dev.print} only if the session is
#' interactive.
#' 
#' A commonly used shortcut is \code{ipdf}, which prints the current device to
#' a PDF file.
#' 
#' @aliases idev ipdf
#' @param \dots Parameters passed to \code{\link{dev.print}}
#' @param file PDF file name
#' @return Side effect used.
#' @note \code{useDingbats} is set to \code{FALSE} in \code{ipdf}. Setting the
#' option to \code{TRUE} causes problem in importing the PDF to Inkscape, a
#' vector-based figure modifying software. Though the option may reduces
#' smaller and (according to the R manual) better output, we have noticed no
#' difference.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{dev.print}}, \code{\link{pdf}}
#' @examples
#' 
#' tmfile <- tempfile()
#' plot(1:15, type="h")
#' idev(png, tmfile,width=600, height=800)
#' ipdf(tmfile)
#' 
#' @export idev
idev <- function(...) {
  if(interactive())
    dev.print(...)
}

#' @rdname idev
#' @export
ipdf <- function(file, ...) {
  if(interactive())
    dev.print(pdf, file=file, useDingbats=FALSE,...)
}


#' Use 'convert' (ImageMagick) to convert PDF to high-quality PNG
#' 
#' The function makes a system call to convert PDF files to high-quality (300
#' dpi) PNG files.
#' 
#' 
#' @param \dots PDF files
#' @param convert Name of the convert program. It is overwritten if the program
#' is running on the udis machine (rbaus024). See the code for more details.
#' @param density DPI. Default 300 is good enough for publications in most
#' biology/medicine journals
#' @param outdir Output directory. If the value is \code{NULL}, the output
#' files will be written in the same directory as the input file
#' @param outfile Output file names. If the value is \code{NULL}, the output
#' file names will be basename of the input PDF files appended with the
#' \code{.png} suffix. If given, its length must equal the length of PDF files.
#' @param wait Logical, should the function wait until the conversion is
#' finished?
#' @return Output file names are returned invisibly.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#'   tmpdir <- tempdir()
#' 
#'   pdffile <- file.path(tmpdir, "test-plot.pdf")
#'   pdf(pdffile)
#'   plot(1:5)
#'   dev.off()
#' 
#'   pdf2png(pdffile)
#'   pdf2png(pdffile, outfile="test.png")
#' 
#'   testfile <- system.file("/doc/intro.pdf", package="limma")
#'   if(file.exists(testfile)) {
#'     pdf2png(testfile, outdir=tmpdir)
#'     Sys.sleep(1)
#'     dir(tmpdir)
#' 
#'    ## or waiting
#'     pdf2png(testfile, outdir=tmpdir, wait=TRUE)
#'   }
#' 
#' @export pdf2png
pdf2png <- function(..., convert="convert", density=300, outdir=NULL, outfile=NULL, wait=FALSE) {
  files <- unlist(list(...))
  assertFile(files)
  if(is.null(outdir)) outdir <- dirname(files)
  if(is.null(outfile)) {
    outfile <- file.path(outdir,
                         sprintf("%s.png", basefilename(files)))
  } else {
    haltifnot(length(outfile)==length(files))
  }

  ## mon udis machine, ghostscript has to be added to the path
  if(Sys.info()[["nodename"]]=="rbaus024.bas.roche.com") {
    convert <- "PATH=/apps64/ghostscript-9.10/bin/:/apps64/ImageMagick-6.7.5-4/bin/:${PATH} convert"
  }
  comms <- sprintf("%s -density %d %s %s", convert, density, files, outfile)
  for(i in seq(along=comms))
    system(comms[i], wait=wait)
  return(invisible(outfile))
}

#' Return a range defined by integers
#' 
#' The function is similar to \code{\link{range}} but returns integer ranges
#' that are just outside the real range: i.e. the floor of the left range and
#' the ceiling of the right range.
#' 
#' @param x A numeric vector
#' @param na.rm Logical, whether NA should be removed
#' @return A vector of integers of length 2.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' intRange(rnorm(100))
#' 
#' @export intRange
intRange <- function(x, na.rm=TRUE) {
  range <- range(x, na.rm=na.rm)
  range[1] <- floor(range[1])
  range[2] <- ceiling(range[2])
  return(range)
}


#' Get xlim/ylim ranges for plots from real values
#' 
#' 
#' @param ... one or more vectors of real values
#' @param perc percentage of dynamic range that should be covered by the
#' limits; if set to 1 the whole range is used.
#' @param symm logical value; if set to \code{TRUE}, the range will be
#' symmetric around zero
#' @examples
#' 
#' myX <- rnorm(100, mean=1)
#' myY <- rnorm(100)
#' myLim <- getLims(myX, myY, perc=0.99)
#' plot(myX, myY, xlim=myLim, ylim=myLim)
#' mySymmLim <- getLims(myX, myY, perc=0.99, symm=TRUE)
#' plot(myX, myY, xlim=myLim, ylim=mySymmLim)
#' 
#' 
#' @export getLims
getLims <- function(..., perc=0.99, symm=TRUE) {
    lower <- (1-perc)/2
    higher <- 1-lower
    allVal <- list(...)
    quant <- quantile(unlist(allVal), c(lower, higher), na.rm=TRUE)
    if(symm) {
        quantAbsMax <- max(abs(quant))
        quant <- c(-quantAbsMax, quantAbsMax)
    }
    return(quant)
}
