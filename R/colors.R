## colorpanel function was created in the gplots package (CRAN)
## using with the GPL-2 license

#' Generates a set of colors that varies smoothly.
#' 
#' (copied from the \code{colorpanel} man page from the \code{gplots} package.
#' See NOTES below)
#' 
#' The values for \sQuote{low, mid, high} can be given as color names
#' (\sQuote{red}), plot color index (\code{2}=red), and HTML-style RGB,
#' (\dQuote{\#FF0000}=red).
#' 
#' If \sQuote{mid} is supplied, then the returned color panel will consist of
#' \sQuote{n - floor(n/2)} HTML-style RGB elements which vary smoothly between
#' \sQuote{low} and \sQuote{mid}, then between \sQuote{mid} and \sQuote{high}.
#' Note that if \sQuote{n} is even, the color \sQuote{mid} will occur twice at
#' the center of the sequence.
#' 
#' If \sQuote{mid} is omitted, the color panel will vary smoothly beween
#' \sQuote{low} and \sQuote{high}.
#' 
#' @param n Desired number of color elements in the panel
#' @param low Color to use for the lowest value
#' @param mid Color to use for the middle value. It may be ommited
#' @param high Color to use for the highest value
#' @return Vector of HTML-style RGB colors.
#' @note The colorpanel function is copied from the \code{gplots} package
#' (written by Warnes et al.) under the GPL-2 license. The \code{gplots}
#' require heavy dependencies that prevent this function being used in
#' speed-sensitive scenarios, e.g. in command-line tools.
#' @author Originally by Gregory R. Warnes <greg@@warnes.net>. Adapted by Jitao
#' David Zhang <jitao_david.zhang@@roche.com>.
#' @seealso \code{blackyellow} and \code{royalbluered} for two- and three-color
#' panels.
#' @references See \code{gplots} package.
#' @examples
#' 
#' showpanel <- function(col) {
#'   image(z=matrix(1:100, ncol=1), col=col, xaxt="n", yaxt="n")
#' }
#'  
#' par(mfrow=c(3,3))
#'  
#' # two colors only:
#' showpanel(colorpanel(8,low="red",high="green"))
#'  
#' # three colors
#' showpanel(colorpanel(8,"red","black","green"))
#' # note the duplicatation of black at the center, using an odd
#' # number of elements resolves this:
#' showpanel(colorpanel(9,"red","black","green"))
#'  
#' showpanel(greenred(64))
#' showpanel(redgreen(64))
#' showpanel(bluered(64))
#' showpanel(redblue(64))
#' 
#' showpanel(royalbluered(64))
#' showpanel(royalredblue(64))
#' 
#' @importFrom ribiosUtils isOdd
#' @export colorpanel
colorpanel <- function (n, low, mid, high) {
    if (missing(mid) || missing(high)) {
        low <- col2rgb(low)
        if (missing(high)) 
            high <- col2rgb(mid)
        else high <- col2rgb(high)
        red <- seq(low[1, 1], high[1, 1], length = n)/255
        green <- seq(low[3, 1], high[3, 1], length = n)/255
        blue <- seq(low[2, 1], high[2, 1], length = n)/255
    }
    else {
        isodd <- isOdd(n)
        if (isodd) {
            n <- n + 1
        }
        low <- col2rgb(low)
        mid <- col2rgb(mid)
        high <- col2rgb(high)
        lower <- floor(n/2)
        upper <- n - lower
        red <- c(seq(low[1, 1], mid[1, 1], length = lower), seq(mid[1, 
            1], high[1, 1], length = upper))/255
        green <- c(seq(low[3, 1], mid[3, 1], length = lower), 
            seq(mid[3, 1], high[3, 1], length = upper))/255
        blue <- c(seq(low[2, 1], mid[2, 1], length = lower), 
            seq(mid[2, 1], high[2, 1], length = upper))/255
        if (isodd) {
            red <- red[-(lower + 1)]
            green <- green[-(lower + 1)]
            blue <- blue[-(lower + 1)]
        }
    }
    rgb(red, blue, green)
}


## .factorLevels returns the color used along the levels, whereas .factor return 1:1 mapping from levels to colors
brewer.pal.factorLevels <- function(factor, name="Greys") {
  .Deprecated("fcbrewer")
  nlevel <- nlevels(factor)
  ncol <- bound(nlevel, 3L, brewer.pal.info[name, "maxcolors"])
  cols <- brewer.pal(ncol, name)
  if(nlevel<3) {
    cols <- cols[1:nlevel]
  } else if (nlevel>ncol) {
    cols <- colorRampPalette(cols)(nlevel)
  }
  names(cols) <- levels(factor)
  return(cols)
}



#' Build brewer.pal colors from factor (Deprecated)
#' 
#' The functionality has been replaced by \code{fcbrewer}. The functions will
#' be removed in the future release.
#' 
#' The function is useful to build named RGB color values from factors.
#' \code{brewer.pal.factor} return a color-HTML-string vector as the same
#' length of the input factor vector, which is named by the input factor as
#' well. \code{brewer.pal.factorLevels} returns a color vector of the length of
#' the factor level, and the colors are named by the levels. See examples
#' below.
#' 
#' From version 1.1-16, the color palette is automatically reduced/expanded
#' when the levels of input factors underlies or exceeds the minimum and
#' maximum colors. See example below.
#' 
#' @aliases brewer.pal.factor brewer.pal.factorLevels
#' @param factor A factor vector
#' @param name Color panel name to be passed to \code{brewer.pal}
#' @return Named HTML RGB colors.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' \dontrun{
#' myFac <- factor(c("HSV", "BVB", "FCB", "HSV", "BVB", "HSV"))
#' brewer.pal.factor(myFac, name="Set1")
#' brewer.pal.factorLevels(myFac, name="Set1")
#' 
#' myLongFac <- factor(paste("Sample", 1:20))
#' brewer.pal.factor(myLongFac, name="Set1")
#' 
#' myShortFac <- factor(paste("Sample", 1:2))
#' brewer.pal.factor(myShortFac, name="Set1")
#' }
#' 
#' @export brewer.pal.factor
brewer.pal.factor <- function(factor, name="Greys") {
  .Deprecated("fcbrewer")
  colbase <- brewer.pal.factorLevels(factor=factor, name=name)
  if(is.null(colbase)) return(NULL)
  return(colbase[factor])
}

##-------------------- three-color (or more) systems --------------------##

RIBIOS_BLUEREDS <- c("#2166AC", "#D1E5F0", "white", "#FDDBC7", "#B2182B")


#' Two and three-color panels
#' 
#' @aliases royalbluered royalredblue royalbluegrayred royalredgrayblue turyeb
#' redgreen greenred bluered redblue redblackblue cyanblackyellow
#' yellowblackcyan blueblackred blackredyellow blackgoldred whiteblueblackheat
#' heat magentayellow yellowmagenta blackyellow yellowblack
#' whiteblue whitered blackred blackgreen whiteblack blackwhite
#' @param n Number of colors needed
#' @return Character vector of length \code{n} coding colors
#' @seealso \code{\link{blackyellow}} for two-color systems
#' @examples
#' 
#' display.threecolor.panels()
#' 
#' @export royalbluered
royalbluered <- function(n) colorRampPalette(RIBIOS_BLUEREDS,
                                             space="Lab")(n)

#' @rdname royalbluered
#' @export 
royalredblue <- function(n) colorRampPalette(rev(RIBIOS_BLUEREDS),
                                             space="Lab")(n)

RIBIOS_BLUEGRAYREDS <- c("#2166AC", "gray", "#B2182B")

#' @rdname royalbluered
#' @export 
royalbluegrayred <- function(n) colorRampPalette(RIBIOS_BLUEGRAYREDS,
                                                 space="Lab")(n)

#' @rdname royalbluered
#' @export 
royalredgrayblue <- function(n) colorRampPalette(rev(RIBIOS_BLUEGRAYREDS),
                                                 space="Lab")(n)

#' @rdname royalbluered
#' @export 
blackyellow <- function(n) colorpanel(n, "black", "yellow")

#' @rdname royalbluered
#' @export 
yellowblack <- function(n) colorpanel(n, "yellow", "black")

RIBIOS_WHITEBLUES <- c("#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3")
#' @rdname royalbluered
#' @export 
whiteblue <- function(n) colorRampPalette(RIBIOS_WHITEBLUES,
                                          space="Lab")(n)

RIBIOS_WHITEREDS <- c("#F7F7F7", "#FDDBC7", "#F4A582", "#D6604D")
#' @rdname royalbluered
#' @export 
whitered <- function(n) colorRampPalette(RIBIOS_WHITEREDS,
                                         space="Lab")(n)

#' @rdname royalbluered
#' @export 
blackred <- function(n) colorpanel(n, "black", "red")

#' @rdname royalbluered
#' @export 
blackgreen <- function(n) colorpanel(n, "black", "green")

#' @rdname royalbluered
#' @export 
whiteblack <- function(n) rev(blackwhite(n))
#' @rdname royalbluered
#' @export 
blackwhite <- function(n) gray(seq(0, 1, 1/(n-1)))


##-------------------- l3 color systems --------------------##

#' @rdname royalbluered
#' @export 
turyeb <- function(n) colorpanel(n, "turquoise1", "yellow", "black")

## following functions were created in the gplots package (CRAN):
## redgreen, bluered, greenred, redblue
## using with the GPL-2 license
#' @rdname royalbluered
#' @export 
redgreen <- function(n) colorpanel(n, "red", "black", "green")

#' @rdname royalbluered
#' @export 
greenred <- function(n) colorpanel(n, "green", "black", "red")

#' @rdname royalbluered
#' @export 
bluered <- function(n) colorpanel(n, "blue", "white", "red")

#' @rdname royalbluered
#' @export 
redblue <- function(n) colorpanel(n, "red", "white", "blue")

#' @rdname royalbluered
#' @export 
blueblackred <- function(n) colorpanel(n, "blue", "black", "red")

#' @rdname royalbluered
#' @export 
cyanblackyellow <- function(n) colorpanel(n, "#9BD0EC", "black", "#FCF6C3")

#' @rdname royalbluered
#' @export 
yellowblackcyan <- function(n) colorpanel(n, "#FCF6C3", "black", "#9BD0EC")

#' @rdname royalbluered
#' @export 
redblackblue <- function(n) colorpanel(n, "red", "black", "blue")

#' @rdname royalbluered
#' @export 
blackredyellow <- function(n) colorRampPalette(c("black", "darkred", "red2", "goldenrod1", "yellow"),
                                               space="Lab")(n)

#' @rdname royalbluered
#' @export 
blackgoldred <- function(n) colorpanel(n, "black", "goldenrod1", "red2")

#' @rdname royalbluered
#' @export 
magentayellow <- function(n) colorRampPalette(c("magenta2", "black", "yellow"), space="Lab")(n)

#' @rdname royalbluered
#' @export 
yellowmagenta <- function(n) colorRampPalette(c("yellow", "black", "magenta2"), space="Lab")(n)


#' @rdname royalbluered
#' @export 
whiteblueblackheat <- function(n) colorRampPalette(c("white", "blue", "blue3", "black", "red3", "red", "yellow"))(n)

#' @rdname royalbluered
#' @export 
heat <- function(n)  colorRampPalette(c("transparent", blues9, "black", "red3", "red", "yellow"))(n)


##-------------------- Display functions --------------------##
#' Return available three-color panels
#' @return A vector of character strings
#' @export twocolor.panels
twocolor.panels <- function() {
  return(c("blackyellow", "yellowblack",
           "whiteblue", "whitered",
           "blackred", "blackgreen", "whiteblack", "blackwhite"))
}

#' Return available three-color panels
#' @return A vector of character strings
#' @export
threecolor.panels <- function() {
  return(c("royalbluered", "royalredblue",
           "royalbluegrayred","royalredgrayblue",
           "turyeb",
           "redgreen", "greenred",
           "bluered", "redblue",
           "redblackblue", "blueblackred",
           "cyanblackyellow", "yellowblackcyan",
           "heat.colors",
           "blackredyellow", "blackgoldred", 
           "magentayellow", "yellowmagenta",
           "whiteblueblackheat", "heat"))
}

#' Display color panels
#' 
#' @param panel.names A vector of character strings, panels to be visualized
#' @param nc Number of color columns
#' @return Side effect (visuzalization is used)
#' @author Jitao David Zhang
#' @examples
#' display.colorpanels(threecolor.panels(), 6)
#' @export
display.colorpanels <- function(panel.names, nc) {
  nc <- as.integer(pmax(pmin(nc, 100), 3))
  np=length(panel.names)
  oldpar <- par(mgp=c(2, 0.25, 0))
  on.exit(par(oldpar))
  plot(1, 1, xlim=c(0, nc), ylim=c(0,length(panel.names)), type="n",
       bty="n",axes=FALSE, bty="n", xlab="", ylab="")
  for(i in seq(along=panel.names)) {
    curcols <- eval(call(panel.names[i], nc))
    rect(xleft=0:(nc-1), ybottom=i-1, xright=1:nc, ytop=i-0.2,col=curcols, border="lightgray")
    text(rep(-0.1, np), (1:np)-0.6, labels=panel.names, xpd=TRUE, adj=1)
  }
  return(invisible(NULL))
}

#' Display two-color panels
#' @param nc Number of columns
#' @return Side effect is used
#' @examples
#' 
#' display.twocolor.panels()
#' @export
display.twocolor.panels <- function (nc=20) {
  display.colorpanels(twocolor.panels(), nc)
}

#' Display three-color panels
#' @param nc Number of columns
#' @return Side effect is used
#' 
#' @examples
#' 
#' display.threecolor.panels()
#' @export
display.threecolor.panels <- function (nc=20) {
  display.colorpanels(threecolor.panels(), nc)
}




#' Blender two colors to get the midpoint color of two colors
#' 
#' @param col1 Character string, represting the first color. It can be of
#' length 2 or 1; in the former case, \code{col2} should be missing
#' @param col2 Character string, represting the second color
#' @return The midpoint color of the two in the Lab space.
#' @examples
#' 
#' midCol("black", "red")
#' midCol(c("black", "red"))
#' \dontrun{
#' set.seed(1778)
#' nCol <- 20
#' candCol <- grep("gr[a|e]y", colors(), value=TRUE, invert=TRUE)
#' firstCols <- sample(candCol, nCol)
#' secondCols <- rev(sample(candCol, nCol))
#' midCols <- sapply(seq(along=firstCols), function(i) 
#'   midCol(firstCols[i], secondCols[i]))
#' plot.new()
#' plot.window(xaxt="n", yaxt="n", xlim=c(0, nCol),
#'      ylim=c(0.5, 4), bty="n")
#' title("Example of midCol")
#' segments(x0=1:nCol, y0=0, x1=1:nCol, y1=4, col="lightgray")
#' points(x=rep(1:nCol, each=3),
#'      y=rep(1:3, nCol), 
#'      pch=21, cex=1.75,
#'      bg=as.vector(rbind(firstCols, midCols, secondCols)))
#' text(0, c(1.5, 2.5, 3.5), c("Second", "Midpoint", "First"),
#'      pos=4)
#' }
#' 
#' @export midCol
midCol <- function(col1, col2) {
  if(missing(col2)) {
    if(length(col1)==2) {
      col2 <- col1[2]
      col1 <- col1[1]
    } else {
      stop("Two colors should be provided")
    }
  }
  colorRampPalette(c(col1, col2), space="Lab")(3)[2]
}
