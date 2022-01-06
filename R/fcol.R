#'
NULL

#' Construct a fcol object
#' @param colors A character vector, containing colors
#' @param base A character vector, containing base colors
#' @return A S3 class known as \code{fcol}, containing colors as vector and base colors in the attribute \code{fcbase}
#' 
#' @examples 
#' fcol(c("lightblue", "orange", "lightblue"), base=c("orange", "lightblue"))
#' @export
fcol <- function(colors, base) {
  attr(colors, "fcbase") <- base
  class(colors) <- "fcol"
  return(colors)
}

#' Return base colors of a fcol object
#' @param fcol A fcol object, likely constructed by \code{\link{fcol}}
#' @return A character vector, representing base colors
#' @examples 
#' fc <- fcol(c("lightblue", "orange", "lightblue"), base=c("orange", "lightblue"))
#' fcbase(fc)
#' @export
fcbase <- function(fcol) {
  return(attr(fcol, "fcbase"))
}

#' Print a fcol object
#' @param x A fcol object, likely constructed by \code{\link{fcol}}
#' @param ... Not used now
#' @examples
#' fc <- fcol(c("lightblue", "orange", "lightblue"), base=c("orange", "lightblue"))
#' fc
#' @importFrom ribiosUtils chosenFew
#' @export
print.fcol <- function(x, ...) {
  acol <- as.character(x)
  bcol <- fcbase(x)
  cat("Factor-matching colors\n",
      "Colors: (", length(acol), "):", ribiosUtils::chosenFew(acol),"\n",
      "Base colors (", length(bcol), "):", ribiosUtils::chosenFew(fcbase(x)), "\n",
      sep="")
}

#' Replace base colors of a fcol object with a different value
#' @param fcol A fcol object, likely constructed by \code{\link{fcol}}
#' @param value A character vector, indicating the new values
#' @return A new \code{fcol} object
#' @importFrom ribiosUtils mmatch
#' @examples 
#' fc <- fcol(c("lightblue", "orange", "lightblue"), base=c("orange", "lightblue"))
#' fcbase(fc)
#' fcbase(fc)[1] <- "red"
#' print(fc)
#' fcbase(fc) <- c("gray", "darkblue")
#' fc
#' @export
`fcbase<-` <- function(fcol, value) {
  oldVal <- fcbase(fcol)
  attr(fcol, "fcbase") <- value
  inds <- ribiosUtils::mmatch(oldVal, fcol)
  fcol[unlist(inds)] <- unlist(rep(value, sapply(inds, length)))
  return(fcol)
}


#' Factor color brewer
#' 
#' The function generates a vector of color names for a factor(-like) object.
#' 
#' When using \code{brewer.pal} to generate palettes, the panel is
#' automatically expanded using \code{\link[grDevices]{colorRampPalette}} when
#' the number of levels of the input factor exceeds the limit of respective
#' panel. This is done automatically.
#' 
#' @param factor A vector of factors. Non-factors will be cast to factors by
#' calling the \code{factor} function.
#' @param panel This parameter can take three types of values: (1) a color set
#' name defined in \code{brewer.pal.info} in the RColorBrewer package, (2) a
#' function (or the name of a function) that takes an integer as input and
#' returns a vector of colors that will be used as the base colors of levels of
#' the factor, or (3) a character vector which represents the base colors. In
#' the last case, the length of the vector must match the number of levels of
#' the factor. See examples below.
#' @return An \code{fcol} object encoding colors matching the factors as well
#' as the base colors. The latter is often needed in figure legends.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{brewer.pal.info} for color panels.
#' @examples
#' 
#' testFactor <- gl(4,25)
#' testCol1 <- fcbrewer(testFactor, panel="Set2")
#' testCol2 <- fcbrewer(testFactor, panel=heat.colors)
#' testCol3 <- fcbrewer(testFactor, panel="heat.colors")
#' testCol4 <- fcbrewer(testFactor, panel=c("black", "green", "orange", "lightblue"))
#' 
#' testRan <- runif(100)
#' ## use colors of each item and colors of each level
#' plot(testRan, pch=21, bg=testCol1)
#' legend("topright", legend=paste("Class", 1:4),
#'        pch=21, pt.bg=fcbase(testCol1))
#' 
#' ## boxplot uses colors matching to each level only
#' boxplot(testRan ~ testFactor, col=fcbase(testCol1))
#' 
#' @importFrom ribiosUtils bound
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @export fcbrewer
fcbrewer <- function(factor, panel="Set1") {
  if(!is.factor(factor)) factor <- factor(factor)
  nlevel <- nlevels(factor)
  if(is.function(panel)) {
    cols <- do.call(panel, list(nlevel))
  } else if (is.character(panel) && length(panel)==1 && exists(panel) && is.function(panelf <- get(panel))) {
    cols <- do.call(panelf, list(nlevel))
  } else if (is.character(panel)) {
    if(length(panel)==1 && panel %in% rownames(RColorBrewer::brewer.pal.info)) {
      ncol <-  bound(nlevel, 3L, brewer.pal.info[panel, "maxcolors"])
      cols <- RColorBrewer::brewer.pal(ncol, panel)
      if (nlevel < 3) {
        cols <- cols[1:nlevel]
      } else if (nlevel > ncol) {
        cols <- grDevices::colorRampPalette(cols)(nlevel)
      }
    } else {
      haltifnot(nlevel==length(panel),
                msg=sprintf("panel length (%d) must match the number of factor levels (%d)",
                            length(panel), nlevel))
      cols <- panel
    }
  } else {
    stop("panel must be either (a) a valid brewer.pal color name (see brewer.pal.info), (b) a function or function name that generates colors, or (c) a vector of colors that match the number of levels of the input factor")
  }
  
  
  names(cols) <- levels(factor)
  fullcols <- cols[as.integer(factor)]
  return(fcol(fullcols, base=cols))
}
