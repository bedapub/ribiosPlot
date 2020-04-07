#' ribiosPlot: Data transformation and visualization in ribios
#' 
#' The package provides data structures and functions to plot graphics in the
#' ribios software suite It is a collection of code snippets I used to generate
#' plots to visualize omics data.
#' 
#' @name ribiosPlot-package
#' @aliases ribiosPlot-package ribiosPlot
#' @docType package
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @keywords package
NULL

#' Factor-matching colors
#' 
#' This object represents a vector of colors (in characters), together with a
#' vector of 'base colors', i.e. the elementary colors used in the vector. This
#' data structure is useful to represent factor using colors since the base
#' colors are mapped to the levels of the factor.
#' 
#' @name fcol-class
#' @aliases fcol-class fcol fcbase fcbase-methods fcbase,fcol-method
#' show,fcol-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the
#' function \code{fcol}. However, the users should not directly call this
#' function. Instead, \code{\link{fcbrewer}} should be called to generate a
#' factor-matching color vector. \code{fcbrewer} calls \code{fcol} internally.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @keywords classes
#' @examples
#' 
#'   testFac <- gl(4,2)
#'   testCol <- fcbrewer(testFac, panel="Set1")
#' 
#'   print(testCol)
#'   fcbase(testCol)
#'   as.character(testCol)
#' 
NULL


#' Build fcol objects
#' 
#' @name fcol-methods
#' @aliases fcol-methods fcol,character,character-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(object = \"character\", base = \"character\")")}{
#' Object should be a vector of colors, and base should be the unique elements.
#' } }
NULL




