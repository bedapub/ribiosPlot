#' Return a compact setting for lattice plots, useful for preparing
#' publications
#' 
#' The function returns a set of lattice options that are useful for compact
#' figures, with less room for padding and therefore more room for the figure.
#' It is often used to prepare for publications.
#' 
#' 
#' @return A list that can be used in \code{lattice.options}
#' @examples
#' 
#' opts <- compactTrellis()
#' 
#' @export compactTrellis
compactTrellis <- function() {
  op <- lattice::col.whitebg()
  op$layout.widths=list(left.padding=0,
    key.ylab.padding=0.5,
    ylab.axis.padding=0.5,
    axis.right=0.5,
    right.padding=0)
  op$layout.heights=list(top.padding=0,
    bottom.padding=0,
    axis.top=0,
    main.key.padding=0.5,
    key.axis.padding=0.5) ## margins are controlled by 'padding' options
  return(op)
}

#' Set compact trellis as default
#'
#' The function sets compact trellis options as default.
#' The previous \code{lattice.options} are saved and restored
#' via \code{on.exit} when the calling function exits.
#'
#' @return Invisibly, the previous value of the \code{default.theme}
#'   lattice option, so it can be restored manually if needed.
#' @examples
#'
#' \donttest{
#' old <- setCompactTrellis()
#' }
#'
#' @export setCompactTrellis
setCompactTrellis <- function() {
  old <- lattice::lattice.options("default.theme")
  lattice::lattice.options("default.theme" = compactTrellis())
  invisible(old)
}

