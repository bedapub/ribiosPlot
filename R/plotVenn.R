## Venn diagram plotting adapted from Vennerable package
## (js229/Vennerable, GPL license)
## Custom margins c(1.5, 1, 3, 1) are used instead of the default c(1, 1, 1, 1)

## Local helper adapted from Vennerable (unexported PlotSetLabels)
.plotSetLabels <- function(object, gp) {
  VLabels <- Vennerable::VennGetSetLabels(object)
  if (nrow(VLabels) == 0) {
    warning("Can't show Set labels")
    return(invisible(NULL))
  }
  hj <- sapply(VLabels$hjust, function(x) {
    switch(x, left = 0, right = 1, center = , centre = 0.5)
  })
  vj <- sapply(VLabels$vjust, function(x) {
    switch(x, top = 1, bottom = 0, center = , centre = 0.5)
  })
  for (i in seq_len(nrow(VLabels))) {
    grid.text(
      x = VLabels$x[i], y = VLabels$y[i],
      hjust = hj[i], vjust = vj[i], gp = gp[[i]],
      label = as.character(VLabels$Label[i]),
      default.units = "native"
    )
  }
}

#' Plot Venn object of the Vennerable package
#'
#' The function plots Venn objects from the Vennerable package with
#' custom margins that better suit publication figures.
#'
#' @param venn Venn object from the Vennerable package
#' @param main Figure title
#' @param show Named list controlling which elements to display.
#'   Supported elements: \code{Universe} (logical), \code{Sets} (logical),
#'   \code{SetLabels} (logical), \code{DarkMatter} (logical),
#'   \code{Faces} (logical), \code{FaceText} (character, e.g. \code{"weight"}).
#' @param ... Other parameters passed to \code{Vennerable::compute.Venn}
#' @return Side effect is used - a plot is generated.
#' @note The function requires Vennerable package version 3.0 or later.
#'   Plotting logic adapted from Vennerable (GPL license).
#' @examples
#'
#' if (requireNamespace("Vennerable", quietly = TRUE)) {
#'   myVenn <- list(A = LETTERS[1:24], B = LETTERS[3:8], C = LETTERS[5:9])
#'   plotVenn(Vennerable::Venn(myVenn), main = "Letters")
#' }
#'
#' @export plotVenn
plotVenn <- function(venn,
                     main = "",
                     show = list(FaceText = "weight", Universe = FALSE),
                     ...) {
  if (!requireNamespace("Vennerable", quietly = TRUE)) {
    stop("Package 'Vennerable' is required but not installed.")
  }

  C3 <- Vennerable::compute.Venn(venn, ...)

  ## Merge show parameters with defaults
  show.default <- list(
    Universe = FALSE, Sets = TRUE, SetLabels = TRUE,
    DarkMatter = FALSE, Faces = TRUE, FaceText = "weight"
  )
  show.default[names(show)] <- show

  ## Get graphical parameters
  gp <- Vennerable::VennThemes(drawing = C3)

  ## Set up viewports with custom margins
  grid.newpage()
  ranges <- Vennerable::VennGetUniverseRange(C3)
  xrange <- ranges[, 1]
  yrange <- ranges[, 2]
  grid::pushViewport(grid::plotViewport(
    name = "Vennmar", margins = c(1.5, 1, 3, 1)
  ))
  grid::pushViewport(grid::viewport(
    name = "Vennlay",
    layout = grid::grid.layout(1, 1,
      widths = diff(xrange), heights = diff(yrange), respect = TRUE
    )
  ))
  grid::pushViewport(grid::viewport(
    name = "Vennvp", layout.pos.row = 1, layout.pos.col = 1,
    xscale = xrange, yscale = yrange
  ))

  ## Draw Venn elements
  if (show.default$Faces) {
    Vennerable::PlotFaces(C3, gp = gp[["Face"]])
  }
  if (show.default$Sets) {
    Vennerable::PlotSetBoundaries(C3, gp = gp[["Set"]])
  }
  if (show.default$SetLabels) {
    .plotSetLabels(C3, gp = gp[["SetText"]])
  }
  if (length(show.default$FaceText) > 0) {
    Vennerable::PlotIntersectionText(
      C3,
      element.plot = show.default$FaceText,
      gp = gp[["FaceText"]],
      show.dark.matter = show.default$DarkMatter
    )
  }

  ## Pop viewports
  grid::upViewport(3)

  ## Draw title
  grid.text(main, y = unit(0.95, "npc"), gp = gpar(fontsize = 16, font = 2))
}
