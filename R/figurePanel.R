#' Make a figure panel with title
#' @param gg A \code{grob} object
#' @param title Character, title of the plot panel, e.g. \code{A}, \code{(B)}, etc.
#' 
## @importFrom grid gpar textGrob
## @importFrom gridExtra arrangeGrob
#' @return A \code{grob} object
#' 
#' @examples 
#' require("ggplot2")
#' df <- data.frame(
#'   gp = factor(rep(letters[1:3], each = 10)),
#'   y = rnorm(30)
#' )
#' 
#' ds <- do.call(rbind, lapply(split(df, df$gp), function(d) {
#'   data.frame(mean = mean(d$y), sd = sd(d$y), gp = d$gp)
#'   }))
#'   
#' g1 <- ggplot(df, aes(gp, y)) +
#'   geom_point() +
#'   geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)
#'   
#' g2 <- ggplot() +
#'   geom_point(data = df, aes(gp, y)) +
#'   geom_point(data = ds, aes(gp, mean), colour = 'red', size = 3) +
#'   geom_errorbar(
#'     data = ds,
#'     aes(gp, mean, ymin = mean - sd, ymax = mean + sd),
#'     colour = 'red',
#'     width = 0.4
#'   )
#'   
#' panelA <- figurePanel(g1, "(A)")
#' panelB <- figurePanel(g2, "(B)")
#' layoutMat <- matrix(c(1,2), nrow=1)
#' gridExtra::grid.arrange(grobs=list(panelA, panelB),
#'  layout_matrix=layoutMat)
figurePanel <- function(gg, title) {
  titleg <- grid::textGrob(title, x=unit(0, "npc"), y=unit(1, "npc"), 
                          just=c("left", "top"),
                          gp=grid::gpar(col="black", fontsize=18, fontface="bold"))
  compactPlot <- gg + ggplot2::theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    axis.title.x = ggplot2::element_text(margin=ggplot2::margin(0,0,0,0)), 
    axis.title.y = ggplot2::element_text(margin=ggplot2::margin(0,0,0,0)))
  res <- gridExtra::arrangeGrob(compactPlot,
                                top=titleg,
                                padding = unit(0.15, "line"))
  return(res)
}

