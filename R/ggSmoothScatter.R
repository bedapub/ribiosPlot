#' Mimicking the graphics::smoothScatter behaviour for GGally::ggpairs
#' @param data Data to be visualized, normally not directly set by the user
#' @param mapping Data mapping, normally not directly set by the user
#' @param colours Colours to be used
#' @param xlim NULL or a vector of two numbers
#' @param ylim NULL or a vector of two numbers
#' @param ... Other parameters passed to stat_density_2d
#' 
#' #' @note 
#' So far the outliers are not plotted, to be done later
#' 
#' #' @importFrom ggplot2 ggplot stat_density_2d scale_fill_gradientn geom_vline geom_hline aes scale_x_continuous scale_y_continuous stat
#' @export
ggSmoothScatter <- function(data, mapping, 
                            colours=colorRampPalette(c("white",blues9[5:9], 
                                                       "black"))(256),
                            xlim=NULL, ylim=NULL, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    stat_density_2d(aes(fill=stat(density)^0.25, alpha=1), 
                    geom="tile", contour = FALSE,  ...) +
    scale_fill_gradientn(colours=colours) 
  if(!is.null(xlim))
    p <- p + scale_x_continuous(limits = xlim)
  if(!is.null(ylim))
    p <- p + scale_y_continuous(limits = ylim)
  p
}

#' Mimicking the graphics::smoothScatter behaviour for GGally::ggpairs, with aux lines
#' @param data Data to be visualized, normally not directly set by the user
#' @param mapping Data mapping, normally not directly set by the user
#' @param colours Colours to be used
#' @param xlim NULL or a vector of two numbers
#' @param ylim NULL or a vector of two numbers
#' @param ... Other parameters passed to stat_density_2d
#' 
#' Compared with \code{ggSmoothScatter}, 
#' 
#' @export
ggSmoothScatterWithAux <-  function(data, mapping,  
                                    colours=colorRampPalette(c("white",blues9[5:9], 
                                                               "black"))(256),
                                    xlim=NULL, ylim=NULL, ...) {
  p <- ggSmoothScatter(data=data, mapping=mapping, colours=colours, 
                       xlim=xlim, ylim=ylim, ...) +
    geom_vline(xintercept=0, col="#999999") +
    geom_hline(yintercept=0, col="#999999") +
    geom_abline(slope=1, intercept=0, col="red")
  return(p)
}
