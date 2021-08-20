scaleRadius <- function(data) {
  # radius -> [0,1], then to [0.01, 0.1]
  minr <- min(data$radius, na.rm = TRUE)
  maxr <- max(data$radius, na.rm = TRUE)
  if (minr == maxr) {
    if (minr > 0) {
      if (minr > 1) data$radius <- 1
      return (data)
    }
  }

  data$radius <- with(data, (radius - minr)/abs(maxr - minr))
  data$radius <- with(data, 0.01 + 0.1*radius)
  data
}

#' @rdname geom_circle
#' @export
GeomCircle <- ggplot2::ggproto("GeomCircle", ggplot2::Geom,
  required_aes = c("x", "y", "radius", "radius.fixed"),
  default_aes = ggplot2::aes(
    colour = "grey30", fill=NA, alpha=NA, linewidth=1, linetype="solid"),

  draw_key = function (data, params, size)
  {
    grid::circleGrob(
      0.5, 0.5,
      r=0.35,
      gp = grid::gpar(
        col = scales::alpha(data$colour, data$alpha),
        fill = scales::alpha(data$fill, data$alpha),
        lty = data$linetype,
        lwd = data$linewidth,
        fontsize = data$radius*2
      )
    )
  },


  draw_panel = function(data, panel_scales, coord,  radius.fixed, na.rm = TRUE) {
    print("---")
    print(radius.fixed)
    print("---")
    
    if(radius.fixed) {
      dx <- abs(panel_scales$x.range[2] - panel_scales$x.range[1])
      dy <- abs(panel_scales$y.range[2] - panel_scales$y.range[1])
      d <- min(dx, dy)
      
      coords <- coord$transform(data, panel_scales)
      print(coords)
      
      coords$radius <- coords$radius / d # scale proportional to screen
      #coords <- scaleRadius(coords) # scaling to reset to [0, 1] unnecessary
      print(coords)
    } else {
      coords <- coord$transform(data, panel_scales)
      print(coords)
      coords <- scaleRadius(coords)
      print(coords)
    }

    grid::circleGrob(
      x=coords$x, y=coords$y,
      r=coords$radius,
      gp = grid::gpar(
        col = alpha(coords$colour, coords$alpha),
        fill = alpha(coords$fill, coords$alpha),
        lty = coords$linetype,
        lwd = coords$linewidth,
        fontsize = coords$radius*2
      )
    )
  }
)

#' Geom for drawing circles in the ggplot2 framework
#'
#' Circles are drawn with a specified radius centered at (x, y).
#' This geom is very much exploratory - we are using it for drawing edges for self references.
#' It is not explored for any more general use, so use with caution!
#' @inheritParams ggplot2::geom_point
#' @param radius numeric value giving the radius of the circle to be drawn (0-1 normalized scale)
#' @param radius.fixed Make the size of the radius fixed to grid coordinates instead of xlim and ylim (T/F)
#' @export
#' @examples
#' # circles are drawn centered at x and y
#' library(ggplot2)
#' data(mpg)
#' ggplot(mpg, aes(displ, hwy)) + geom_circle(radius=0.1) + geom_point()
#' ggplot(mpg, aes(displ, hwy)) + geom_circle(linetype=2, radius=0.05, alpha=0.5)
#' ggplot(mpg, aes(displ, hwy)) + geom_circle(aes(linetype=factor(cyl)), radius=0.05, alpha=0.5)
#' df = data.frame(x = 0, y = 0)
#' ggplot(df, aes(x=x, y=y)) + geom_point(cex=4) + geom_circle(radius=1, col="red", radius.fixed=T) + xlim(-3,3) + ylim(-3,3)
#' ggplot(df, aes(x=x, y=y)) + geom_point(cex=4) + geom_circle(radius=0.55, col="red", radius.fixed=F) + xlim(-3,3) + ylim(-3,3)

geom_circle <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, radius = 0.05, radius.fixed = F, ...) {
  
  print(":::")
  print(radius.fixed)
  print(":::")
  
  ggplot2::layer(
    geom = GeomCircle, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, radius = radius, radius.fixed=radius.fixed, ...)
  )
}


