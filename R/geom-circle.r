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
  required_aes = c("x", "y", "radius"),
  default_aes = ggplot2::aes(
    colour = "grey30", fill=NA, alpha=NA, linewidth=1, linetype="solid"),

  draw_key = ggplot2::draw_key_blank,


  draw_panel = function(data, panel_scales, coord, na.rm = TRUE) {
    coords <- coord$transform(data, panel_scales)
    coords <- scaleRadius(coords)
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
#' @export
#' @examples
#' # circles are drawn centered at x and y
#' library(ggplot2)
#' data(mpg)
#' ggplot(mpg, aes(displ, hwy)) + geom_circle(size=0.1) + geom_point()
#' ggplot(mpg, aes(displ, hwy)) + geom_circle(linetype=2, size=0.05, alpha=0.5)
#' ggplot(mpg, aes(displ, hwy)) + geom_circle(aes(linetype=factor(cyl)), size=0.05, alpha=0.5)

geom_circle <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomCircle, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

