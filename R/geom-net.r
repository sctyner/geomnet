#' Geom for network visualization
#'
#' @inheritParams ggplot2::stat_identity
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning. If \code{TRUE} silently removes missing values.
#'
#' @export
#' @examples
#' library(ggplot2)
#' data(blood)
#' p <- ggplot(data = blood$edges, aes(from_id = from, to_id = to))
#' p + geom_net()
#' p + geom_net(vertices = blood$vertices)
#' p + geom_net(vertices = blood$vertices, vlabel=blood$vertices$rho=="neg")
#' p + geom_net(vertices = blood$vertices, vcolour = I('red'), layout = 'circle',
#' vlabel = TRUE, vsize = I(3), directed = TRUE) +
#' expand_limits(x = c(0,1), y = c(0,1)) + theme_net
#'
geom_net <- function (mapping = NULL, data = NULL, stat = "net", position = "identity", show.legend = NA, inherit.aes = TRUE,  alpha = 0.25,
                        layout="kamadakawai", layout.par=list(), vertices=NULL, vlabel=FALSE, ...) {
browser()
    layer(
    geom = GeomNet, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(layout=layout, layout.par=layout.par, vertices=vertices, vlabel=vlabel, ...)
  )
}

#' @importFrom grid grobTree
#' @export
GeomNet <- ggplot2::ggproto("GeomNet", ggplot2::Geom,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(width = 0.75, linetype = "solid", fontsize=5,
                             shape = 19, colour = "black", size = 0.75, fill = NA,
                             alpha = NA, stroke = 0.5, linewidth=1, angle=0),
  draw_key = ggplot2::draw_key_point,

  draw_panel = function(data, panel_scales, coord, vlabel=FALSE) {
browser()
    vertices <- data.frame(
      data$vertices[[1]],
      colour = data$colour[1],
      shape = data$shape[1],
      size = 5*data$size[1],
      stroke = data$outlier.stroke[1] %||% data$stroke[1],
      fill = NA,
      alpha = NA,
      stringsAsFactors = FALSE,
      row.names=NULL)

    if (any(vlabel)) {
      if (length(vlabel) == 1) vlabel <- rep(vlabel, nrow(vertices))
      labels <- subset(vertices, vlabel == TRUE)
      labels$angle <- data$outlier.angle[1] %||% data$angle[1]
      labels$colour="red"

      label_grob <- GeomText$draw_panel(labels, panel_scales, coord)
    } else label_grob <- NULL

    ggplot2:::ggname("geom_net", grobTree(
      GeomSegment$draw_panel(data, panel_scales, coord),
      GeomPoint$draw_panel(vertices, panel_scales, coord),
      label_grob
    ))
  },

  draw_legend = function(data, ...)  {
    with(data, grobTree(
      rectGrob(gp = gpar(col = colour, fill = alpha(fill, alpha), lty = linetype)),
      linesGrob(gp = gpar(col = colour, lwd = size * .pt, lineend="butt", lty = linetype))
    ))
  }
)

