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
#' bloodnet <- merge(blood$edges, blood$vertices, by.x="from", by.y="label")
#' p <- ggplot(data = bloodnet, aes(from_id = from, to_id = to))
#' p + geom_net()
#' p + geom_net(aes(colour=rho))
#' p + geom_net(aes(colour=rho)) + geom_text(aes(x=..x.., y=..y.., label=from)) # not working yet
#' p + geom_net(colour = "orange", layout = 'circle', size = 6)
#' p + geom_net(colour = "orange", layout = 'circle', size = 6, esize=.75)
#' p + geom_net(colour = "orange", layout = 'circle', size = 0, esize=.75, directed = TRUE)
#'
#'
#' #Madmen Relationships
#' data(madmen)
#' MMnet <- merge(madmen$edges, madmen$vertices, by.x="Name1", by.y="label", all=T)
#' p <- ggplot(data = MMnet, aes(from_id = Name1, to_id = Name2))
#' p + geom_net()
#' p + geom_net(vertices = madmen$vertices, vsize=5, vlabel= TRUE, colour="grey30",
#'              aes(vcolour=c("#FF69B4", "#0099ff")[as.numeric(madmen$vertices$Gender)]))

geom_net <- function (mapping = NULL, data = NULL, stat = "net", position = "identity", show.legend = NA, inherit.aes = TRUE,  alpha = 0.25,
                      layout="kamadakawai", layout.par=list(), ecolour="grey60", esize = NULL, directed = FALSE, arrowsize=1,...) {
    layer(
    geom = GeomNet, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(layout=layout, layout.par=layout.par, ecolour = ecolour, esize = esize, directed=directed, arrowsize=arrowsize, ...)
  )
}

#' @importFrom grid grobTree
#' @export
GeomNet <- ggplot2::ggproto("GeomNet", ggplot2::Geom,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(width = 0.75, linetype = "solid", fontsize=5,
                             shape = 19, colour = "grey30",
                             size = 4, fill = NA,
                             alpha = NA, stroke = 0.5, linewidth=1, angle=0),
  draw_key = ggplot2::draw_key_point,

  setup_data = function(data, params, mapping) {

    data
  },

  draw_panel = function(data, panel_scales, coord, ecolour="grey60", esize=NULL, directed=FALSE, arrowsize=1) {
    browser()
    edges <- data.frame(
      x = data$x,
      xend = data$xend,
      y = data$y,
      yend = data$yend,
      colour = ecolour,
      size = esize %||% (data$size / 4),
      alpha = data$alpha
    )
    arrow = NULL
    if (directed) arrow = arrow(length = unit(arrowsize*0.3,"cm"), type="closed")

    vertices <- data.frame(
      x = data$x,
      y = data$y,
      colour = data$colour,
      shape = data$shape,
      size = data$size,
      stroke = data$stroke,
      fill = NA,
      alpha = data$alpha,
      stringsAsFactors = FALSE
    )
    vertices <- unique(vertices)

    ggplot2:::ggname("geom_net", grobTree(
      GeomSegment$draw_panel(edges, panel_scales, coord, arrow),
      GeomPoint$draw_panel(vertices, panel_scales, coord)
#      label_grob
    ))
  },

  draw_legend = function(data, ...)  {
    with(data, grobTree(
      rectGrob(gp = gpar(col = colour, fill = alpha(fill, alpha), lty = linetype)),
      linesGrob(gp = gpar(col = colour, lwd = size * .pt, lineend="butt", lty = linetype))
    ))
  }
)

