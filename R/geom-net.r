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
#' p + geom_net(vertices = blood$vertices, vcolour = "orange", layout = 'circle',
#' vlabel = TRUE, vsize = 6, directed = TRUE) +  theme_net
#'
#' #Madmen Relationships
#' data(madmen)
#' p <- ggplot(data = madmen$edges, aes(from_id = Name1, to_id = Name2))
#' p + geom_net(vlabel= TRUE)
#' p + geom_net(vertices = madmen$vertices, vsize=5, vlabel= TRUE, colour="grey30",
#'              aes(vcolour=c("#FF69B4", "#0099ff")[as.numeric(madmen$vertices$Gender)]))

geom_net <- function (mapping = NULL, data = NULL, stat = "net", position = "identity", show.legend = NA, inherit.aes = TRUE,  alpha = 0.25,
                      layout="kamadakawai", layout.par=list(), vertices=NULL, vlabel=FALSE, vcolour = NULL, vsize=NULL, vshape=NULL,
                      directed = FALSE, ...) {
    layer(
    geom = GeomNet, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(layout=layout, layout.par=layout.par, vertices=vertices,
                  vlabel=vlabel, vcolour=vcolour, vsize=vsize, vshape=vshape,
                  directed=directed, ...)
  )
}

#' @importFrom grid grobTree
#' @export
GeomNet <- ggplot2::ggproto("GeomNet", ggplot2::Geom,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(width = 0.75, linetype = "solid", fontsize=5,
                             shape = 19, colour = "grey60", size = 0.75, fill = NA,
                             alpha = NA, stroke = 0.5, linewidth=1, angle=0),
  draw_key = ggplot2::draw_key_point,

  setup_data = function(data, params, mapping) {

    data
  },

  draw_panel = function(data, panel_scales, coord, vlabel=FALSE, vcolour=NULL, vsize=NULL, vshape=NULL, directed=FALSE) {
    vertices <- data.frame(
      data$vertices[[1]],
      colour = vcolour %||% data$colour[1],
      shape = vshape %||% data$shape[1],
      size = vsize %||% 5*data$size[1],
      stroke = data$outlier.stroke[1] %||% data$stroke[1],
      fill = NA,
      alpha = NA,
      stringsAsFactors = FALSE,
      row.names=NULL)

    label_grob <- NULL
    if (any(vlabel)) {
      if (length(vlabel) == 1) vlabel <- rep(vlabel, nrow(vertices))
      labels <- subset(vertices, vlabel == TRUE)
      labels$y <- labels$y + 0.02
      labels$vjust <- 1
      labels$angle <- data$outlier.angle[1] %||% data$angle[1]
      labels$colour="grey30"

      label_grob <- GeomText$draw_panel(labels, panel_scales, coord)
    }

    if (directed == TRUE){
      if (is.null(data$arrow)){
        #default arrow parameters.
        data$arrow <- arrow(angle = 15, length = unit(10,"npc"), type = 'closed')
      }
      data$arrow$length[which(data$from == data$to)] <- unit(0,"npc")
    }

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

