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
#' bloodnet <- merge(blood$edges, blood$vertices, by.x="from", by.y="label", all=T)
#' p <- ggplot(data = bloodnet, aes(from_id = from, to_id = to))
#' p + geom_net()
#' p + geom_net(aes(colour=rho))
#' p + geom_net(aes(colour=rho), label=TRUE)
#' p + geom_net(colour = "orange", layout = 'circle', size = 6)
#' p + geom_net(colour = "orange", layout = 'circle', size = 6, esize=.75)
#' p + geom_net(colour = "orange", layout = 'circle', size = 0, esize=.75, directed = TRUE)
#' p + geom_net(aes(size=Predominance, colour=rho, shape=rho, linetype=group_to), esize=0.75, label =TRUE,
#'     labelcolour="black") + facet_wrap(~Ethnicity) +
#'     scale_colour_brewer(palette="Set2")
#'
#' #Madmen Relationships
#' data(madmen)
#' MMnet <- merge(madmen$edges, madmen$vertices, by.x="Name1", by.y="label", all=T)
#' p <- ggplot(data = MMnet, aes(from_id = Name1, to_id = Name2))
#' p + geom_net(label=TRUE)
#' p + geom_net(aes(colour=Gender), size=6, esize=1, label=TRUE, fontsize=3, labelcolour="black")
#' p + geom_net(aes(colour=Gender), size=6, esize=1) +
#'     scale_colour_manual(values=c("#FF69B4", "#0099ff"))
#'
#' p <- ggplot(data = MMnet, aes(from_id = Name1, to_id = Name2))
#' # alternative labelling: specify label variable.
#' p + geom_net(aes(colour=Gender, label=Gender), size=6, esize=1, fontsize=3, labelcolour="black")


geom_net <- function (mapping = NULL, data = NULL, stat = "net", position = "identity", show.legend = NA, inherit.aes = TRUE,  alpha = 0.25,
                      layout="kamadakawai", layout.par=list(), label=FALSE, ecolour="grey60", esize = NULL, directed = FALSE, arrowsize=1,
                      labelcolour=NULL, ...) {
    layer(
    geom = GeomNet, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(layout=layout, layout.par=layout.par, label=label,
                  ecolour = ecolour, esize = esize, directed=directed,
                  arrowsize=arrowsize, labelcolour=labelcolour, ...)
  )
}

#' @importFrom grid grobTree
#' @export
GeomNet <- ggplot2::ggproto("GeomNet", ggplot2::Geom,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(width = 0.75, linetype = "solid", fontsize=5,
                             shape = 19, colour = "grey30",
                             size = 4, fill = NA,
                             alpha = NA, stroke = 0.5, linewidth=1, angle=0, vjust=0),

  draw_key = function(data, params)  {
    with(data, grobTree(
      grid::pointsGrob(0.5, 0.5, pch = data$shape,
                       gp = grid::gpar(col = alpha(data$colour, data$alpha),
                                       fill = alpha(data$fill, data$alpha),
                                       fontsize = data$size * .pt + data$stroke * .stroke/2,
                                       lwd = data$stroke * .stroke/2)),
      grid::rectGrob(gp = grid::gpar(col = colour, fill = alpha(fill, alpha), lty = linetype))
      #      grid::linesGrob(gp = grid::gpar(col = colour, lwd = size * .pt, lineend="butt", lty = linetype))
    ))
  },

  setup_data = function(data, params, mapping) {

    data
  },

  draw_panel = function(data, panel_scales, coord,  ecolour="grey60", esize=NULL,
                        directed=FALSE, arrowsize=1, label=FALSE, labelcolour=NULL) {
#    browser()
    edges <- data.frame(
      x = data$x,
      xend = data$xend,
      y = data$y,
      yend = data$yend,
      colour = ecolour,
      size = esize %||% (data$size / 4),
      alpha = data$alpha,
      linetype=data$linetype,
      stringsAsFactors = FALSE
    )
    edges <- unique(subset(edges, !is.na(xend)))

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

    label_grob <- NULL
    if (label | !is.null(data$label)) {
      labels <- data.frame(
        x = data$x,
        y = data$y,
        label = data$label %||% data$from,
        colour = labelcolour %||% data$colour,
        shape = data$shape,
        size = data$fontsize,
        angle = data$angle,
        alpha = NA,
        vjust= data$vjust,
        stringsAsFactors = FALSE
      )
      labels <- unique(labels)
      label_grob <- GeomText$draw_panel(labels, panel_scales, coord)
    }

    ggplot2:::ggname("geom_net", grobTree(
      GeomSegment$draw_panel(edges, panel_scales, coord, arrow),
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

