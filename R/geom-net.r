#' Networks
#'
#' The net geom is used visualize networks within the \pkg{ggplot2} framework. \code{geom_net} combines the many parts of a network visualization
#' into a single layer in \pkg{ggplot2}. It makes use of various other geoms, including but not limited to, \code{\link[ggplot2]{geom_point}}, \code{\link[ggplot2]{geom_segment}}, and \code{\link[ggplot2]{geom_text}}.
#'
#' @section Aesthetics:
#' \aesthetics{geom}{net}
#'
#' @inheritParams ggplot2::geom_point
#' @param layout.alg character. Value specifying the layout algorithm to use. Defaults to "kamadakawai". See \code{?sna::}\code{\link[sna]{gplot.layout}} documentation for more choices.
#' @param layout.par list. Parameters detailing algorithmic specs. Default parameters from \pkg{sna} are used initially. See  \code{?sna::}\code{\link[sna]{gplot.layout}} documentation for all options corresponding to all layouts.
#' @param directed logical value. Should an arrow be drawn pointing to the \code{to_id} node? Default is \code{FALSE}.
#' @param alpha numeric. Value from 0-1 of alpha blending of nodes.
#' @param ealpha numeric. Value from 0-1 of alpha blending of edges.
#' @param fiteach logical. Should the network be fit in each of the panels separately, or is there going to be one fit for all?
#' @param labelon logical. Include labels for all nodes.  Labels are taken from the \code{from_id} variable, unless a \code{label} aesthetic is provided.
#' @param labelcolour character. Colour for the labels. If this argument is not specified, labels have the same colour as the nodes.
#' @param labelgeom character. Which \pkg{ggplot2} \code{geom} should be used to draw the labels? Either \code{"text"} or \code{"label"}. Default is \code{"text"}
#' @param repel logical. If \code{TRUE}, uses the \pkg{ggrepel} package geoms to draw the node labels instead of the ggplot2 geoms.
#' @param ecolour character. Colour for edges.
#' @param selfloops logical value. Should loops (self-referencing edges) be shown (by drawing a circle adjacent to the corresponding node)? Default is \code{FALSE}.
#' @param arrow what kind of arrow should be drawn? See \code{?grid::}\code{\link[grid]{arrow}} for more.
#' @param arrowsize numeric. How big should the arrow be drawn? Multiplicative of the default, 10 points.
#' @param arrowgap numeric value between 0 and 1 specifying how much (as a proportion of the line length) earlier the line segment should be stopped drawing before reaching the target node. This parameters is only regarded in directed networks.
#' @param vertices data frame. Dataset containing vertex information. Usage is a bit awkward, because every variable in this data set can only be used with the ggplot2 double dot representation ..varname.. Better: use the \code{\link{fortify.edgedf}} method
#'
#' @export
#' @examples
#' \dontrun{
#' library(geomnet)
#' data(blood)
#' p <- ggplot(data = blood$edges, aes(from_id = from, to_id = to))
#' p + geom_net(vertices=blood$vertices, aes(colour=..type..)) + theme_net()
#'
#' bloodnet <- fortify(as.edgedf(blood$edges), blood$vertices)
#' p <- ggplot(data = bloodnet, aes(from_id = from_id, to_id = to_id))
#' p + geom_net()
#' p + geom_net(aes(colour=rho)) + theme_net()
#' p + geom_net(aes(colour=rho), labelon=TRUE, vjust = -0.5)
#' p + geom_net(aes(colour=rho, linetype = group_to, label = from_id),
#'              vjust=-0.5, labelcolour="black",
#'              directed=TRUE) + theme_net()
#' p + geom_net(colour = "orange", layout.alg = 'circle', size = 6)
#' p + geom_net(colour = "orange", layout.alg = 'circle', size = 6, linewidth=.75)
#' p + geom_net(colour = "orange", layout.alg = 'circle', size = 0, linewidth=.75,
#'              directed = TRUE)
#' p + geom_net(aes(size=Predominance, colour=rho, shape=rho, linetype=group_to),
#'              linewidth=0.75, labelon =TRUE, labelcolour="black") +
#'     facet_wrap(~Ethnicity) +
#'     scale_colour_brewer(palette="Set2")
#' gg <- ggplot(data = blood$edges, aes(from_id = from, to_id = to)) +
#'   geom_net(colour = "darkred", layout.alg = "circle", labelon=TRUE, size = 15,
#'          directed = TRUE, vjust = 0.5, labelcolour = "grey80",
#'          arrowsize = 1.5, linewidth = 0.5, arrowgap = 0.05,
#'          selfloops = TRUE, ecolour = "grey40") +
#'   theme_net()
#' gg
#' dframe <- ggplot_build(gg)$data[[1]] # contains calculated node and edge values
#'
#' #Madmen Relationships
#' data(madmen)
#' MMnet <- fortify(as.edgedf(madmen$edges), madmen$vertices)
#' p <- ggplot(data = MMnet, aes(from_id = from_id, to_id = to_id))
#' p + geom_net(labelon=TRUE)
#' p + geom_net(aes(colour=Gender), size=6, linewidth=1, labelon=TRUE, fontsize=3, labelcolour="black")
#' p + geom_net(aes(colour=Gender), size=6, linewidth=1, labelon=TRUE, labelcolour="black") +
#'     scale_colour_manual(values=c("#FF69B4", "#0099ff")) + xlim(c(-.05,1.05))
#' p + geom_net(aes(colour=Gender), size=6, linewidth=1, directed=TRUE, labelon=TRUE,
#'              arrowgap=0.01, labelcolour="black") +
#'     scale_colour_manual(values=c("#FF69B4", "#0099ff")) + xlim(c(-.05,1.05))
#'
#' p <- ggplot(data = MMnet, aes(from_id = from_id, to_id = to_id))
#' # alternative labelling: specify label aesthetic.
#' p + geom_net(aes(colour=Gender, label=Gender), size=6, linewidth=1, fontsize=3,
#'              labelcolour="black")
#'
#' ## visualizing ggplot2 theme elements
#' data(theme_elements)
#' TEnet <- fortify(as.edgedf(theme_elements$edges[,c(2,1)]), theme_elements$vertices)
#' ggplot(data = TEnet, aes(from_id = from_id, to_id = to_id)) +
#'   geom_net(labelon=TRUE, vjust=-0.5)
#'
#' ## emails example from VastChallenge 2014
#' # care has to be taken to make sure that for each panel all nodes are included with
#' # the necessary information.
#' # Otherwise line segments show on the plot without nodes.
#'
#' emailnet <- fortify(as.edgedf(
#'                subset(email$edges, nrecipients < 54)[,c(1,5,2:4,6:9)]
#'              ),  email$nodes)
#' #no facets
#' ggplot(data = emailnet, aes(from_id = from_id, to_id = to_id)) +
#'   geom_net(aes(colour= CurrentEmploymentType), linewidth=0.5) +
#'   scale_colour_brewer(palette="Set2")
#' #facet by day
#' emailnet <- fortify(as.edgedf(
#'                 subset(email$edges, nrecipients < 54)[,c(1,5,2:4,6:9)]
#'              ), email$nodes, group = "day")
#' ggplot(data = emailnet, aes(from_id = from, to_id = to_id)) +
#'   geom_net(aes(colour= CurrentEmploymentType), linewidth=0.5, fiteach=TRUE) +
#'   scale_colour_brewer(palette="Set2") +
#'   facet_wrap(~day, nrow=2) + theme(legend.position="bottom")
#' ggplot(data = emailnet, aes(from_id = from, to_id = to_id)) +
#'   geom_net(aes(colour= CitizenshipCountry), linewidth=0.5, fiteach=TRUE) +
#'   scale_colour_brewer(palette="Set2") +
#'   facet_wrap(~day, nrow=2) + theme(legend.position="bottom")
#' ggplot(data = emailnet, aes(from_id = from, to_id = to_id)) +
#'   geom_net(aes(colour= CurrentEmploymentType), linewidth=0.5, fiteach=FALSE) +
#'   scale_colour_brewer(palette="Set2") +
#'   facet_wrap(~day, nrow=2) + theme(legend.position="bottom")
#'
#' ## Les Miserables example
#' data(lesmis)
#' lesmisnet <- fortify(as.edgedf(lesmis$edges), lesmis$vertices[, c(2,1)])
#' p <- ggplot(data=lesmisnet, aes(from_id=from_id, to_id=to_id))
#' p + geom_net(layout.alg="fruchtermanreingold")
#' p + geom_net(layout.alg="fruchtermanreingold", labelon=TRUE, vjust=-0.5)
#' p + geom_net(layout.alg="fruchtermanreingold", labelon=TRUE, vjust=-0.5,
#'     aes(linewidth=degree/5))
#'
#' ## College Football Games in the Fall 2000 regular season
#' # Source: http://www-/personal.umich.edu/~mejn/netdata/
#' data(football)
#' ftnet <- fortify(as.edgedf(football$edges), football$vertices)
#' p <- ggplot(data=ftnet, aes(from_id=from_id, to_id=to_id))
#' p + geom_net(aes(colour=value), linewidth=0.75, size=4.5, ecolour="grey80") +
#'   scale_colour_brewer("Conference", palette="Paired") + theme_net() +
#'   theme(legend.position="bottom")
#'   }

geom_net <- function (
  mapping = NULL, data = NULL, stat = "net", position = "identity", show.legend = NA, na.rm = FALSE, inherit.aes = TRUE,
  layout.alg="kamadakawai", layout.par=list(), directed = FALSE, fiteach=FALSE,  selfloops = FALSE,
  alpha = 0.25, ecolour=NULL, ealpha=NULL, arrow=NULL, arrowgap=0.01, arrowsize=1,
  labelon=FALSE, labelcolour=NULL, labelgeom = 'text', repel = FALSE,
  vertices=NULL, ...) {
##browser()
    ggplot2::layer(
    geom = GeomNet, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, layout.alg=layout.alg, layout.par=layout.par, fiteach=fiteach, labelon=labelon, labelgeom=labelgeom,
                  ecolour = ecolour, ealpha=ealpha, arrow=arrow, arrowgap=arrowgap, directed=directed, repel = repel,
                  arrowsize=arrowsize,
                  labelcolour=labelcolour, vertices=vertices, selfloops = selfloops,
                  ...)
  )
}

#' @rdname geom_net
#' @importFrom grid grobTree
#' @export
GeomNet <- ggplot2::ggproto("GeomNet", ggplot2::Geom,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(width = 0.75, linetype = "solid", fontsize=5, label = NULL,
                             shape = 19, colour = "grey40", arrowsize = 1,
                             size = 4, fill = NA, alpha = NA, stroke = 0.5,
                             linewidth=1, angle=0, vjust=0, hjust=0.5, curvature = 0),

  draw_key = function(data, params, size)  {
# #browser()
    draw_arrow <-  NULL
    if (params$directed) {
      if (any(data$curvature != 0)){
        draw_arrow <- arrow(length = unit(params$arrowsize*5,"points"), type="open")
        }
      else{
        draw_arrow <- arrow(length = unit(params$arrowsize*5,"points"), type="closed")
      }
   }

    with(data, grobTree(
      grid::pointsGrob(x = c(.15, .85), y = c(.85, .15),
                       pch = data$shape, size = unit(data$size, "points"),
                       gp = grid::gpar(col = alpha(data$colour, data$alpha),
                                       fill = alpha(data$fill, data$alpha),
                                       fontsize = data$size * .pt + data$stroke * .stroke/2,
                                       lwd = data$stroke * .stroke/2)
                       ),
     grid::segmentsGrob(x0 = .15, y0 = .85 ,
                        x1 = ifelse(is.null(draw_arrow), .85, .82),
                        y1 = ifelse(is.null(draw_arrow), .15, .18),
                     gp = grid::gpar(
                       col = alpha(data$colour, data$alpha),
                       fill = alpha(data$colour, data$alpha),
                       lwd = data$linewidth,
                       lty = data$linetype,
                       lineend="butt"),
                     arrow = draw_arrow
                     )

    ))
  },

  setup_data = function(data, params, mapping) {
#    cat("setup_data geom_net\n")

##browser()
    data$from <- as.character(data$from)
    data$to <- as.character(data$to)
    selfie <- (data$from == data$to) & (params$selfloops == TRUE)
  # maximum radius is at the moment hard coded to 0.05
    data$ymax = max(with(data, pmax(y, yend) + 2*0.05*selfie))
    data$xmax = with(data, pmax(x, xend) + 2*0.05*selfie)

    data$from <- factor(data$from)
    data$to <- factor(data$to)

    data
  },

  draw_panel = function(data, panel_scales, coord,  ecolour=NULL, ealpha=NULL, arrow=NULL, arrowgap=0.01,
                        directed=FALSE, arrowsize=1, repel = FALSE,
                        labelon=FALSE, labelgeom='text', labelcolour=NULL, selfloops = FALSE) {

# #browser()
#    data$self <- as.character(data$to) == as.character(data$from)
    edges <- data.frame(
      x = data$x,
      xend = data$xend,
      y = data$y,
      yend = data$yend,
      weight = data$weight,
      colour = ecolour %||% ifelse(data$.samegroup, data$colour, "grey40"),
      size = data$linewidth %||% (data$size / 4),
      nodesize = data$size,
      alpha = ealpha %||% data$alpha,
      linetype=data$linetype,
      stroke = data$stroke,
      selfie = data$.selfie,
      stringsAsFactors = FALSE
    )

    selfy <- subset(edges, selfie == TRUE)
    edges <- subset(edges, selfie != TRUE) # what are we going to do with self references?
    edges <- subset(edges, !is.na(xend))


    vertices <- data.frame(
      x = data$x,
      y = data$y,
      colour = data$colour,
      shape = data$shape,
      size = data$size,
      fill = NA,
      alpha = data$alpha,
      stroke = 0.5,
      stringsAsFactors = FALSE
    )
    vertices <- unique(vertices)

    if (directed) {
      if (any(data$curvature != 0)) {
        if (is.null(arrow)) arrow = arrow(length = unit(data$arrowsize*10,"points"), type="open")
      } else {
        if (is.null(arrow)) arrow = arrow(length = unit(data$arrowsize*10,"points"), type="closed")
      }
      arrowgap <- with(edges, arrowgap/sqrt((xend-x)^2+(yend-y)^2))
      edges <- transform(
        edges,
        xend = x + (1-arrowgap)*(xend-x),
        yend = y + (1-arrowgap)*(yend-y),
        x = x + arrowgap*(xend-x),
        y = y + arrowgap*(yend-y)
      )
    } else arrow=NULL
#
    if (any(data$curvature != 0)){
      edges_draw <- GeomCurve$draw_panel(edges, panel_scales,
                                         coord, arrow=arrow, curvature=data$curvature[1], angle=90)
    }
      else {edges_draw <- GeomSegment$draw_panel(edges, panel_scales, coord, arrow, lineend = "round")}

#

    selfies_draw <- NULL
    if ((nrow(selfy) > 0) & selfloops) {
      selfy$radius <- min(0.04, 1/sqrt(nrow(vertices)))
      selfy <- transform(selfy,
                           x = x + (radius + nodesize/(100*.pt) + size/100)/sqrt(2),
                           y = y + (radius + nodesize/(100*.pt) + size/100)/sqrt(2),
                           linewidth = size*.pt,
                           fill = NA
      )
      selfies_draw <- GeomCircle$draw_panel(selfy, panel_scales, coord)
    }

    selfies_arrows <- NULL
    if ((nrow(selfy) > 0) & selfloops & directed) {
#
      selfy_arrows <- transform (
        selfy,
        xend = x - 0.5* arrowsize*.pt/100,
        yend = y-0.04 - size/100,
        y = y-0.04 - size/100
      )
      selfies_arrows <- GeomSegment$draw_panel(selfy_arrows, panel_scales, coord,
                                               arrow=arrow)
    }

    label_grob <- NULL
    if (labelon | !is.null(data$label)) {
      labels <- data.frame(
        x = data$x,
        y = data$y,
        label = data$label %||% data$from,
        colour = labelcolour %||% data$colour,
        shape = data$shape,
        size = data$fontsize,
        angle = data$angle,
        alpha = data$alpha,
        hjust = data$hjust,
        fill = data$colour,
        vjust = data$vjust,
        stringsAsFactors = FALSE
      )
      labels <- unique(labels)
#       if (labelgeom=='label'){
#       label_grob <- GeomLabel$draw_panel(labels, panel_scales, coord)
#       }
#       else {label_grob <- GeomText$draw_panel(labels, panel_scales, coord)}
#     }

      if (labelgeom=='label'){
        if(repel){
          label_grob <- ggrepel::GeomLabelRepel$draw_panel(labels, panel_scales, coord)
        } else {label_grob <- ggplot2::GeomLabel$draw_panel(labels, panel_scales, coord)}
      } else {
        if(repel){
          label_grob <- ggrepel::GeomTextRepel$draw_panel(labels, panel_scales, coord)
        } else{label_grob <- ggplot2::GeomText$draw_panel(labels, panel_scales, coord)}
      }

  }

    ggplot2:::ggname("geom_net", grobTree(
      edges_draw,
      selfies_draw,
      selfies_arrows,
      GeomPoint$draw_panel(vertices, panel_scales, coord),
      label_grob
    ))
  }
)

