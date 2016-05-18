#' Geom for network visualization within the ggplot2 framework
#'
#' @inheritParams ggplot2::stat_identity
#' @param stat character string of the network stat corresponding to geom_net.
#' @param alpha numeric value of alpha blending of vertices.
#' @param ealpha numeric value of alpha blending of edges.
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning. If \code{TRUE} silently removes missing values.
#' @param layout character value specifying the layout algorithm to use. Defaults to "kamadakawai". See \code{?gplot.layout} in the package sna for other choices.
#' @param layout.par list of parameters detailing algorithmic specs. Default parameters are used initially. See \code{?gplot.layout} in the package sna for other choices.
#' @param fiteach logical value. Should the network be fit in each of the panels separately, or is there going to be one fit for all?
#' @param label logical value. Include labels for (all) nodes. labelcolour specifies colour of labels, if they should not be the same as the nodes. labels are taken from the from_id variable, unless a label variable is given.
#' @param labelcolour character of colour for the labels.
#' @param fontsize numeric. If labels are present, changes the size of the label.
#' @param ecolour colour for edges.
#' @param directed logical value. Should an arrow be drawn from 'from' to 'to' node?
#' @param selfies logical value. Should self-references be shown (by drawing a circle adjacent to the corresponding node)? defaults to FALSE.
#' @param arrow what kind of arrow should be drawn? See specification of function \code{arrow} in grid package
#' @param arrowsize numeric value (non-negative). How big should the arrow be drawn? Multiplicative of a pre-specified unit.
#' @param arrowgap numeric value between 0 and 1 specifying how much (as a proportion of the line length) earlier the line segment should be stopped drawing before reaching the target node. This parameters is only regarded in directed networks.
#' @param vertices data frame containing vertex information. Usage is a bit awkward, because every variable in this data set can only be used with the ggplot2 double dot representation ..varname.. Better: use a joint to include this information in the data dataframe
#'
#' @export
#' @examples
#' library(ggplot2)
#' data(blood)
#' p <- ggplot(data = blood$edges, aes(from_id = from, to_id = to))
#' p + geom_net(vertices=blood$vertices, aes(colour=..type..)) + theme_net()
#'
#' bloodnet <- merge(blood$edges, blood$vertices, by.x="from", by.y="label", all=TRUE)
#' p <- ggplot(data = bloodnet, aes(from_id = from, to_id = to))
#' p + geom_net()
#' p + geom_net(aes(colour=rho)) + theme_net()
#' p + geom_net(aes(colour=rho), label=TRUE, vjust = -0.5)
#' p + geom_net(aes(colour=rho), label=TRUE, vjust=-0.5, labelcolour="black",
#'              directed=TRUE, curvature=0.2) + theme_net()
#' p + geom_net(colour = "orange", layout = 'circle', size = 6)
#' p + geom_net(colour = "orange", layout = 'circle', size = 6, linewidth=.75)
#' p + geom_net(colour = "orange", layout = 'circle', size = 0, linewidth=.75,
#'              directed = TRUE)
#' p + geom_net(aes(size=Predominance, colour=rho, shape=rho, linetype=group_to),
#'              linewidth=0.75, label =TRUE, labelcolour="black") +
#'     facet_wrap(~Ethnicity) +
#'     scale_colour_brewer(palette="Set2")
#' ggplot(data = blood$edges, aes(from_id = from, to_id = to)) +
#'   geom_net(colour = "darkred", layout = "circle", label = TRUE, size = 15,
#'          directed = TRUE, vjust = 0.5, labelcolour = "grey80",
#'          arrowsize = 1.5, linewidth = 0.5, arrowgap = 0.05,
#'          selfies = TRUE, ecolour = "grey40") +
#'   theme_net()

#'
#' #Madmen Relationships
#' data(madmen)
#' MMnet <- merge(madmen$edges, madmen$vertices, by.x="Name1", by.y="label", all=TRUE)
#' p <- ggplot(data = MMnet, aes(from_id = Name1, to_id = Name2))
#' p + geom_net(label=TRUE)
#' p + geom_net(aes(colour=Gender), size=6, linewidth=1, label=TRUE, fontsize=3, labelcolour="black")
#' p + geom_net(aes(colour=Gender), size=6, linewidth=1, label=TRUE, labelcolour="black") +
#'     scale_colour_manual(values=c("#FF69B4", "#0099ff")) + xlim(c(-.05,1.05))
#' p + geom_net(aes(colour=Gender), size=6, linewidth=1, directed=TRUE, label=TRUE,
#'              arrowgap=0.01, labelcolour="black") +
#'     scale_colour_manual(values=c("#FF69B4", "#0099ff")) + xlim(c(-.05,1.05))
#'
#' p <- ggplot(data = MMnet, aes(from_id = Name1, to_id = Name2))
#' # alternative labelling: specify label variable.
#' p + geom_net(aes(colour=Gender, label=Gender), size=6, linewidth=1, fontsize=3,
#'              labelcolour="black")
#'
#' ## visualizing ggplot2 theme elements
#' data(theme_elements)
#' TEnet <- merge(theme_elements$edges, theme_elements$vertices, by.x="parent",
#'                by.y="name", all=TRUE)
#' ggplot(data = TEnet, aes(from_id = parent, to_id = child)) +
#'   geom_net(label=TRUE, vjust=-0.5)
#'
#'
#' ## emails example from VastChallenge 2014
#' # care has to be taken to make sure that for each panel all nodes are included with
#' # the necessary information.
#' # Otherwise line segments show on the plot without nodes.
#'
#' data(email)
#' employee <- data.frame(expand.grid(
#'               label=unique(email$nodes$label), day=unique(email$edges$day)))
#' employee <- merge(employee, email$nodes, by="label")
#' emailnet <- merge(subset(email$edges, nrecipients < 54), employee,
#'                   by.x=c("From", "day"), by.y=c("label", "day"), all=TRUE)
#'
#' #no facets
#' ggplot(data = emailnet, aes(from_id = From, to_id = to)) +
#'   geom_net(aes(colour= CurrentEmploymentType), linewidth=0.5) +
#'   scale_colour_brewer(palette="Set2")
#'
#' #facet by day
#' ggplot(data = emailnet, aes(from_id = From, to_id = to)) +
#'   geom_net(aes(colour= CurrentEmploymentType), linewidth=0.5, fiteach=TRUE) +
#'   scale_colour_brewer(palette="Set2") +
#'   facet_wrap(~day, nrow=2) + theme(legend.position="bottom")
#' ggplot(data = emailnet, aes(from_id = From, to_id = to)) +
#'   geom_net(aes(colour= CitizenshipCountry), linewidth=0.5, fiteach=TRUE) +
#'   scale_colour_brewer(palette="Set2") +
#'   facet_wrap(~day, nrow=2) + theme(legend.position="bottom")
#' ggplot(data = emailnet, aes(from_id = From, to_id = to)) +
#'   geom_net(aes(colour= CurrentEmploymentType), linewidth=0.5, fiteach=FALSE) +
#'   scale_colour_brewer(palette="Set2") +
#'   facet_wrap(~day, nrow=2) + theme(legend.position="bottom")
#'
#' ## Les Miserables example
#'
#' data(lesmis)
#' lesmisnet <- merge(lesmis$edges, lesmis$vertices, by.x="from", by.y="label", all=TRUE)
#' p <- ggplot(data=lesmisnet, aes(from_id=from, to_id=to))
#' p + geom_net(layout="fruchtermanreingold")
#' p + geom_net(layout="fruchtermanreingold", label=TRUE, vjust=-0.5)
#' p + geom_net(layout="fruchtermanreingold", label=TRUE, vjust=-0.5, aes(linewidth=degree/5))
#'
#' ## College Football Games in the Fall 2000 regular season
#' # Source: http://www-personal.umich.edu/~mejn/netdata/
#' data(football)
#' ftnet <- merge(football$edges, football$vertices, by.x="from", by.y="label", all=TRUE)
#' p <- ggplot(data=ftnet, aes(from_id=from, to_id=to))
#' p + geom_net(aes(colour=value), linewidth=0.75, size=4.5, ecolour="grey80") +
#'   scale_colour_brewer("Conference", palette="Paired") + theme_net() +
#'   theme(legend.position="bottom")

geom_net <- function (mapping = NULL, data = NULL, stat = "net", position = "identity", show.legend = NA, na.rm = TRUE, inherit.aes = TRUE,  alpha = 0.25,
                      layout="kamadakawai", layout.par=list(), fiteach=FALSE,  label=FALSE, ecolour=NULL, ealpha=NULL, arrow=NULL, arrowgap=0.01, directed = FALSE, arrowsize=1,
                      labelcolour=NULL, vertices=NULL, selfies = FALSE, ...) {
    ggplot2::layer(
    geom = GeomNet, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, layout=layout, layout.par=layout.par, fiteach=fiteach, label=label,
                  ecolour = ecolour, ealpha=ealpha, arrow=arrow, arrowgap=arrowgap, directed=directed,
                  arrowsize=arrowsize,
                  labelcolour=labelcolour, vertices=vertices, selfies = selfies,
                  ...)
  )
}

#' @rdname geom_net
#' @importFrom grid grobTree
#' @export
GeomNet <- ggplot2::ggproto("GeomNet", ggplot2::Geom,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(width = 0.75, linetype = "solid", fontsize=5,
                             shape = 19, colour = "grey40", arrowsize = 1,
                             size = 4, fill = NA, alpha = NA, stroke = 0.5,
                             linewidth=1, angle=0, vjust=0, hjust=0.5, curvature = 0),

  draw_key = function(data, params, size)  {
#    browser()
#    arrow = arrow
    if (params$directed) {
      if(is.null(arrow)){
      if (any(data$curvature != 0)){
        arrow = arrow(length = unit(params$arrowsize*10,"points"), type="open")
        }
      else{
        arrow = arrow(length = unit(params$arrowsize*10,"points"), type="closed")
      }
      }
      else arrow = arrow
    }
    with(data, grobTree(
      grid::pointsGrob(0.5 + .15*params$directed, 0.5, pch = data$shape,
                       gp = grid::gpar(col = alpha(data$colour, data$alpha),
                                       fill = alpha(data$fill, data$alpha),
                                       fontsize = data$size * .pt + data$stroke * .stroke/2,
                                       lwd = data$stroke * .stroke/2)) #,
#      grid::linesGrob(x = unit(c(1, 0), "npc"), y = unit(c(0.5, 0.5), "npc"),
#                      gp = grid::gpar(
#                        col = data$colour, data$ecolour %||% "grey60", # not right yet
#                        lwd = data$linewidth, lineend="butt",
#                        lty = linetype), arrow = arrow)
    ))
  },

  setup_data = function(data, params, mapping) {
#    cat("setup_data geom_net\n")

#browser()
    data$from <- as.character(data$from)
    data$to <- as.character(data$to)
    selfie <- (data$from == data$to) & (params$selfies == TRUE)
  # maximum radius is at the moment hard coded to 0.05
    data$ymax = max(with(data, pmax(y, yend) + 2*0.05*selfie))
    data$xmax = with(data, pmax(x, xend) + 2*0.05*selfie)

    data$from <- factor(data$from)
    data$to <- factor(data$to)

    data
  },

  draw_panel = function(data, panel_scales, coord,  ecolour=NULL, ealpha=NULL, arrow=NULL, arrowgap=0.01,
                        directed=FALSE, arrowsize=1,
                        label=FALSE, labelcolour=NULL, selfies = FALSE) {

 #   browser()
    data$self <- as.character(data$to) == as.character(data$from)
    edges <- data.frame(
      x = data$x,
      xend = data$xend,
      y = data$y,
      yend = data$yend,
      weight = data$weight,
      colour = ecolour %||% ifelse(data$.samegroup, data$colour, "grey60"),
      size = data$linewidth %||% (data$size / 4),
      nodesize = data$size,
      alpha = ealpha %||% data$alpha,
      linetype=data$linetype,
      stroke = data$stroke,
      self = data$self,
      stringsAsFactors = FALSE
    )

    selfy <- subset(edges, self == TRUE)
    edges <- subset(edges, self != TRUE) # what are we going to do with self references?
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
#    browser()
    if (any(data$curvature != 0))
      edges_draw <- GeomCurve$draw_panel(edges, panel_scales,
                                         coord, arrow=arrow, curvature=data$curvature[1], angle=90)
    else edges_draw <- GeomSegment$draw_panel(edges, panel_scales, coord, arrow)

#    browser()

    selfies_draw <- NULL
    if ((nrow(selfy) > 0) & (selfies == TRUE)) {
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
    if ((nrow(selfy) > 0) & (selfies == TRUE) & (directed == TRUE)) {
#      browser()
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
    if (label | !is.null(data$label)) {
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
        vjust = data$vjust,
        stringsAsFactors = FALSE
      )
      labels <- unique(labels)
      label_grob <- GeomText$draw_panel(labels, panel_scales, coord)
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

