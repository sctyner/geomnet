"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#' @export
StatNet <- ggplot2::ggproto("StatNet", ggplot2::Stat,
  required_aes = c("from_id", "to_id"),
  non_missing_aes = "weight",

  setup_params = function(data, params) {
    print(str(params))

    params
  },

  setup_data = function(data, params) {
    browser()
  #  data <- remove_missing(data, na.rm, "y", name = "stat_net", finite = TRUE)

    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
#     plyr::ddply(data, "group", transform,
#                 ymin = min(y),
#                 ymax = max(y),
#                 xmin = x - width / 2,
#                 xmax = x + width / 2
#     )
    data
  },
#  compute_group = function(data, scales, params, na.rm = FALSE,
#                           layout="kamadakawai", layout.par=list(), vertices=NULL,  ...) {
#    data
#  },
  compute_panel = function(self, data, scales, params, na.rm = FALSE,
                           layout="kamadakawai", layout.par=list(), vertices=NULL) {
browser()
    edges <- data[,c('from_id', 'to_id')]
    #3/17 - the next two lines are the source of the deletion of lone vertices.
    net <- network::as.network(edges, matrix.type = "edgelist") #from network package
    m <- network::as.matrix.network.adjacency(net)

    #print("it would be nice at this point to check, whether layout is one of the supported functions, and if not,

    layoutFun <- paste('gplot.layout.',layout,sep='')
    vert.coord <- data.frame(do.call(layoutFun, list(m, layout.par = layout.par)))

    vert.coord$label <- row.names(m)
    vert.coord$X1 <- scale(vert.coord$X1, center=min(vert.coord$X1), scale=diff(range(vert.coord$X1))) # center nodes
    vert.coord$X2 <- scale(vert.coord$X2, center=min(vert.coord$X2), scale=diff(range(vert.coord$X2)))
    names(vert.coord) <- c("x", "y", "label")

    edgelist <- network::as.matrix.network.edgelist(net) #network pkg
    edge.coord <- data.frame(vert.coord[edgelist[,1],], vert.coord[edgelist[,2],], row.names=NULL)
    names(edge.coord) <- c('x','y', "from", 'xend','yend', "to")
    edges <- data.frame(data, edge.coord[, c("x", "y", "xend", "yend")])


    # vertices data set gets stuffed into the first element of each panel
    if (!is.null(vertices)) {
      vert.coord <- merge(vert.coord, vertices, by="label")
    }

#     vert.coord$edges <- NA
#     vert.coord$edges[1] <- I(list(edges))
#     vert.coord
    edges$vertices <- NA
    edges$vertices[1] <- I(list(vert.coord))
    edges
  }


)

#' @rdname geom_net
#'
#' @return A data frame with additional columns:
#'   \item{density}{density estimate}
#'   \item{scaled}{density estimate, scaled to maximum of 1}
#'   \item{count}{density * number of points - not sure about usefulness for vase plots}
#'   \item{vasewidth}{density scaled for the vase plot, according to area, counts
#'                      or to a constant maximum width}
#'   \item{n}{number of points}
#'   \item{width}{width of vase bounding box}
#' @export
stat_net <- function(mapping = NULL, data = NULL, geom = "point",
                     position = "identity", show.legend = NA,
                     inherit.aes = TRUE, layout="kamadakawai", layout.par=list(),
                     vertices=NULL, na.rm=FALSE, ...) {
  layer(
    stat = StatNet, data = data, mapping = mapping, geom = geom, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(vertices=vertices, layout=layout, layout.par=layout.par,
                  na.rm=na.rm, ...
    )
  )
}
