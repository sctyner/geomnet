"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


#' @export
StatNet <- ggplot2::ggproto("StatNet", ggplot2::Stat,
  required_aes = c("from_id", "to_id"),
  non_missing_aes = "weight",

  setup_params = function(data, params) {
#    browser()
#    print(str(params))

    params
  },

  setup_data = function(self, data, params) {

    fiteach=params$fiteach
    if (!is.factor(data$from_id)) data$from_id <- factor(data$from_id)
    if (!is.factor(data$to_id)) data$to_id <- factor(data$to_id)

    if (!is.null(params$vertices)) {
      data <- merge(data, params$vertices, by.x="from_id", by.y="label", all=TRUE)
    }

    # we want to keep all of the values that are NA in the second edge - give them a special value, so we can pull them out later
    levels <- levels(data$to_id)
    data$to_id <- as.character(data$to_id)
    data$to_id[is.na(data$to_id)] <- "..NA.."
    data$to_id <- factor(data$to_id, levels = c(levels, "..NA.."))

    # check that all vertices are included in the data set
    only_to <- setdiff(levels(data$to_id), levels(data$from_id))
    only_to <- setdiff(only_to, "..NA..")
    if (length(only_to) > 0)
      warning(sprintf("There are %d nodes without node information: %s\n\nDid you use all=T in merge?\n\n", length(only_to), paste(only_to, collapse=", ")))

    if (! is.null(params$seed)) set.seed(params$seed)
    if (fiteach) return(data)

    data$.samegroup <- FALSE

    self$compute_network(data, layout=params$layout, layout.par=params$layout.par)
  },

compute_network = function(data, layout="kamadakawai", layout.par=list()) {
  require(dplyr)
  edges <- subset(data, to_id != "..NA..")[,c('from_id', 'to_id')]
  edges <- edges %>% group_by(from_id, to_id) %>% summarise(wt = n())

  net <- network::as.network(edges[,1:2], matrix.type = "edgelist")

  edgeweights <- diff(range(edges$wt)) != 0
  if (edgeweights) {
    # make a (weighted) sna edgelist
    require(sna)

    edgelist <- sna::as.edgelist.sna(net) #sna pkg
    edgelist[,3] <- sqrt(edges$wt)  # doesn't change anything for wt == const
  } else {
    edgelist <- network::as.matrix.network.edgelist(net) #network pkg
  }
#  else {
#    m <- network::as.matrix.network.adjacency(net)
#  }

  if (is.null(layout)) {
    if (is.null(data$x) || is.null(data$y)) stop("If no layout mechanism is specified, x and y coordinates have to be given\n\n")
    vert.coord <- data[, c("x", "y", "from_id")]
    vert.coord <- subset(vert.coord, from_id %in% attr(edgelist, "vnames"))
    vert.coord <- unique(vert.coord)
#    vert.coord$x <- as.numeric(scale(vert.coord$x, center=min(vert.coord$x), scale=diff(range(vert.coord$x))))
#    vert.coord$y <- as.numeric(scale(vert.coord$y, center=min(vert.coord$y), scale=diff(range(vert.coord$y))))
    names(vert.coord)[3] <- "label"
  } else {
  #print("it would be nice at this point to check, whether layout is one of the supported functions, and if not,
  require(sna)
  layoutFun <- paste('gplot.layout.',layout,sep='')
  vert.coord <- data.frame(do.call(layoutFun, list(edgelist, layout.par = layout.par)))

  vert.coord$label <- attr(edgelist, "vnames") #row.names(m)
  vert.coord$X1 <- as.numeric(scale(vert.coord$X1, center=min(vert.coord$X1), scale=diff(range(vert.coord$X1)))) # center nodes
  vert.coord$X2 <- as.numeric(scale(vert.coord$X2, center=min(vert.coord$X2), scale=diff(range(vert.coord$X2))))
  names(vert.coord) <- c("x", "y", "label")
}

  edge.coord <- data.frame(vert.coord[edgelist[,1],], vert.coord[edgelist[,2],], row.names=NULL)
  names(edge.coord) <- c('x','y', "from", 'xend','yend', "to")

  relVars <- setdiff(names(data), c("x", "y"))
  fromto <- subset(data, to_id != "..NA..")[,relVars]
  edges <- merge(edge.coord, fromto, by.x=c("from", "to"), by.y=c("from_id", "to_id"), all=TRUE)

  fromonly <- subset(data, to_id == "..NA..")[,relVars]
  if (nrow(fromonly) > 0) {
    fromonly <- merge(fromonly, edge.coord[,c("xend", "yend", "to")], by.x = "from_id", by.y="to", all.x=T)
    fromonly <- transform(fromonly,
                          from = from_id, to=to_id,
                          x=xend, y=yend, xend=NA, yend=NA)

    edges <- rbind(edges, fromonly[, names(edges)])
  }
#browser()
#  edges <- edges %>% group_by(from, to) %>% mutate(n = n())
  unique(edges)
},
  compute_panel = function(self, data, scales, params, na.rm = FALSE,
                           layout="kamadakawai", layout.par=list(), fiteach=FALSE,
                           vertices=NULL) {
    if (fiteach) data <- self$compute_network(data, layout=layout, layout.par=layout.par)

    data <- plyr::ddply(data, "group", plyr::mutate, .samegroup = to %in% unique(from))

    data
  }

)

#' @rdname geom_net
#'
#' @return A data frame with additional columns:
#'   \item{x, y}{coordinates of the nodes, beginning of edges,}
#'   \item{xend, yend}{coordinates end points of edges.}
#' @export
stat_net <- function(mapping = NULL, data = NULL, geom = "point",
                     position = "identity", show.legend = NA,
                     inherit.aes = TRUE, layout="kamadakawai", layout.par=list(), fiteach=FALSE, vertices=NULL,
                     na.rm=FALSE, ...) {
  layer(
    stat = StatNet, data = data, mapping = mapping, geom = geom, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(layout=layout, layout.par=layout.par, fiteach=fiteach,
                  na.rm=na.rm, vertices=vertices, ...
    )
  )
}
