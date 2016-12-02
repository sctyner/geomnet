"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#' @rdname geom_net
#' @importFrom sna as.edgelist.sna
#' @importFrom network as.matrix.network.edgelist
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @export
StatNet <- ggplot2::ggproto("StatNet", ggplot2::Stat,
  required_aes = c("from_id", "to_id"),
  non_missing_aes = "weight",
  setup_params = function(data, params) {
  #  browser()
#    print(str(params))

    params
  },

#   setup_params = function(data, params) {
#     cat("setup_params\n")
# #    #browser()
# #    print(str(params))
#
#     params
#   },

  setup_data = function(self, data, params) {
#cat("setup_data stat_net\n")
    #browser()
    fiteach=params$fiteach
#    if (!is.factor(data$from_id)) data$from_id <- factor(data$from_id)
#    if (!is.factor(data$to_id)) data$to_id <- factor(data$to_id)

    if (!is.null(params$vertices)) {
      data <- merge(data, params$vertices, by.x="from_id", by.y="label", all=TRUE)
    }

    # we want to keep all of the values that are NA in the second edge - give them a special value, so we can pull them out later
    levels <- levels(as.factor(data$to_id))
    data$to_id <- as.character(data$to_id)
    # add .selfie variable to get true selfies out later.
    data$.selfie <- as.character(data$from_id) == data$to_id
    data$to_id[is.na(data$to_id)] <- as.character(data$from_id)[is.na(data$to_id)]
#    data$to_id[is.na(data$to_id)] <- "..NA.."
#    data$to_id <- factor(data$to_id, levels = c(levels, "..NA.."))

    # check that all vertices are included in the data set
    only_to <- setdiff(levels(data$to_id), levels(data$from_id))
    only_to <- setdiff(only_to, "..NA..")
    if (length(only_to) > 0)
      warning(sprintf("There are %d nodes without node information: %s\n\nDid you use all=T in merge?\n\n", length(only_to), paste(only_to, collapse=", ")))

    if (! is.null(params$seed)) set.seed(params$seed)
#    if (fiteach) return(data)

#    data$.samegroup <- FALSE

#    self$compute_network(data, layout=params$layout, layout.par=params$layout.par)
    data
  },

compute_network = function(data, layout.alg="kamadakawai", layout.par=list()) {
# cat("compute_network\n")
#browser()

  edges <- subset(data, to_id != "..NA..")[,c('from_id', 'to_id')]
  edges <- dplyr::group_by(edges, from_id, to_id)
  edges <- dplyr::summarise(edges, wt = n())

    # there should not be any missing values at this point, but just make sure
  if (any(is.na(edges$from_id))) message(sprintf("%d missing values excluded\n", sum(is.na(edges$from_id))))
  net <- network::as.network(na.omit(edges[,1:2]), matrix.type = "edgelist")

  edgeweights <- diff(range(edges$wt)) != 0
  if (edgeweights) {
    # make a (weighted) sna edgelist
    edgelist <- sna::as.edgelist.sna(net) #sna pkg
    edgelist[,3] <- sqrt(edges$wt)  # doesn't change anything for wt == const
  } else {
    edgelist <- sna::as.edgelist.sna(net) # switched from network to sna for consistency
  }
#  else {
#    m <- network::as.matrix.network.adjacency(net)
#  }

  if (is.null(layout.alg)) {
    if (is.null(data$x) || is.null(data$y)) stop("If no layout mechanism is specified, x and y coordinates have to be given\n\n")
    vert.coord <- data[, c("x", "y", "from_id")]
    vert.coord <- subset(vert.coord, from_id %in% attr(edgelist, "vnames"))
    vert.coord <- unique(vert.coord)
#    vert.coord$x <- as.numeric(scale(vert.coord$x, center=min(vert.coord$x), scale=diff(range(vert.coord$x))))
#    vert.coord$y <- as.numeric(scale(vert.coord$y, center=min(vert.coord$y), scale=diff(range(vert.coord$y))))
    names(vert.coord)[3] <- "label"
  } else {
  #print("it would be nice at this point to check, whether layout is one of the supported functions, and if not,
# browser()
  layoutFun <- getFromNamespace(paste('gplot.layout.',layout.alg,sep=''), asNamespace("sna"))
#  requireNamespace("sna")
#  layoutFun <- paste('gplot.layout.',layout.alg,sep='')
#  vert.coord <- data.frame(do.call(layoutFun, list(edgelist, layout.par = layout.par)))
  vert.coord <- data.frame(layoutFun(edgelist, layout.par = layout.par))

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
  edges <- mutate(group_by(edges, from, to), weight = n())
  unique(edges)
},

compute_panel = function(self, data, scales, na.rm = FALSE,
                         layout.alg="kamadakawai", layout.par=list(),
                         fiteach=FALSE, vertices=NULL) {
#  cat("compute_panel in stat_net\n")
#  #browser()
#    if (fiteach)
      data <- self$compute_network(data, layout.alg =layout.alg, layout.par=layout.par)

    #    data <- plyr::ddply(data, "group", plyr::mutate, .samegroup = to %in% unique(from))
    if (any(data$group) != -1) {
      data <- mutate(group_by(data, group), .samegroup = to %in% unique(from))
    }

   # #browser()
    data.frame(data)
  },

compute_layer = function(self, data, params, layout, na.rm = FALSE, layout.alg,
                        # layout="kamadakawai", layout.par=list(),
                         fiteach=FALSE,
                         vertices=NULL) {
#  cat("compute_layer in stat_net\n")
#  #browser()

  if (params$fiteach) {
    # only do this plyr statement in the case that fiteach is true.
    plyr::ddply(data, "PANEL", function(data) {
      if (ggplot2:::empty(data)) return(data.frame())
#browser()
      scales <- #ggplot2:::Layout$get_scales(data$PANEL[1])
        list(x = NULL, y = NULL)
      self$compute_panel(data = data, scales = scales,
                         na.rm=params$na.rm, layout.alg=params$layout.alg,
                         layout.par=params$layout.par, fiteach=params$fiteach,
                         vertices=params$vertices)
    })
  }
  else {
    if (ggplot2:::empty(data)) return(data.frame())
#browser()
    scales <- list(x = NULL, y = NULL)
    #scales <- ggplot2:::Layout$get_scales(data$PANEL[1])
    self$compute_panel(data = data, scales = scales,
                       na.rm=params$na.rm, layout.alg=params$layout.alg,
                       layout.par=params$layout.par, fiteach=params$fiteach,
                       vertices=params$vertices)
  }
})

#' @rdname geom_net
#'
#' @param geom \code{geom_net}, the geom attached to \code{stat_net} is called \code{"net"}.
#' @return A data frame with additional columns:
#'   \item{x, y}{coordinates of the nodes, beginning of edges,}
#'   \item{xend, yend}{coordinates end points of edges.}
#' @export
stat_net <- function(mapping = NULL, data = NULL, geom = "net",
                     position = "identity", show.legend = NA,
                     inherit.aes = TRUE, layout.alg="kamadakawai", layout.par=list(), fiteach=FALSE, vertices=NULL,
                     na.rm=FALSE, ...) {
# #browser()
    ggplot2::layer(
    stat = StatNet, data = data, mapping = mapping, geom = geom, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(layout.alg=layout.alg, layout.par=layout.par, fiteach=fiteach,
                  na.rm=na.rm, vertices=vertices, ...
    )
  )
}
