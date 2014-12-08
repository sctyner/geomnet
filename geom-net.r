#' Add heatmap of 2d bin counts.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "bin2d")}
#'
#' @export
#' @inheritParams geom_point
#' @examples
#' This is Mortiz Marbach's plotg function, from his post at http://sumtxt.wordpress.com/2011/07/02/visualizing-networks-with-ggplot2-in-r/.
#' plotg <- function(net, value=NULL) {
#'  m <- as.matrix.network.adjacency(net) # get sociomatrix
#'   # get coordinates from Fruchterman and Reingold's force-directed placement algorithm.
#'  plotcord <- data.frame(gplot.layout.fruchtermanreingold(m, NULL))
#'   # or get it them from Kamada-Kawai's algorithm:
#'  # plotcord <- data.frame(gplot.layout.kamadakawai(m, NULL))
#'colnames(plotcord) = c("X1","X2")
#'edglist <- as.matrix.network.edgelist(net)
#'edges <- data.frame(plotcord[edglist[,1],], plotcord[edglist[,2],])
#'plotcord$elements <- as.factor(get.vertex.attribute(net, "elements"))
#'colnames(edges) <-  c("X1","Y1","X2","Y2")
#'edges$midX  <- (edges$X1 + edges$X2) / 2
#'edges$midY  <- (edges$Y1 + edges$Y2) / 2
#'pnet <- ggplot()  +
#'  geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2),
#'               data=edges, size = 0.5, colour="grey") +
#'  geom_point(aes(X1, X2,colour=elements), data=plotcord) +
#'  scale_colour_brewer(palette="Set1") +
#'  scale_x_continuous(breaks = NA) + scale_y_continuous(breaks = NA) +
#'  # discard default grid + titles in ggplot2
#'  opts(panel.background = theme_blank()) + opts(legend.position="none")+
#'  opts(axis.title.x = theme_blank(), axis.title.y = theme_blank()) +
#'  opts( legend.background = theme_rect(colour = NA)) +
#'  opts(panel.background = theme_rect(fill = "white", colour = NA)) +
#'  opts(panel.grid.minor = theme_blank(), panel.grid.major = theme_blank())
#' return(print(pnet))
#' }
#'
#'
#' g <- network(150, directed=FALSE, density=0.03)
#' classes <- rbinom(150,1,0.5) + rbinom(150,1,0.5) + rbinom(150,1,0.5)
#' set.vertex.attribute(g, "elements", classes)
#' plotg(g)

#geom_map <- function(mapping = NULL, data = NULL, map, stat = "identity", ...) {

geom_net <- function (mapping = NULL, data = NULL, nodes, stat = "net", ...) {
  GeomNet$new(geom_params = list(nodes = nodes, ...), mapping = mapping,
              data = data, stat = stat, ...)
}

GeomNet <- proto(Geom, {
  draw <- function(., data, scales, coordinates, ...) {
    linesGrob(data[,c('x','y')], data[,c('xend','yend')], default.units = "native", id = grob_id)
    pointsGrob(data$x, data$y, pch = 1, size = unit(1, "char"), default.units = "native", name = NULL,
               gp = gpar(), vp = NULL)
  }

  objname <- "net"

  guide_geom <- function(.) "point"

  default_stat <- function(.) StatNet
  required_aes <- c('node_id')
  default_aes <- function(.) {
    aes(colour = NA, fill = "grey60", size = 0.5, linetype = 1, weight = 1, , alpha = NA, shape = 16)
  }

})
