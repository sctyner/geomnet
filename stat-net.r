#' 2d density estimation.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "density2d")}
#'
#' @param contour If \code{TRUE}, contour the results of the 2d density
#'   estimation
#' @param n number of grid points in each direction
#' @param ... other arguments passed on to \code{\link{kde2d}}
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @inheritParams stat_identity
#' @return A data frame in the same format as \code{\link{stat_contour}}
#'
#@importFrom network
#' @export
#' @examples
#' \donttest{
#' #put in example here.
#' }
stat_net <- function (mapping = NULL, data = NULL, geom = "net", position = "identity",
na.rm = FALSE, ...) {
  StatNet$new(mapping = mapping, data = data, geom = geom,
  position = position, na.rm = na.rm, layout = NULL, ...)
}

StatNet <- proto(Stat, {
  objname <- "net"

  default_geom <- function(.) GeomNet
  default_aes <- function(.) aes(colour = "#3366FF", size = 0.5)
  #required_aes <- c("x", "y")
   required_aes <- c("layout")

#calculate function: extracts smaller data frame from original data using just req params. then computes some other stuff.
  calculate <- function(., data, scales, na.rm = FALSE,  layout, ...) {  #add requirements here later.
  	#compute x and y using layout method if x,y are not there. then can assume that x and y are there from now on.
    #library(network,sna) #how/where to call igraph pkg here?
    browser()
    df <- data.frame(data)
     #first column of data frame needs to be the 'from' vertex.
     #second column of data frame needs to be the 'to' vertex. other cols can be anything
    edges <- data.frame("from" = df[,1], "to" = df[,2])
    vert.labels <- unique(c(edges$from,edges$to)) #needed here?
    net <- as.network(edges, matrix.type = "edgelist") #from network package
    m <- as.matrix.network.adjacency(net)
    #change the stuff below later to include multiple layouts (in if else statements?)
    vert.coord <- data.frame(gplot.layout.kamadakawai(m, NULL)) # from sna package
    vert.coord$name <- sort(vert.labels) #needed here?
    edgelist <- as.matrix.network.edgelist(net)
    edge.coord <- data.frame(vert.coord[edgelist[,1],], vert.coord[edgelist[,2],])
    names(edge.coord) <- c('x','y','xend','yend')
    #at the end of this routine, we need to have an x and a y. CHECK!

#where to put this?
#     net.plot <- ggplot()  +
#       geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
#                    data = edge.coord, size = , colour="grey") +
#       geom_point(aes(X1, X2,colour=elements), data=plotcord) +
#       scale_colour_brewer(palette="Set1") +
#       scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
#       # discard default grid + titles in ggplot2
#       theme(panel.background = element_blank(), legend.position="none", #changed opts to theme
#             axis.title.x = element_blank(), axis.title.y = element_blank(), #changed theme_blank to element_blank
#             legend.background = element_rect(colour = NA),
#             panel.background = element_rect(fill = "white", colour = NA),
#             panel.grid.minor = element_blank(), panel.grid.major = element_blank())
#

  }
})
