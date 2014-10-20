#' test function
#' 
#' this is what the function does
#' @export
#' @param x numeric vector
#' @param ... addiional parameters passed on to min
#' @return minimum of the vector x
minx <-  function(x, ...) return(min(x, ...))

#' @export
#' @example
#' data(mtcars)
#' library(ggplot2)
#' ggplot(data=mtcars) + geom_net(aes(x=mpg, y=disp)) 
geom_net <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
na.rm = FALSE, ...) {
  GeomNet$new(mapping = mapping, data = data, stat = stat, position = position, 
  na.rm = na.rm, ...)
}


#' @export
GeomNet <- proto::proto(ggplot2:::Geom, {
  objname <- "net"

  draw_groups <- function(., ...) .$draw(...)
  draw <- function(., data, scales, coordinates, na.rm = FALSE, ...) {    
    data <- remove_missing(data, na.rm, 
      c("x", "y", "size", "shape"), name = "geom_net")
    if (empty(data)) return(zeroGrob())
    
    with(coord_transform(coordinates, data, scales), 
      ggname(.$my_name(), pointsGrob(x, y, size=unit(size, "mm"), pch=shape, 
      gp=gpar(col=alpha(colour, alpha), fill = alpha(fill, alpha), fontsize = size * .pt)))
    )
  }

  # draw_legend <- function(., data, ...) {
    # data <- aesdefaults(data, .$default_aes(), list(...))
    
    # with(data,
      # pointsGrob(0.5, 0.5, size=unit(size, "mm"), pch=shape, 
      # gp=gpar(
        # col=alpha(colour, alpha), 
        # fill=alpha(fill, alpha), 
        # fontsize = size * .pt)
      # )
    # )
  # }

  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(shape=16, colour="black", size=10, fill = NA, alpha = NA) 
})

#' Moritz Marbach's function
#' 
#' on blog at http://sumtxt.wordpress.com/2011/07/02/visualizing-networks-with-ggplot2-in-r/
#' updated to new ggplot2 reqs
plotg <- function(net, value=NULL) {
  m <- as.matrix.network.adjacency(net) # get sociomatrix #pkg: network
  # get coordinates from Fruchterman and Reingold's force-directed placement algorithm.
  plotcord <- data.frame(gplot.layout.fruchtermanreingold(m, NULL))  #pkg: sna
  # or get it them from Kamada-Kawai's algorithm: 
  # plotcord <- data.frame(gplot.layout.kamadakawai(m, NULL)) # pkg: sna
  colnames(plotcord) = c("X1","X2")
  edglist <- as.matrix.network.edgelist(net) #pkg: network
  edges <- data.frame(plotcord[edglist[,1],], plotcord[edglist[,2],])
  plotcord$elements <- as.factor(get.vertex.attribute(net, "elements")) #pkg: network
  colnames(edges) <-  c("X1","Y1","X2","Y2")
  edges$midX  <- (edges$X1 + edges$X2) / 2
  edges$midY  <- (edges$Y1 + edges$Y2) / 2
  pnet <- ggplot()  + 
    geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2), 
                 data=edges, size = 0.5, colour="grey") +
    geom_point(aes(X1, X2,colour=elements), data=plotcord) +
    scale_colour_brewer(palette="Set1") +
    scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
    # discard default grid + titles in ggplot2 
    theme(panel.background = element_blank(), legend.position="none", #changed opts to theme
          axis.title.x = element_blank(), axis.title.y = element_blank(), #changed theme_blank to element_blank 
          legend.background = element_rect(colour = NA),
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.grid.minor = element_blank(), panel.grid.major = element_blank())
  return(print(pnet))
}
