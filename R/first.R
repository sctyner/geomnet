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
  browser()
  GeomNet$new(mapping = mapping, data = data, stat = stat, position = position, 
              na.rm = na.rm, ...)
}

GeomNet <- proto::proto(ggplot2:::Geom, {
  objname <- "net"
  
  draw_groups <- function(., ...) .$draw(...)
  draw <- function(., data, coordinates, na.rm = FALSE, ...) {    
    browser()
    # net is network object from network package
    #    if (empty(data)) return(zeroGrob())
    if (empty(net)) return(zeroGrob())
    
    
    m <- as.matrix.network.adjacency(net) # get sociomatrix #pkg: network
    # get coordinates from Fruchterman and Reingold's force-directed placement algorithm.
    plotcord <- data.frame(gplot.layout.fruchtermanreingold(m, NULL))  #pkg: sna
    # or get it them from Kamada-Kawai's algorithm: 
    # plotcord <- data.frame(gplot.layout.kamadakawai(m, NULL)) # pkg: sna
    colnames(plotcord) <- c("x","y")
    nodes_grob <- GeomPoint$draw(plotcord, ...)
    
    edglist <- as.matrix.network.edgelist(net) #pkg: network
    edges <- data.frame(plotcord[edglist[,1],], plotcord[edglist[,2],])
    plotcord$elements <- as.factor(get.vertex.attribute(net, "elements")) #pkg: network
    colnames(edges) <-  c("x","y","xend","yend")
    edges_grob <- GeomSegment$draw(edges, ...) #add $draw like in nodes_grob
    
    with(coord_transform(coordinates, data), #got rid of scales b/c we don't have it anywhere else
         ggname(.$my_name(), grobTree(
           nodes_grob,
           edges_grob
         )))
  }
   draw_legend <- function(., data, ...) {
     data <- aesdefaults(data, .$default_aes(), list(...))
  
    with(data,
       pointsGrob(0.5, 0.5, size=unit(size, "mm"), pch=shape, 
       gp=gpar(
       col=alpha(colour, alpha), 
       fill=alpha(fill, alpha), 
       fontsize = size * .pt)
   )
   )
   }
  
  browser()
  default_stat <- function(.) StatIdentity
  required_aes <- c("net")
  default_aes <- function(.) aes(shape=16, colour="black", size=10, fill = NA, alpha = NA) 
})





#' @export
StatNet <- proto::proto(ggplot2:::Stat, {
  objname <- "net"
  
  calculate_groups <- function(., data, na.rm = FALSE, width = NULL,
                               scale = "area", ...) {

    print("calculate net")
        browser()
    data <- remove_missing(data, na.rm, "y", name = "stat_net", finite = TRUE)
    data <- .super$calculate_groups(., data, na.rm = na.rm, width = width, ...)
      print("calculate net 2")
      browser()
#     
#     # choose how violins are scaled relative to each other
#     scale <- match.arg(scale, c("area", "count", "width"))
#     data$vasewidth <- switch(scale,
#                              # area : keep the original densities but scale them to a max width of 1
#                              #        for plotting purposes only
#                              area = data$ydensity / max(data$ydensity),
#                              # count: use the original densities scaled to a maximum of 1 (as above)
#                              #        and then scale them according to the number of observations
#                              count = (data$ydensity / max(data$ydensity)) * data$n / max(data$n),
#                              # width: constant width (density scaled to a maximum of 1)
#                              width = data$scaled
#     )
#     
#     data
   }
   
   calculate <- function(., data, scales, width=NULL, adjust=1, kernel="gaussian",
                         trim=TRUE, na.rm = FALSE, ...) {
     print("calculate")
    browser()
#     data$weight <- 1
#     data$weight <- data$weight %||% 1
#     width <- width %||%  resolution(data$x) * 0.75
#     
#     fivenum <- StatBoxplot$calculate(data=data, width=width, ...)
#     data$weight <- 1/sum(data$weight)
#     densdf <- StatYdensity$calculate(data=data, adjust=adjust, ...)
#     
#     densdf$fivenum <- I(list(fivenum))
#     densdf
   }
  
  default_geom <- function(.) GeomNet
  required_aes <- c("net")
  
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
