#' Geom for visualizing a network.
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
#' g <-
#' (150, directed=FALSE, density=0.03)
#' classes <- rbinom(150,1,0.5) + rbinom(150,1,0.5) + rbinom(150,1,0.5)
#' set.vertex.attribute(g, "elements", classes)
#' plotg(g)
#'
#' # make a theme for networks:
#' theme_net <- theme(aspect.ratio = 1, line = element_blank(), panel.background = element_blank(), plot.background = element_blank(), axis.text=element_blank(),axis.title=element_blank())
#' data(blood) #3/16 - added to the data file in dropbox. a list with an edges df and a vertices df.
#' #node_id is a required aesthetic. #i have yet to match it with the nodes data in the geom code.
#' ggplot(data = blood$edges, aes(from_id = from, to_id = to)) + geom_net(vertices = blood$vertices) + expand_limits(x = c(-5,5), y = c(-5,5))

#' #ecolour by group_to doesnt work
#' ggplot(data = blood$edges, aes(from_id = from, to_id = to)) + geom_net(vertices = blood$vertices, aes(ecolour = group_to)) + expand_limits(x = c(-5,5), y = c(-5,5))
#' #facetting seems to work?
#' ggplot(data = blood$edges, aes(from_id = from, to_id = to)) + geom_net(vertices = blood$vertices) + expand_limits(x = c(-5,5), y = c(-5,5)) + facet_wrap(~group_to)
#' #Mad Men Example
#' sex <- c("m", "f", "m", "m", "f", "f", "f", "m", "f", "m", "f", "m", "m", "f", "m", "m", "f", "m", "m",
#' rep("f",9), "m", "m", "m", "f", "f", "f", "f", "m", "f", "m", "f", "f", "m", "f", "m", "m", "f",
#' "m", "f", "f", "f", "f", "f", "f")
#' data(madmen2, package="gcookbook")
#' mm.vert <- data.frame(Name = as.factor(unique(c(madmen2$Name1,madmen2$Name2))), Gender = as.factor(sex))
#' ggplot(data = madmen2, aes(from_id = Name1, to_id = Name2)) + geom_net(vertices = mm.vert, vsize=3, esize=1.5, vlabel=TRUE) + expand_limits(x = c(-15,15), y = c(-15,15))
#' # we'd like to get this to work:
#' ggplot(data = madmen2, aes(from_id = Name1, to_id = Name2, vsize = 2)) + geom_net(vertices = mm.vert, aes(vcolour=Gender), size=I(2)) + expand_limits(x = c(-15,15), y = c(-15,15))


# Getting aes from data but not applying it to the new data.frames named edges and vertices
# when doing the grouping. how to do this?

#geom_map <- function(mapping = NULL, data = NULL, map, stat = "identity", ...) {

geom_net <- function (mapping = NULL, data = NULL, vertices = NULL, vlabel = FALSE, directed = FALSE, stat = "net", ...) {
#browser()
#  print("new net geom")
  vmapping <- mapping[grep("^v", names(mapping))]
  if (length(vmapping) > 0) {
    names(vmapping) <- gsub("^v", "", names(vmapping))
    p <- GeomBar$new(data=vertices, mapping = vmapping, ...)
    vertices <- data.frame(p$data, p$compute_aesthetics(data=vertices, ggplot()))
#browser()
 #   scales <- p$scales
  #  vertices <- lapply(vertices, scales_transform_df, scales = scales)

  }

  idx <- grep("^v", names(mapping))
  if (length(idx) > 0) mapping <- mapping[-idx]
  GeomNet$new(vertices = vertices, mapping = mapping,
              data = data, vlabel = vlabel, directed = directed, stat = stat,  ...)
}

GeomNet <- proto(Geom, {
  objname <- "net"
  #error: argument "coordinates" is missing, with no default
  #draw <- function(., data, coordinates, ...) {
  draw <- function(., data, vlabel = vlabel, vertices = vertices, directed = directed, ...) {
#    print("draw net")

    edges <- data.frame(data$edges[[1]])
    #added 2/2 from geom_boxplot
# only take the values from data, if there are no columns of the same name in edges
    aes <- setdiff(c("ecolour", "elinetype", "ealpha", "esize"), names(edges))
    for (x in aes) {
      edges[,x] <- data[, x]
    }
    # get "e" out of the aesthetics' name
    idx <- which(names(edges) %in% c("ecolour", "elinetype", "ealpha", "esize"))
    names(edges)[idx] <- gsub("^e", "", names(edges)[idx])

# need to be merged, but it's a bit tricky ...
    if (!is.null(vertices))
      vertices <- merge(data$vertices[[1]], vertices, by="label")
    else vertices <- data$vertices[[1]]

    aes <- setdiff(c("colour", "shape", "fill", "alpha", "size"), names(vertices))
    for (x in aes) {
      vertices[,x] <- data[, paste("v",x, sep="")]
    }
    vertices$group = data$group
    names(vertices)[2:3] <- c("x", "y")
    #    linesGrob(data[,c('x','y')], data[,c('xend','yend')], default.units = "native", id = grob_id)
    #    pointsGrob(vertices$x, vertices$y, pch = 1, size = unit(1, "char"), default.units = "native", name = NULL,
    #               gp = gpar(), vp = NULL)
    #      GeomSegment$draw(edges, scales, coordinates, colour = ecolour, alpha = ealpha, ...),
labellayer <- NULL
    if (vlabel == TRUE){
      vertex.labels <- data.frame(vertices, angle = 0, hjust = 0, vjust = 0, family = "", fontface = 1,
                                 lineheight = 1.2)
      vertex.labels$size <- vertex.labels$size * 2
#      ggname(.$my_name(), grobTree(
 #       GeomSegment$draw(data=edges,...),
  #      GeomPoint$draw(data=vertices,...),
   labellayer <-     GeomText$draw(data=vertex.labels,...)
  #    ))
    }
  edges$arrow <- NULL
#browser()
  if (directed == TRUE){
    edges$arrow <- arrow(length = unit(.015,"npc"))
    edges$arrow$length[which(edges$from == edges$to)] <- unit(0,"npc")
  }
#     else{
    ggname(.$my_name(), grobTree(   #tried to add the coordinates we made to the draw statement to no avail. it doesn't even get into the draw function without expand limits
      GeomSegment$draw(data=edges,...), #colour=colour, alpha=alpha, fill=fill, linetype=linetype, size=size, coordinates=coordinates, scales=scales, ...),
      GeomPoint$draw(data=vertices,...), # colour=colour, alpha=alpha, fill=fill, shape=shape, size=size, coordinates=coordinates, scales=scales, ...)
    labellayer
    ))
#  }
    }


  guide_geom <- function(.) "point"

  default_stat <- function(.) StatNet
 # required_aes <- c('from_id', 'to_id')
 required_aes <- c('vertices','edges') # 'id'
  default_aes <- function(.) {
    aes(ecolour = "grey20", vcolour = "black", vfill="black", fill = NA, esize = 1, vsize = 2, elinetype = 1, weight = NA, valpha = 1, vshape = 16, ealpha=1)
  }
})
