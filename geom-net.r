#' Geom for visualizing a network.
#' #Documentation to be added.
#' 

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
