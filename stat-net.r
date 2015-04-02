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
stat_net <- function (mapping = NULL, data = NULL, vertices = NULL,  geom = "net", position = "identity", ...) {
#  print("new net stat")

  StatNet$new(mapping = mapping, data = data, vertices = vertices, geom = geom,
  position = position, ...)
}

StatNet <- proto(Stat, {
  objname <- "net"
  default_geom <- function(.) GeomNet
  default_aes <- function(.) aes(colour = "black", size =3, layout="kamadakawai",layout.par = NA)
  #required_aes <- c("x", "y")
  #required_aes <- c("from_id", "to_id")

  calculate_groups <- function(., data, coordinates=coordinates, vertices = vertices,  ...){
#    print("calculate groups net")
#    browser()

    data$group <-1
    .super$calculate_groups(., data, coordinates=coordinates, vertices = vertices, ...)
  }

#calculate function: extracts smaller data frame from original data using just req params. then computes some other stuff.
  calculate <- function(., data, na.rm = FALSE,  coordinates=coordinates, vertices = vertices, ...) {  #add requirements here later.
#    print("calculate net")
#    browser()

    #compute x and y using layout method if x,y are not there. then can assume that x and y are there from now on.
    require(network)
    require(sna) #how/where to call network, sna, pkg here?
    stopifnot(is.data.frame(data))
    edges <- data[,c('from_id', 'to_id')]
    #3/17 - the next two lines are the source of the deletion of lone vertices.
    net <- network::as.network(edges, matrix.type = "edgelist") #from network package
    m <- network::as.matrix.network.adjacency(net)
   # 3/17 attempt to fix issue. error: argument "vertices" is missing with no default. took out vertices above.
   #  iso.vert.idx <- which(as.character(data$vertices[,1]) %in% rownames(m))  #require first column in df to be the names of vertices
      #paste "layout name" instead of 'kamadakawai' (2/15)
   # vert.coord <- data.frame(do.call(paste('sna::gplot.layout.',layout,sep=''), list(m, NULL)))
  if (is.null(list(...)$layout))
    layout <- .$default_aes()$layout
 else layout <- list(...)$layout
 if (is.null(list(...)$layout.par))
   layout.par <- .$default_aes()$layout.par
 else layout.par <- list(...)$layout.par
#print("it would be nice at this point to check, whether layout is one of the supported functions, and if not,
 #suggest to the user which functions are supported ... what about layout parameters?")
  #3/17 - need to fix the stuff below as well to ensure that standalone vertices get plotted!

if (is.na(layout.par)){
    vert.coord <- data.frame(do.call(paste('gplot.layout.',layout,sep=''), list(m, NULL)))
  }
  else vert.coord <- data.frame(do.call(paste('gplot.layout.',layout,sep=''), list(m, layout.par = layout.par)))
#   vert.coord <- data.frame(sna::gplot.layout.kamadakawai(m, NULL)) # from sna package
 #   vert.labels <- unique(c(as.character(edges$from),as.character(edges$to)))
 #   vert.coord$name <- sort(vert.labels) #needed here?
    vert.coord$label <- row.names(m)
    vert.coord$X1 <- scale(vert.coord$X1, center=min(vert.coord$X1), scale=diff(range(vert.coord$X1))) # center nodes
    vert.coord$X2 <- scale(vert.coord$X2, center=min(vert.coord$X2), scale=diff(range(vert.coord$X2)))
    edgelist <- as.matrix.network.edgelist(net) #network pkg
    edge.coord <- data.frame(vert.coord[edgelist[,1],], vert.coord[edgelist[,2],])
    names(edge.coord) <- c('x','y', "from", 'xend','yend', "to")
    coordinates <- coord_cartesian(xlim=range(edge.coord$x), ylim=range(edge.coord$y))
    #edge.coord$grob_id <- 1:nrow(edge.coord)
    #at the end of this routine, we need to have an x,xend and a y,yend. CHECK!
    data.frame(edges = I(list(data.frame(edge.coord,data))), vertices = I(list(vert.coord)),
               coordinates = I(coordinates))
  }
})
