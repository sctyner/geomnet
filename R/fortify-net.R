#' Function for converting a network object into the correct format for use with geomnet
#'
#' @param model object of class \code{"network"}
#' @param data NULL - not used in this function
#' @param group character. Used for facetting. If you wish to facet on a network variable provide the name of that variable here.
#' @param ...  not used in this function
#' @examples
#' # class network (sna, network, statnet packages)
#'
#' library(network)
#' data(emon, package = "network")
#' fortify(emon$Cheyenne)
#'
#' @import network
#' @export
fortify.network <- function(model, data = NULL, group = NULL, ...){
  net <- model
  node.attr <- network::list.vertex.attributes(net)
  edge.attr <- network::list.edge.attributes(net)
  N <- network::network.size(net)
  P <- length(node.attr)
  node.data <- data.frame(matrix("", nrow = N, ncol = P+1), stringsAsFactors = F)
  names(node.data) <- c("ID", node.attr)
  node.data$ID <- 1:N
  for (i in 1:P){
    node.data[,(i+1)] <- network::get.vertex.attribute(net, node.attr[i])
  }
  NE <- nrow(network::as.edgelist(net))
  P2 <- length(edge.attr)
  edge.data <- data.frame(network::as.edgelist(net), matrix("", nrow = NE, ncol = P2), stringsAsFactors = F)
  names(edge.data) <- c("from", "to", edge.attr)

  for (i in 1:P2){
    edge.data[,(i+2)] <- network::get.edge.attribute(net, edge.attr[i])
  }

  if(!is.null(group)){
    nodes <- unique(node.data$ID)
    groups <- unique(edge.data[,group])
    allnodes <- expand.grid(nodes, groups, stringsAsFactors = F)
    names(allnodes) <- c("ID", group)
    node.data.expanded <- merge(allnodes, node.data, by = "ID")
    matchnames <- intersect(names(edge.data), names(node.data.expanded))
    dat <- merge(edge.data, node.data.expanded, by.x = matchnames, by.y = matchnames, all = T)
  } else {
  dat <- merge(edge.data, node.data, by.x = "from", by.y = "ID", all = T)
  }
  return(dat)
}
#' Function for converting an igraph object into the correct format for use with geomnet
#'
#' @param model A network object of class \code{"igraph"}.
#' @param data NULL - not used in this function
#' @param group character. Used for facetting. If you wish to facet on a network variable provide the name of that variable here.
#' @param ...  not used in this function
#' @examples
#' # class igraph (igraph, igraphdata packages)
#' library(igraph)
#' data(blood, package = "geomnet")
#' blood.igraph <- graph_from_data_frame(d = blood$edges, 
#'    directed = TRUE, vertices = unique(blood$vertices[, 1:3]))
#' fortify(blood.igraph)
#'
#' @export
fortify.igraph <- function(model, data = NULL, group = NULL, ...){
  net <- model
  node.data <- igraph::as_data_frame(net, what = "vertices")
  names(node.data)[1] <- "ID"
  edge.data <- igraph::as_data_frame(net, what = "edges")
  if(!is.null(group)){
    nodes <- unique(node.data$ID)
    groups <- unique(edge.data[,group])
    allnodes <- expand.grid(nodes, groups, stringsAsFactors = F)
    names(allnodes) <- c("ID", group)
    node.data.expanded <- merge(allnodes, node.data, by = "ID")
    matchnames <- intersect(names(edge.data), names(node.data.expanded))
    dat <- merge(edge.data, node.data.expanded, by.x = matchnames, by.y = matchnames, all = T)
  } else {
  dat <- merge(edge.data, node.data, by.x = "from", by.y = "ID", all = T )
  }
  return(dat)
}
#' Function for converting a network edge list in data frame form into the correct format for use with geomnet
#'
#' @param model A network edgelist of class \code{"edgedf"}. See \code{\link{as.edgedf}}. Can contain edge variables as well. 
#' @param data Data frame containing network node list and other node information. First column should contain node ids.
#' @param group character. Used for facetting. If you wish to facet on network variable provide the name of that variable here.
#' @param ...  not used in this function
#' @examples
#'
#' data(blood)
#' fortify(as.edgedf(blood$edges), blood$vertices)
#' fortify(as.edgedf(blood$edges), blood$vertices, group = "Ethnicity")
#' @export
fortify.edgedf <- function(model, data, group = NULL, ...){
  edge.data <- model
  if (is.null(data)){
    stop("Error: Must provide the node data to the data argument.")
  }
  node.data <- data
  message(paste("Joining edge and node information by", 
                names(edge.data)[1], "and", 
                names(node.data)[1], "respectively."))
  if(!is.null(group)){
    nodes <- unique(node.data[,1])
    if(group %in% names(edge.data)){
      groups <- unique(edge.data[,group])
    } else groups <- unique(node.data[,group])
    allnodes <- expand.grid(nodes, groups, stringsAsFactors = F)
    names(allnodes) <- c("ID", group)
    names(node.data)[1] <- "ID"
    names(edge.data)[1] <- "from"
    if(group %in% names(edge.data)){
      node.data.expanded <- merge(allnodes, node.data, by = "ID")
      dat <- merge(edge.data, node.data.expanded, by.x = c("from", group), by.y = c("ID", group), all = T)
    } 
    else {
      node.data.expanded <- merge(allnodes, node.data, by = c("ID",group))
      dat <- merge(edge.data, node.data.expanded, by.x = "from", by.y = "ID", all = T)
    }
  } else {
   dat <- merge(edge.data, node.data, by.x = names(edge.data)[1], by.y = names(node.data)[1], all = T)
  }
   return(dat)
}
#' Function for converting a network adjacency matrix into the correct format for use with geomnet
#'
#' @param model An adjacency matrix of class \code{"adjmat"}.
#' @param data not used in this function
#' @param ...  not used in this function
#' @export
#' @importFrom tidyr gather
#' @importFrom readr parse_number
#' @examples
#' data(emon, package = "network")
#' adjmat <- as.adjmat(network::as.matrix.network.adjacency(emon$MtSi))
#' str(adjmat)
#' fortify(adjmat)
#' @export
fortify.adjmat <- function(model, data = NULL,  ...){
  net <- model
  if (dim(net)[1] != dim(net)[2]){
    stop("Error: Please supply a square adjacency matrix.")
  }
  if (!is.null(rownames(net))){
    ID <- rownames(net)
  } else if (!is.null(colnames(net))){
    ID <- colnames(net)
  } else ID <- 1:ncol(net)
  net <- as.data.frame(net, stringsAsFactors = F)
  net$from <- ID
#  introduce visible binding for global variables
  to <- NULL
  from <- NULL
  value <- NULL

  net <- tidyr::gather(net, to, value, -from)

  #net <- dplyr::filter(net, value > 0)
  net <- subset(net, value > 0)
  #net <- dplyr::mutate(net, edge.weight = value)
  net$edge.weight = net$value
#  edge.data <- dplyr::select(net, from, to, edge.weight)
  edge.data <- net[, c("from", "to", "edge.weight")]

#  net %>%
#    tidyr::gather(to, value, -from) %>%
#    dplyr::filter(value > 0) %>%
#    dplyr::mutate(edge.weight = value) %>%
#    dplyr::select(from, to, edge.weight) -> edge.data
  froms <- unique(edge.data$from)
  tos <- unique(edge.data$to)
  if (class(froms) != class(tos)){
    if (class(froms) %in% c("numeric", "integer")){
      tos <- readr::parse_number(tos)
    } else if (class(froms) == "factor" && class(tos) == "character"){
      froms <- as.character(froms)
    } else if (class(tos) == "factor" && class(froms) == "character"){
      tos <- as.character(tos)
    } else {stop("Error: Cannot match from and to columns. Please provide an\nadjacency matrix with row or column names.")}
  }
  allnodes <- sort( unique(
    c(unique(froms), unique(tos))
  ) )
  node.data <- data.frame(id = allnodes, stringsAsFactors = F)
  dat <- merge(edge.data, node.data, by.x = 'from', by.y = 'id', all = T)
  return(dat)
}
