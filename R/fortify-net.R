#' Function for converting a network object into the correct format for use with geomnet
#' @param model. object of class "network"
#' @param data NULL
#' @param group. character. Used for facetting. If you wish to facet on an edge variable provide the name of that variable here.
#' @examples 
#' # class network (sna, network, statnet packages)
#' 
#' library(network)
#' data(emon)
#' fortify(emon$Cheyenne)
#' 
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
#' @param model. A an "igraph" object
#' @param data. NULL 
#' @param group. character. Used for facetting. If you wish to facet on an edge variable provide the name of that variable here.
#' @examples 
#' # class igraph (igraph, igraphdata packages)
#' library(igraph)
#' library(igraphdata)
#' data("USairports", package = "igraphdata")
#' head(fortify(USairports))
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
#' @param model. A network edgelist of class "data.frame" object. The first column should contain the "from" node column.
#' @param data. Data frame containing network node list and other node information. First column should contain node ids. 
#' @param group. character. Used for facetting. If you wish to facet on an edge variable provide the name of that variable here.
#' @examples 
#' # class data.frame and ndata 
#' data(blood)
#' fortify(blood$edges, blood$vertices)
#' @export
fortify.data.frame <- function(model, data, group = NULL, ...){
  edge.data <- model
  if (is.null(data)){
    stop("Error: Must provide the node data to the data argument.")
  }
  node.data <- data
  if(!is.null(group)){
    nodes <- unique(node.data[,1])
    groups <- unique(edge.data[,group])
    allnodes <- expand.grid(nodes, groups, stringsAsFactors = F)
    names(allnodes) <- c("ID", group)
    names(node.data)[1] <- "ID"
    names(edge.data)[1] <- "from"
    node.data.expanded <- merge(allnodes, node.data, by = "ID")
    dat <- merge(edge.data, node.data.expanded, by.x = c("from", group), by.y = c("ID", group), all = T)
  } else {
   dat <- merge(edge.data, node.data, by.x = names(edge.data)[1], by.y = names(node.data)[1], all = T)
  }
   return(dat)
}
#' Function for converting a network adjacency matrix into the correct format for use with geomnet
#' @param model. An adjacency matrix of class "matrix".
#' @param data. NULL 
#' @param group. character. Used for facetting. If you wish to facet on an edge variable provide the name of that variable here.
#' # class matrix (for adjacency matrices)
#' 
#' adjmat <- network::as.matrix.network.adjacency(emon$MtSi)
#' str(adjmat)
#' fortify(adjmat)
#' @export
fortify.matrix <- function(model, data = NULL, ...){
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
  net %>%
    tidyr::gather(to, value, -from) %>%
    dplyr::filter(value > 0) %>% 
    dplyr::mutate(edge.weight = value) %>% 
    dplyr::select(from, to, edge.weight) -> edge.data
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
