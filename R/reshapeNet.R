#' Function for converting various network structures into the correct format for use with geomnet
#' 
#' @param net. A network object. Can be a class "network", "igraph", "matrix" (for adjaceny matrices), or "data.frame" object. If net is a "data.frame", must provide the ndata argument and the first column of net must be the "from" node column.
#' @param ndata. Data frame containing network node list and other node information. Only applicable for class "data.frame." First column name should be node ids. 
#' 
#' @examples 
#' 
#' # class network (sna, network, statnet packages)
#' 
#' library(network)
#' data(emon)
#' reshapeNet(net = emon$Cheyenne)
#' 
#' # class igraph (igraph, igraphdata packages)
#' library(igraph)
#' library(igraphdata)
#' data("USairports", package = "igraphdata")
#' head(reshapeNet(net = USairports))
#' 
#' # class matrix (for adjacency matrices)
#' 
#' adjmat <- network::as.matrix.network.adjacency(emon$MtSi)
#' str(adjmat)
#' reshapeNet(net = adjmat)
#' 
#' # eclass data.frame and ndata 
#' data(blood)
#' reshapeNet(net = blood$edges, ndata = blood$vertices)
#' 
#' @export
reshapeNet <- function(net, ...){
  UseMethod("reshapeNet", net)
}
#' @export
reshapeNet.network <- function(net, ...){
  require(network)
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
  
  dat <- merge(edge.data, node.data, by.x = "from", by.y = "ID", all = T)
  
  return(dat)
}
reshapeNet.igraph <- function(net, ...){
  require(igraph)
  node.data <- igraph::as_data_frame(net, what = "vertices")
  names(node.data)[1] <- "ID"
  edge.data <- igraph::as_data_frame(net, what = "edges")
  dat <- merge(edge.data, node.data, by.x = "from", by.y = "ID", all = T )
  return(dat)
}

reshapeNet.data.frame <- function(net, ndata, ...){
  if (is.null(ndata)){
    stop("Error: Must provide the node data to the ndata argument.")
  }
  dat <- merge(net, ndata, by.x = names(net)[1], by.y = names(ndata)[1], all = T)
  return(dat)
}

reshapeNet.matrix <- function(net, ...){
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
    filter(value > 0) %>% 
    mutate(edge.weight = value) %>% 
    select(from, to, edge.weight) -> edge.data
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
  dat <- merge(edge.data, node.data, by.x = 'from', by.y = 'id')
  return(dat)
}
