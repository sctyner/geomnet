#' Number of JTTs in a graph
#'
#' Number of jumping transitive triplets (JTT) in a graph. A JTT between three nodes i,j,
#' and k is defined as the situation that when there is a (directed) edge from i to j and an
#' edge from j to k there is also a direct edge from i to k.
#' In an undirected situation we can think of any undirected edge as two directed edges between the two nodes involved.
#' @param data data set
#' @param from_id name of the variable of edge tails
#' @param to_id name of the variable of edge heads
#' @importFrom network as.network.matrix
#' @importFrom network as.matrix.network
#' @export
#' @examples
#' data(blood)
#' ggplot(data= blood$edges) + geom_net(aes(from_id=from, to_id=to), directed=TRUE) + theme_net()
#' jtt(blood$edges, "from", "to")
#' # this number is very high compared to the overall number of edges that are not self-loops
#' nrow(subset(blood$edges, from != to))
jtt <- function(data, from_id, to_id) {
  data <- data[,c(from_id, to_id)]
  X <- network::as.matrix.network(network::as.network.matrix(data))
  diag(X) <- 0
  X2 <- X %*% X
  #  browser()
  diag(X2) <- 0
  path2 <- sum(X2[(X2 > 0) & (X > 0)])
  path2
}
