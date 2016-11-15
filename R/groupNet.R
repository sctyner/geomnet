#' A function for creating a data set appropriate for facetted plotting, so that all nodes appear in all facets, whether or not they are active in that facet.
#' 
#' @param dat data frame. A fortified network data frame. See fortify methods for geomnet. 
#' @param group character. Name of the variable that you wish to facet by 
#' @param from_id character. Name of the variable that contains the from nodes. 
#' 
#' 
#' @export
groupNet <- function(dat, group, from_id){
  nodes <- unique(dat[, from_id])
  groups <- unique(dat[,var])
  allnodes <- expand.grid(nodes, groups)
  names(allnodes) <- c(from_id, group)
  dat2 <- merge(dat, allnodes, by.x = c(from_id, group), by.y = c(from_id, group))
  return(dat2)
}