#' A function for creating a data set appropriate for facetted plotting, so that all nodes appear in all facets, whether or not they are active in that facet.
#' 
#' @param dat data frame. A fortified network data frame. See fortify methods for geomnet. 
#' @param group character. Name of the variable that you wish to facet by 
#' @param from_id character. Name of the variable that contains the from nodes. 
#' 
#' @examples
#' library(geomnet)
#' data(email)
#' emailnet <- fortify(subset(email$edges, nrecipients < 54), email$nodes)
#' ggplot(data = emailnet, aes(from_id = From, to_id = to)) +
#'  geom_net(layout.alg = "fruchtermanreingold",
#'           aes(colour = CurrentEmploymentType,
#'               group = CurrentEmploymentType,
#'               linewidth = 3 * (...samegroup.. / 8 + .125)),
#'           ealpha = 0.25,
#'           size = 4, curvature = 0.05,
#'           directed = TRUE, arrowsize = 0.5) +
#'  scale_colour_brewer("Employment Type", palette = "Set1") +
#'  theme_net() +
#'  theme(legend.position = "bottom")
#'  
#'  emailnetfacet <- groupNet(dat = emailnet, group = "day", from_id = "From")
#'   
#' @export
groupNet <- function(dat, group, from_id){
  nodes <- unique(dat[,from_id])
  groups <- unique(dat[,group])
  allnodes <- expand.grid(nodes, groups, stringsAsFactors = F)
  names(allnodes) <- c(from_id, group)
  mergevars <- names(dat)[names(dat) == names(allnodes)]
  dat2 <- dplyr::left_join(dat, allnodes)
  return(dat2)
}