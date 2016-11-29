#' @importFrom utils getFromNamespace
#' @importFrom plotly to_basic
#' @export
to_basic.GeomNet <- function(data, prestats_data, layout, params, p, ...) {
  # get x,y and xend,yend coordinates, among other things
  dat2 <- ggplot2::ggplot_build(p)$data[[1]]
  data <- getFromNamespace("left_join", asNamespace("dplyr"))(data, dat2)
  node_names <- names(data)[!(names(data) %in% c("to","xend", "yend", ".selfie", "weight", ".samegroup", "ymax", "xmax", "width", "linetype", "fontsize", "arrowsize", "stroke"))]
  edge_names <- c("from","to", "x", "y","xend", "yend", ".selfie", "weight", ".samegroup", "ymax", "xmax", "width", "linetype", "linewidth", "fontsize", "arrowsize", "stroke")
  node_data <- unique(data[, node_names])
  node_data <- getFromNamespace("prefix_class", asNamespace("plotly"))(node_data, "GeomPoint")
  if (is.null(node_data$label)){
    node_data$hovertext <- node_data$from
  } 
  edge_data <- unique(data[, edge_names])
  edge_data <- edge_data[which(!edge_data$.selfie),]
  edge_data$size <- edge_data$linewidth
  if (params$directed){
    edge_data$hovertext <- paste("from", edge_data$from, "to", edge_data$to, sep = " ")
  } else edge_data$hovertext <- paste("(", edge_data$from, ",", edge_data$to, ")", sep = "")
  if (is.null(params$ealpha)){
    edge_data$alpha <- 1
  } else edge_data$alpha <- params$ealpha
  edge_data$colour <- ifelse(is.null(params$ecolour), "grey40", params$ecolour)
  edge_data <- getFromNamespace("to_basic.GeomSegment", asNamespace("plotly"))(edge_data)
  edge_data <- getFromNamespace("prefix_class", asNamespace("plotly"))(edge_data, "GeomNet")
  data <- list(edge_data, node_data)
  data
}