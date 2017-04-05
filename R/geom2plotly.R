#' @importFrom utils getFromNamespace
#' @importFrom plotly to_basic
#' @export
to_basic.GeomNet <- function(data, prestats_data, layout, params, p, ...) {
  #browser()
  if (params$directed){
    message("Note: the plotly package does not yet support arrows for segments.\nSee https://github.com/ropensci/plotly/issues/469 for updates")
  }
  # get x,y and xend,yend coordinates, among other things
  dat2 <- ggplot2::ggplot_build(p)$data[[1]]
  data <- getFromNamespace("left_join", asNamespace("dplyr"))(data, dat2)
  node_names <- names(data)[!(names(data) %in% c("to","xend", "yend", ".selfie", "weight", ".samegroup", "ymax", "xmax", "width", "linetype", "fontsize", "arrowsize", "stroke"))]
  # edge_names <- c("from","to", "x", "y","xend", "yend", ".selfie", "weight", ".samegroup", "ymax", "xmax", "width", "linetype", "linewidth", "fontsize", "arrowsize", "stroke", "PANEL")
  node_data <- unique(data[, node_names])
  node_data <- getFromNamespace("prefix_class", asNamespace("plotly"))(node_data, "GeomPoint")
  htext <- apply(t(apply(do.call("rbind",strsplit(node_data$hovertext, "<br>")), 1, unique)), 1, paste, collapse = "<br>")
  node_data$hovertext <- paste0("Node ID:", node_data$from,
                                 "<br>", htext)
  # edge_data <- unique(data[, edge_names])
  edge_data <- data
  edge_data <- edge_data[which(!edge_data$.selfie),]
  edge_data$size <- edge_data$linewidth
  edge_data$hovertext <- NULL
  edge_labels <- edge_data
  if (is.null(params$ealpha)){
    edge_data$alpha <- 1
  } else edge_data$alpha <- params$ealpha
  edge_data$colour <- ifelse(is.null(params$ecolour), "grey40", params$ecolour)
  edge_data <- getFromNamespace("to_basic.GeomSegment", asNamespace("plotly"))(edge_data)
  edge_data <- getFromNamespace("prefix_class", asNamespace("plotly"))(edge_data, "GeomNet")
  edge_labels$x <- (edge_labels$x + edge_labels$xend)/2
  edge_labels$y <- (edge_labels$y + edge_labels$yend)/2
  edge_labels$colour <- "white"
  edge_labels$shape <- ""
  edge_labels$size <- 0
  if (params$directed){
    edge_labels$hovertext <- paste("from", edge_labels$from, "to", edge_labels$to, sep = " ")
  } else edge_labels$hovertext <- paste("(", edge_labels$from, ",", edge_labels$to, ")", sep = "")
  edge_labels <- getFromNamespace("prefix_class", asNamespace("plotly"))(edge_labels, "GeomPoint")
  data <- list(edge_labels, edge_data, node_data)
  data
}