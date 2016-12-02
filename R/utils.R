#' Cast matrix to an adjacency matrix
#'
#' Create and assign a new class for use of \code{geomnet}'s \code{\link{fortify.adjmat}} function.
#' @param dat A square adjacency matrix.
#' @export
as.adjmat <- function(dat){
  if (class(dat) != "matrix"){
    stop("as.adjmat requires a matrix object")
  }
  if (nrow(dat) != ncol(dat)){
    stop("dat must be a square matrix")
  }
  if (sum(rownames(dat)!=colnames(dat)) > 0){
    stop("row and column names of dat must match")
  }
  class(dat) <- c("adjmat", "matrix")
  return(dat)
}

#' Cast a data frame to an edgedf
#'
#' Create and assign a new class for use of \code{geomnet}'s \code{\link{fortify.edgedf}} function.
#' @param dat A network edgelist of class \code{"data.frame"} for use with \code{\link{fortify.edgedf}}.
#' @export
as.edgedf <- function(dat){
  if (class(dat) != "data.frame"){
    stop("as.edgedf requires a data.frame object")
  }
  if (ncol(dat) < 2){
    stop("dat should have at least 2 columns (from node and to node)")
  }
  from_id <- names(dat)[1]
  to_id <- names(dat)[2]
  message(paste("Using", from_id, "as the from node column and",
              to_id, "as the to node column.
If this is not correct, rewrite dat so that the first 2 columns are from and to node, respectively."))
  names(dat)[1:2] <- c("from_id", "to_id")
  class(dat) <- c("edgedf", "data.frame")
  return(dat)
}
