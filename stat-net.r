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
stat_net <- function (mapping = NULL, data = NULL, geom = "net", position = "identity",
na.rm = FALSE, ...) {
  StatNet$new(mapping = mapping, data = data, geom = geom,
  position = position, na.rm = na.rm, layout = NULL, ...)
}

StatNet <- proto(Stat, {
  objname <- "net"

  default_geom <- function(.) GeomNet
  default_aes <- function(.) aes(colour = "#3366FF", size = 0.5)
  #required_aes <- c("x", "y")
   required_aes <- c("layout")

#calculate function: extracts smaller data frame from original data using just req params. then computes some other stuff.
  calculate <- function(., data, scales, na.rm = FALSE,  layout, ...) {  #add requirements here later.
  	#compute x and y using layout method if x,y are not there. then can assume that x and y are there from now on. 
    df <- data.frame(data)
   
   browser()
   #at the end of this routine, we need to have an x and a y.
   df  
  }
})
