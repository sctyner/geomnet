#' test function
#' 
#' this is what the function does
#' @export
#' @param x numeric vector
#' @param ... addiional parameters passed on to min
#' @return minimum of the vector x
minx <-  function(x, ...) return(min(x, ...))

#' @export
#' @example
#' data(mtcars)
#' library(ggplot2)
#' ggplot(data=mtcars) + geom_net(aes(x=mpg, y=disp)) 
geom_net <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
na.rm = FALSE, ...) {
  GeomNet$new(mapping = mapping, data = data, stat = stat, position = position, 
  na.rm = na.rm, ...)
}


#' @export
GeomNet <- proto::proto(ggplot2:::Geom, {
  objname <- "net"

  draw_groups <- function(., ...) .$draw(...)
  draw <- function(., data, scales, coordinates, na.rm = FALSE, ...) {    
    data <- remove_missing(data, na.rm, 
      c("x", "y", "size", "shape"), name = "geom_net")
    if (empty(data)) return(zeroGrob())
    
    with(coord_transform(coordinates, data, scales), 
      ggname(.$my_name(), pointsGrob(x, y, size=unit(size, "mm"), pch=shape, 
      gp=gpar(col=alpha(colour, alpha), fill = alpha(fill, alpha), fontsize = size * .pt)))
    )
  }

  # draw_legend <- function(., data, ...) {
    # data <- aesdefaults(data, .$default_aes(), list(...))
    
    # with(data,
      # pointsGrob(0.5, 0.5, size=unit(size, "mm"), pch=shape, 
      # gp=gpar(
        # col=alpha(colour, alpha), 
        # fill=alpha(fill, alpha), 
        # fontsize = size * .pt)
      # )
    # )
  # }

  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(shape=16, colour="black", size=10, fill = NA, alpha = NA) 
})
