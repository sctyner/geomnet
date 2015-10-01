StatDensityCommon <- ggplot2::ggproto("StatDensityCommon", ggplot2::Stat,
  required_aes = "x",

  setup_params = function(data, params) {
    browser()
    if (!is.null(params$bandwidth))
      return(params)

    xs <- split(data$x, data$group)
    bws <- vapply(xs, bw.nrd0, numeric(1))
    bw <- mean(bws)
    message("Picking bandwidth of ", signif(bw, 3))

    list(bandwidth = bw)
  },

  compute_group = function(data, scales, bandwidth = 1) {
    d <- density(data$x, bw = bandwidth)
    data.frame(x = d$x, y = d$y)
  }
)

#' @export
stat_density_common <- function(mapping = NULL, data = NULL, geom = "line",
                                position = "identity", show.legend = NA,
                                inherit.aes = TRUE, bandwidth = NULL,
                                ...) {
  layer(
    stat = StatDensityCommon, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(bandwidth = bandwidth, ...)
  )
}


#' @export
StatLm <- ggplot2::ggproto("StatLm", ggplot2::Stat,
                  required_aes = c("x", "y"),

                  compute_group = function(data, scales, params, n = 100, formix = y ~ x) {
                    rng <- range(data$x, na.rm = TRUE)
                    grid <- data.frame(x = seq(rng[1], rng[2], length = n))

                    mod <- lm(formix, data = data)
                    grid$y <- predict(mod, newdata = grid)

                    grid
                  }
)

#' @export
stat_lm <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", show.legend = NA,
                    inherit.aes = TRUE, n = 50, formix = y ~ x,
                    ...) {
  layer(
    stat = StatLm, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, formix = formix, ...)
  )
}
