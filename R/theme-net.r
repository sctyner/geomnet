#' theme_net
#' Themes set the general aspect of the plot such as the colour of the
#' background, gridlines, the size and colour of fonts.
#'
#' @param base_size base font size
#' @param base_family base font family
#'
#' @details \describe{
#'
#' \item{\code{theme_net}}{
#' The usual ggplot2 theme with no background, no axes, and an aspect ratio of 1 for better
#' viewing of networks and graphs.}
#' }
#' @examples
#' library(ggplot2)
#' data(blood)
#' bloodnet <- merge(blood$edges, blood$vertices, by.x="from", by.y="label", all=TRUE)
#' p <- ggplot(data = bloodnet, aes(from_id = from, to_id = to))
#' p + geom_net()
#' p + geom_net() + theme_net()
#'
#' @name ggtheme
NULL
#' @export
#' @rdname ggtheme
theme_net <- function (base_size = 11, base_family = "")
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.background = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
}

theme_net_old <- function(base_size = 11, base_family = "") {
  half_line <- base_size / 2

  theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line = element_line(
      colour = "black", size = 0.5, linetype = 1,
      lineend = "butt"
    ),
    rect = element_rect(
      fill = "white", colour = "black",
      size = 0.5, linetype = 1
    ),
    text = element_text(
      family = base_family, face = "plain",
      colour = "black", size = base_size,
      lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
      margin = margin()
    ),

    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.margin = unit(0.2, "cm"),
    legend.key = element_rect(fill = NA, colour = "white"),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,

    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.margin = unit(half_line, "pt"),
    panel.margin.x = NULL,
    panel.margin.y = NULL,
    panel.ontop = FALSE,

    strip.background = element_rect(fill = "grey85", colour = NA),
    strip.text = element_text(colour = "grey10", size = rel(0.8)),
    strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),

    plot.background = element_blank(),
    plot.title = element_text(
      size = rel(1.2),
      margin = margin(b = half_line * 1.2)
    ),
    plot.margin = margin(half_line, half_line, half_line, half_line),
    aspect.ratio = 1,
    complete = TRUE
  )
}

