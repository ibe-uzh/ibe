#' Plot Item Characteristic Curve(s)
#'
#' @param diff Difficulty Parameter
#' @param disc Discrimination Parameter
#' @param xlim Theta display range
#'
#' @return ggplot object
#' @import dplyr
#' @import scales
#' @import ggplot2
#' @import purrr
#' @export
#'
#' @examples
plot_ICC <- function(diff, disc, xlim = -5:5) {
  
  if(length(diff) != length(disc)) stop("parameter vectors do not have identical length!")
  
  pars <- data.frame(
    diff = diff,
    disc = disc
  )
  
  gg_colors <- scales::hue_pal()(nrow(pars))
  
  plot <- ggplot(data.frame(x = xlim)) +
    aes(x) +
    map(1:nrow(pars),
        ~ stat_function(fun = function (x) (exp(pars$disc[.x]*(x - pars$diff[.x])))/(1 + exp(pars$disc[.x]*(x - pars$diff[.x]))), color = gg_colors[.x])) +
    map(1:nrow(pars),
        ~ geom_segment(aes(x = pars$diff[.x], y = 0.5, xend = pars$diff[.x], yend = 0), linetype = "dashed", color = gg_colors[.x], size = 0.1)
    ) +
    scale_y_continuous(name = 'P', limits = c(0,1)) +
    scale_x_continuous(name = 'theta', limits = c(-5,5))
  
  return(plot)
  
}




