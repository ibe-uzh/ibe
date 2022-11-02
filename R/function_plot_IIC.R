#' Plot Item Information Curve(s)
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
#' @import Deriv
#' @export
#'
#' @examples
plot_IIC <- function(diff, disc, xlim = -5:5) {
  
  if(length(diff) != length(disc)) stop("parameter vectors do not have identical length!")
  
  pars <- data.frame(
    diff = diff,
    disc = disc
  )
  
  # apply function to each row of dataframe with parameters
  icc <- function(disc, diff) {
    function(x) (exp(disc*(x - diff)))/(1 + exp(disc*(x - diff)))
  }
  icc_list <- pmap(pars, function(disc, diff) icc(disc, diff))
  
  # apply Deriv to each function in list
  info_list <- map(icc_list, Deriv::Deriv, "x")
  
  # calculate max
  xgetMax <- function(x) (seq(-5, 5, 0.01)[which(x(seq(-5, 5, 0.01)) == max(x(seq(-5, 5, 0.01))))])
  xmax_list <- lapply(info_list, xgetMax)
  xmax <- flatten_dbl(xmax_list)
  
  ymax_list <- map(1:nrow(pars), ~info_list[[.x]](xmax[.x]))
  ymax <- flatten_dbl(ymax_list)
  ymax <- round(ymax, 2)
  
  gg_colors <- scales::hue_pal()(nrow(pars))
  
  plot <- ggplot(data.frame(x = xlim)) +
    aes(x) +
    map2(info_list, gg_colors, ~ stat_function(fun = .x, color = .y)) +
    map(1:nrow(pars), ~ geom_segment(aes(x = xmax[.x], y = 0, xend = xmax[.x], yend = ymax[.x]), linetype = "dashed", color = gg_colors[.x], size = 0.1)) +
    scale_y_continuous(name = 'info') +
    scale_x_continuous(name = 'theta', limits = c(min(xlim), max(xlim)))
  
  return(plot)
  
}

