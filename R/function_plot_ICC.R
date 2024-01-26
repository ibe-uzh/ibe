#' Plot Item Characteristic Curve(s)
#'
#' @param b difficulty parameter
#' @param a discrimination (slope) parameter
#' @param xlim theta display range, defaults to -5 to 5
#'
#' @return ggplot object
#' 
#' @import dplyr
#' @import scales
#' @import ggplot2
#' @import purrr
#'
#' @examples
#' plot_ICC(rep(-1:1, 2), rep(1:2, each=3))
#' 
#' @export
plot_ICC <- function(b, a, xlim = -5:5) {
  
  require(ggplot2)
  
  if(length(b) != length(a)) stop("parameter vectors do not have identical length!")
  
  pars <- data.frame(
    b = b,
    a = a
  )
  
  gg_colors <- scales::hue_pal()(nrow(pars))
  
  ggplot(data.frame(x = xlim)) +
    aes(x) +
    map(1:nrow(pars),
        ~ stat_function(fun = function (x) (exp(pars$a[.x]*(x - pars$b[.x])))/(1 + exp(pars$a[.x]*(x - pars$b[.x]))), color = gg_colors[.x])) +
    map(1:nrow(pars),
        ~ geom_segment(aes(x = pars$b[.x], y = 0.5, xend = pars$b[.x], yend = 0), linetype = "dashed", color = gg_colors[.x], size = 0.1)
    ) +
    scale_y_continuous(name = 'P', limits = c(0,1)) +
    scale_x_continuous(name = 'theta', limits = c(min(xlim), max(xlim)))
  
}




