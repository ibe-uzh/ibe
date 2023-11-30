#' Conventional rounding
#' 
#' Rs `round` function rounds to the nearest even number, so that 1.5 becomes 2 but 0.5 becomes 0. In contrast, `Round` always rounds .5 upwards.
#'
#' @param x A vector of real numbers
#' @param digits Decimal places to preserve
#'
#' @return A vector of numbers rounded to the indicated decimal places
#'
#' @examples
#' a <- seq(-3, 3, by = .5)
#' round(a)
#' Round(a)
#' 
#' @export
Round <- function(x, digits=0) {
  posneg <- sign(x)
  z <- abs(x)*10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10^digits
  z*posneg
}
