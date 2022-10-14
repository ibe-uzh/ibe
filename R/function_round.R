#' Conventional rounding
#'
#' @param x A vector of real numbers
#' @param digits Decimal places to preserve
#'
#' @return A vector of numbers rounded to the indicated decimal places
#' @export
#'
#' @examples

Round <- function(x, digits=0) {
  posneg <- sign(x)
  z <- abs(x)*10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10^digits
  z*posneg
}
