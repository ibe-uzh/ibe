#' Check for duplicates from both ends
#' 
#' The base function `duplicated(x)` returns a logical vector where every duplicated element of `x` is `TRUE`. In order to get the first element, too, we have to use `duplicated(x) | duplicated(x, fromLast=TRUE)`. `duplicated2()` is a wrapper around `duplicated()` that makes this easyier.
#' 
#' `duplicated()` has several methods for different types of `x`. These should work identically for `duplicated2()` because it does nothing but pass the arguments.
#'
#' @param x a vector or a data frame or an array or `NULL`
#' @param incomparables a vector of values that cannot be compared. `FALSE` is a special value, meaning that all values can be compared, and may be the only value accepted for methods other than the default. It will be coerced internally to the same type as `x`.
#' @param ... arguments for particular methods
#'
#' @return For a vector input, a logical vector of the same length as x. For a data frame, a logical vector with one element for each row. For a matrix or array, and when MARGIN = 0, a logical array with the same dimensions and dimnames.
#' 
#' @seealso [duplicated()] which this function wraps.
#' 
#' @export
duplicated2 <- function(x, incomparables = FALSE, ...) {
  duplicated(x, incomparables = incomparables, fromLast = FALSE, ... = ...) |
    duplicated(x, incomparables = incomparables, fromLast = TRUE, ... = ...)
}