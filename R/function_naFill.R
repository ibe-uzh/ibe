#' Fill NAs with replacement values
#'
#' Avoid NAs by filling them with given values. The length of the original vector is retained.
#' `na.fill.t` and `na.fill.f` are wrappers around `na.fill` to fill NAs in logical vectors with `TRUE` or `FALSE`, respectively.
#' 
#' If the length of `x` is not a multiple of `y`s length only the first value in `y` is used, potentially with a warning. 
#' `na.fill.t` and `na.fill.f` coerce `x` to logical, potentially with a warning.
#'
#' @param x A vector that may include `NA`s
#' @param y A vector of values used to replace `NA`s in `x` with
#' @param warn Whether to give warnings about irregularities
#'
#' @return Vector `x` with `NA`s replaced by values from `y`.
#' 
#' @examples
#' # usage to replace NAs
#' bla <- c(1:5, NA_real_, 7:10)
#' na.fill(bla, 100)
#' na.fill(bla, 101:110)
#' 
#' # usage in indexing
#' i <- c(T, T, F, F, T, NA_logical_, F, F, T, T)
#' try(LETTERS[i]) # results in an error, as NAs are not allowed for indices
#' LETTERS[na.omit(i)]
#' LETTERS[na.fill.t(i)]
#' LETTERS[na.fill.f(i)]
#'
#' @name na.fill
NULL

#' @rdname na.fill
#' @export
na.fill <- function(x, y, warn=TRUE) {
  if (!is.vector(x) | !is.vector(y)) stop("x and y must be vectors.")
  i <- is.na(x)
  if (all(!i)) return(x)
  if (length(x)%%length(y) != 0) {
    if (warn) warning("length(x) is not a multiple of length(y), only the first value in y will be used.")
    y <- y[1]
  }
  y2 <- rep(y, length(x)/length(y) )
  if (any(is.na(y2[i])) & warn) warning("Some values in y used to replace NAs in x are themselves NA.")
  x[i] <- y2[i]
  x
}

#' @rdname na.fill
#' @export
na.fill.t <- function(x, warn=FALSE) {
  if (!is.logical(x) & warn) warning("x is not logical but will be coerced to logical.") 
  na.fill(x=as.logical(x), y=TRUE, warn=warn)
}

#' @rdname na.fill
#' @export
na.fill.f <- function(x, warn=FALSE) {
  if (!is.logical(x) & warn) warning("x is not logical but will be coerced to logical.")
  na.fill(x=as.logical(x), y=FALSE, warn=warn)
}
