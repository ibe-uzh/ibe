#' Get linear transformation based on distribution parameters (M, SD) or quantiles
#' 
#' Get linear transformation coefficients (constant and multiplier) to transform from one distribution to the other.
#' `lintrans_distr` uses the mean and standard deviation. `lintrans_regr` uses the coefficients of a linear regression. The latter method requires that the data sets are paired.
#'
#' @param x Numeric vector of the original distribution.
#' @param y Numeric vector of the target distribution.
#' @param use_prop Numeric vector of value `0 < use_prop <= 1` Proportion of data used in each distribution vector. If < 1, equal percentages of data are removed from both ends of the distribution.
#' @param robust Whether to use a robust regression method instead of ordinary least squares regression. Requires the package `robustbase`.
#'
#' @return A list of two numeric vectors of length 1 each: constant and multiplier.
#'
#' @export
lintrans_distr <- function(x, y, use_prop = 1) {
  if (use_prop < 1 & use_prop > 0) {
    perc <- c((1-use_prop)/2, 1-(1-use_prop)/2)
    xquant <- quantile(x, perc)
    yquant <- quantile(y, perc)
    x <- x[x > xquant[1] & x < xquant[2]]
    y <- y[y > yquant[1] & y < yquant[2]]
  } else if (use_prop > 1 | use_prop <= 0) {
    stop("use_prop must be > 0 and <= 1")
  }
  Mx <- mean(x, na.rm=TRUE)
  SDx <- sd(x, na.rm=TRUE)
  My <- mean(y, na.rm=TRUE)
  SDy <- sd(y, na.rm=TRUE)
  multi <- SDy/SDx
  list(constant = My - Mx*multi,
       multiplier = multi)
}

#' @export
lintrans_regr <- function(x, y, robust=FALSE) {
  if (robust) {
    stopifnot(`Package robustbase not available!` = require(robustbase))
    coef_vec <- setNames(coef(robustbase::lmrob(y ~ x, method = "MM")), NULL)
  } else {
    coef_vec <- setNames(coef(lm(y ~ x)), NULL)
  }
  list(constant = coef_vec[1],
       multiplier = coef_vec[2])
}
