#' Find linear transformation via quantiles
#'
#' Find the contant and multiplier to linearly transform *X* into *Y* based on quantiles of their respective distributions.
#'   Basically a wrapper around `lm()` where the quantiles of *Y* are regressed onto *Y*.
#'   For both variables, provide either raw values or given quantiles.
#'
#' @param x Raw values of distribution *X*
#' @param y Raw values of distribution *Y*
#' @param Qx Quantiles of distribution *X* (provide *either* `x` or `Qx`).
#' @param Qy Quantiles of distribution *X* (provide *either* `x` or `Qx`).
#' @param probs Probabilities of quantiles to be computed. Only needed if `Qx` or `Qy` (or both) not provided.
#'   Must be a numeric vector of length >= 2 and values between 0 and 1. Defaults to `c(.05, .25, .50, .75, .95)`.
#'
#' @return A named vector with the elements `constant` and `multiplier`.
#' @export

find_trans_via_quantiles <- function(x=NULL, y=NULL, Qx=NULL, Qy=NULL, probs = c(.05, .25, .50, .75, .95) )
{
  if ( sum(c( is.null(x), is.null(Qx) )) != 1 ) stop("ERROR: Please provide x OR Qx, but not both.")
  if ( sum(c( is.null(y), is.null(Qy) )) != 1 ) stop("ERROR: Please provide y OR Qy, but not both.")
  if ( any(is.null(Qx), is.null(Qy)) & length(probs)<=2 ) stop("ERROR: If raw values of x or y are provided, quantiles must have at least length 2.")
  if ( any(is.null(Qx), is.null(Qy)) & (any(probs<=0) | any(probs>=1)) ) stop("ERROR: Quantiles must be real numbers between 0 and 1.")
  if (is.null(Qx)) Qx <- quantile(x, probs, na.rm = FALSE)
  if (is.null(Qy)) Qy <- quantile(y, probs, na.rm = FALSE)
  trans_lm <- lm(Qy ~ Qx)
  setNames(coef(trans_lm), c("constant", "multiplier"))
}