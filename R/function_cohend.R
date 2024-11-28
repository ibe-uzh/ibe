#' Compute pooled standard deviation and/or Cohen's d effect size
#'
#' @param x A numerical vector or (for `sd_pool`) an SD value.
#' @param y A numerical vector or (for `sd_pool`) an SD value.
#' @param n1 If x is an SD, value, provide the size of sample x as a numerical vector of length 1.
#' @param n2 If x is an SD, value, provide the size of sample y as a numerical vector of length 1.
#'
#' @return Both functions return a umerical vector of length 1 giving the pooled SD or Cohen's d effect size.
#' 
#' @export
cohend <- function(x, y) {
  s <- sd_pool(x, y)
  m <- mean(x, na.rm=TRUE) - mean(y, na.rm=TRUE)
  m/s
}

#' @name cohend
#' @export
sd_pool <- function(x, y, n1=NA, n2=NA) {
  if (all(length(x)==length(n1), length(y)==length(n2), !is.na(n1), !is.na(n2))) {
    sd1 <- x
    sd2 <- y
  }
  if (all(is.na(n1), is.na(n2), sum(!is.na(x))>2, sum(!is.na(y))>2)) {
    sd1 <- sd(x, na.rm=TRUE)
    sd2 <- sd(y, na.rm=TRUE)
    n1 <- sum(!is.na(x))
    n2 <- sum(!is.na(y))
  }
  sd_pool <- sqrt( ((n1 - 1)*sd1^2 + (n2 - 1)*sd2^2)/(n1 + n2 - 2) )
  return(sd_pool)
}