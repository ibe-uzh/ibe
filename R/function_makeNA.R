#' Make NAs of given data type or matching given vector
#'
#' Generate a vector of NA values of a specific data type. Either provide an example vector or specify the type.
#' This is useful for avoiding errors in picky functions (e.g. `dplyr::if_else`).
#' Remember that "numeric" is not a type (use "real" or "integer" instead), it is turned into "real" instad, with a warning.
#'
#' @param x An atomic (!) vector of which the data type and length is used. Omit this if you provide `type`.
#' @param type A data type to use. Omit this if you provide `x`.
#' @param length The length of the output, defaults to 1 if `x` is not given.
#'
#' @return An atomic vector of NA values matching type and length of `x` or of type `type` and length `length`.
#'
#' @examples
#' makeNA(1:10)
#' makeNA(1L:10L)
#' makeNA(type = "character", length = 5)
#' 
#' df <- data.frame(a = letters,
#'                  x = 1:26,
#'                  y = rep(c(T, F), 13))
#' # results in an error as NA is of wrong type
#' for (i in 1:2) dplyr::if_else(df$y, df[[i]], NA)
#' # no error as NA value is adapted to type
#' for (i in 1:2) dplyr::if_else(df$y, df[[i]], makeNA(df[[i]]))
#'
#' @export
makeNA <- function(x = NULL, type = NULL, length = 1) {
  if (is.null(x) & is.null(type)) stop("Either x or type must be provided!")
  if (!is.null(x) & !is.null(type)) stop("Only one of x and type must be provided!")
  if (is.null(x)) type <- match.arg(type,
                                    c("character", "string", "real", "double", "numeric", "complex", "integer", "logical"),
                                    several.ok = FALSE)
  if (is.null(type)) {
    if (!is.atomic(x)) stop("x must be atomic")
    type <- typeof(x)
    length <- length(x)
  }
  if (type == "numeric") warning("'numeric' is not a data type, assuming 'real' (i.e. 'double') instead.")
  rep(switch(type,
             character = NA_character_,
             string = NA_character_,
             real = NA_real_,
             double = NA_real_,
             numeric = NA_real_,
             complex = NA_complex_,
             integer = NA_integer_,
             logical = NA),
      length)
}
