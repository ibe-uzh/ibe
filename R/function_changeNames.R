#' Rename (some) in a object and return it
#' 
#' Just like `setNames` but you can provide indices to set/change only selected names.
#'
#' @param x Any object with elements that can be named (atomic vector, list, data.frame, ...).
#' @param i Vector of indices (integers or logical) of elements in x to rename.
#' @param nm 
#'
#' @return The same object as x but with names of elements i changed.
#' 
#' @export
changeNames <- function(x, i = 1:length(x), nm) {
  names(x)[i] <- nm
  x
}

#' @examples
#' bla <- 1:3
#' bla <- setNames(bla, c("eis", "zwoi", "drei"))
#' bla
#' changeNames(bla, 2:3, c("zwäi", "drüü"))
