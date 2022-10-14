#' Perform serpentine sorts on multiple variables.
#'
#' \code{serpentine} sorts data in a serpentine fashion (alternating between
#'   ascending and descending orders) for all variables specified.
#'   \code{mixed_serpentine} sorts the data with ascending or descending sorts
#'   for every variable specified except the last, which is serpentine sorted.
#'
#' This is helpful in complex sampling designs with implicit stratification, as
#'   it reduces the variation in the stratified outcome for adjacent sampled
#'   units and thus reduces the overall sampling error. Serpentine sorts are
#'   commonly used in NCES surveys.
#'
#' @param data is the data.frame to be sorted
#' @param ... are the variables to serpentine sort, in the given order. In
#'   \code{serpentine}, the first variable listed will be sorted in ascending
#'   order, the second variable will alternate between ascending and descending
#'   order by the value of the first variable, and so on. In
#'   \code{mixed_serpentine}, it is assumed all variables listed should be
#'   sorted in ascending order except the last, which is serpentine sorted. The
#'   user can choose a descending sort for any variable except the last by using
#'   the \code{desc()} wrapper.
#'
#' @return A data.frame with equal size as the original data, but sorted
#'   differently.
#'
#' @examples
#' # All variables except first are serpentine sorted
#' serpentine(data = mtcars, cyl, mpg)
#' serpentine(data = mtcars, cyl, vs, mpg)
#'
#' # cyl, and vs are ascending sorted while mpg is serpentine sorted
#' mixed_serpentine(mtcars, cyl, vs, mpg)
#'
#' # cyl is ascending, vs is descending, and mpg is serpentine sorted
#' mixed_serpentine(mtcars, cyl, desc(vs), mpg)
#'
#' @import rlang
#' @name serpentine
NULL


# HELPER FUNCTIONS -------------------------------------------------------------

# Single Seprentine Sort
# Function to take a data frame and two variables and return a serpentine
# sort on the second var with the a descending srot on the first var
single_serp <- function(data = NULL, var_desc = NULL, var_serp = NULL) {
  
  # Split data into lists by value of var_desc
  split_data <- split(data, dplyr::pull(data, !!var_desc))
  
  # Apply ascending sort to odd list elements
  split_data[c(T, F)] <-
    lapply(
      split_data[c(T, F)],
      function(x) {
        dplyr::arrange(x, !!var_serp)
      }
    )
  
  # Apply descending sort to even list elements
  split_data[c(F, T)] <-
    lapply(
      split_data[c(F, T)],
      function(x) {
        dplyr::arrange(x, desc(!!var_serp))
      }
    )
  
  # Recombine in data by appending list elements
  dplyr::bind_rows(split_data)
  
}


# EXPORTED FUNCTIONS -----------------------------------------------------------

#' @rdname serpentine
#' @export
serpentine <- function(data = NULL, ...) {
  
  # Extract key parts of provided function arguments
  orig_names <- names(data)
  full_vars <- rlang::quos(...)
  
  # Creating error if fewer than 2 variables specified
  if (length(full_vars) < 2) {
    stop("Need at least two variables to perform sort")
  }
  
  # Run serpentine sort on first two variables
  data <- single_serp(data = data, full_vars[[1]], full_vars[[2]])
  data <- dplyr::mutate(data, row_n = dplyr::row_number())
  data <- dplyr::group_by(data, !!!full_vars[1:2])
  data <- dplyr::mutate(data, new_group_num = min(row_n))
  data <- dplyr::ungroup(data)
  
  # Loop throuh next j values
  if (length(full_vars) >= 3) {
    for (j in 3:length(full_vars)) {
      data <- single_serp(data, rlang::quo(new_group_num), full_vars[[j]])
      data <- dplyr::mutate(data, row_n = dplyr::row_number())
      data <- dplyr::group_by(data, !!!full_vars[1:j])
      data <- dplyr::mutate(data, new_group_num = min(row_n))
      data <- dplyr::ungroup(data)
    }
  }
  
  # Clean data and export
  dplyr::select(data, dplyr::one_of(orig_names))
  
}

#' @rdname serpentine
#' @export
mixed_serpentine <- function(data = NULL, ...) {
  
  # Extract key parts of provided function arguments
  orig_names <- names(data)
  full_vars <- rlang::quos(...)
  ascdesc_vars <- full_vars[1:(length(full_vars) - 1)]
  serp_var <- full_vars[[length(full_vars)]]
  
  # Creating error if fewer than 2 variables specified
  if (length(full_vars) < 2) {
    stop("Need at least two variables to perform sort")
  }
  
  # Creating error if desc() used on last variable, which must be serpentined
  if (grepl("^desc\\(", rlang::quo_name(serp_var))) {
    stop(
      paste("Cannot apply a descending sort to last variable specified. It",
            "will automatically be serpentine sorted")
    )
  }
  
  # Perform sorts
  data <- dplyr::arrange(data, !!!ascdesc_vars)
  data <- dplyr::mutate(data, row_n = dplyr::row_number())
  data <- dplyr::group_by(data, !!!ascdesc_vars)
  data <- dplyr::mutate(data, min_row = min(row_n))
  data <- dplyr::ungroup(data)
  data <- serpentine(data, min_row, !!serp_var)
  
  # Clean data and export
  dplyr::select(data, dplyr::one_of(orig_names))
  
}