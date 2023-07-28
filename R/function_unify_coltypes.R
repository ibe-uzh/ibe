#' Unify the types of variables across a list of data frames
#'
#' Coerces columns that appear in several data frames to the same type.
#' This is useful to do before stacking the data frames with `dplyr::bind_rows()`.
#' The order of priority of types is as follows: character > double > integer > logical
#' For instance, if any of the same-named columns is of type "character", then all others will be coerced to character.
#' Columns that appear in only one of the data frames or already have the same type across all data frames remain unchanged.
#'
#' @param df.list A list of data frames
#' @param verbose Logical, whether to give a message for every column that is changed.
#'
#' @return The same list of data frames but colums that appear in several of them are coerced to the same type. 
#' The order of columns may still differ between data frames. The list can be submitted to `dplyr::bind_rows()`.
#' @export
#'

unify_coltypes <- function(df.list, verbose = TRUE) {
  if (!is.list(df.list)) stop("df.list must be a list of data frames!")
  if (!all(sapply(df.list), is.data.frame)) stop("df.list must be a list of data frames!")
  for (col in unique(unlist(lapply(df.list, colnames))) ) {
    if (sum(sapply(df.list,
                   function(df, x) x %in% colnames(df),
                   x = col)) > 1) {
      types <- sapply(df.list,
                      function(df, x) typeof(df[[x]]),
                      x = col)
      if (length(unique(types[types!="NULL"])) > 1) {
        if (any(types == "character")) {
          for (i in 1:length(types)) df.list[[i]][[col]] <- as.character(df.list[[i]][[col]])
          if (verbose) message("column '",col,"' coerced to character")
        } else if (any(types == "double")) {
          for (i in 1:length(types)) df.list[[i]][[col]] <- as.double(df.list[[i]][[col]])
          if (verbose) message("column '",col,"' coerced to double")
        } else if (any(types == "integer")) {
          for (i in 1:length(types)) df.list[[i]][[col]] <- as.integer(df.list[[i]][[col]])
          if (verbose) message("column '",col,"' coerced to integer")
        }
      }
    }
  }
  return(df.list)
}