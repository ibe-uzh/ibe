#' Identify the packages and functions loaded, needed or not needed in a script
#'
#' \code{package_usage} Reads a scriptfile and determines which packages are
#'   being loaded when executing the script and which functions are used. The
#'   function does \em{not} execute the script but it does load (and instantly
#'   unload) all packages loaded via \code{library()} or \code{require()} in the
#'   script. Functions and packages accessed via \code{::} or \code{:::} are also
#'   identified.
#'
#' @param scriptfile is a character string giving the path to the script file.
#' @param extensive Logical, if \code{TRUE}, not only the packages loaded and used
#'   but also the functions used from these packages are listed. Default is
#'   \code{FALSE}.
#'
#' @return A list is returned invisibly. If \code{extensive=FALSE},
#'   the list contains the names of packages loaded, packages used, packages
#'   loaded but not used, packages accessed via \code{::} or \code{:::}. If
#'   \code{extensive=TRUE}, packages used is itself a nested list function names
#'   grouped by packages. Generic functions with separate methods in different
#'   packages (e.g. \code{plot()}) result in groups that are named after several
#'   packages (e.g. \code{"c(base, TAM)"}). Functions called via  \code{::} or
#'   \code{:::} and functions defined within the script are in a group
#'   "character(0)".
#'
#'
#' @import NCmisc
#' @import stringr
#' @name package_usage
<<<<<<< HEAD
#' @export
=======

>>>>>>> b9ceb510df65911a9731e3520ffff89afaaa1b3c
package_usage <- function(scriptfile, extensive=FALSE) {
  
  script <- readLines(con = scriptfile)
  
  # Find which packages are being loaded in that scirpt ----
  load.packages <- script |>
    sub(pattern="\"|'", replacement="") |>
    stringr::str_extract_all("(?<=library\\(|require\\()[[:alnum:]]+(?=\\))") |>
    unlist()
  
  # Find functions and packages used via :: or ::: in that scirpt ----
  access.functions <- script |>
    stringr::str_extract_all("[[:alnum:]]+:{2,3}[[:alnum:]]+(?=\\()") |>
    unlist() |>
    unique() |>
    (function(x) {
      packs <- unique(sub("::[[:alnum:]]+", "", x=x))
      funlist <- lapply(packs,
                        function(p, x) {
                          unique(sub(paste0(p,"::"), "", grep(p, x, value=TRUE)))
                        },
                        x=x)
      setNames(funlist, packs)
      })()
  
  # Find packages which used functions belong to ----
  lapply(load.packages,
         function(x) require(x, quietly=TRUE, warn.conflicts=FALSE, attach.required=FALSE, character.only=TRUE) )
  used.functions <- NCmisc::list.functions.in.file(filename = scriptfile, alphabetic = FALSE)
  # lapply(load.packages,
  #        function(x) unloadNamespace(paste0("package:",x)) )
  
  # Find loaded packages that are used or not ----
  used.packages <- used.functions |>
    names() |>
    grep(pattern = "package:", value = TRUE) |>
    gsub(pattern = "package:", replacement = "")
  
  # print output
  cat("\nPackages loaded via `library()` or `requrie()`:\n",
      paste0(load.packages, collapse=", "),
      "\n\nPackages loaded but not used:\n",
      paste0(setdiff(load.packages, used.packages), collapse=", "),
      "\n\nPackages accessed via `::` or `:::`:\n",
      paste0(names(access.functions), collapse=", "),
      "\n\n")
  
  # create output list
  if (extensive) {
    list(
      packages.loaded = load.packages,
      functions.used = setNames(used.functions, gsub(pattern = "package:", replacement = "", names(used.functions))),
      packages.notused = setdiff(load.packages, used.packages),
      functions.accessed = access.functions
    )
  } else {
    list(
      packages.loaded = load.packages,
      packages.used = used.packages,
      packages.notused = setdiff(load.packages, used.packages),
      packages.accessed = names(access.functions)
    ) |>
      invisible()
  }
}
