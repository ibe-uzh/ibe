#' Test if ID_Daten connection is available via id-srv or IP-address
#'
#' @param verbose Whether to show a message of the result.
#'
#' @return A character vector of length 1: The path to ID_Daten if available, otherwise NA_character_.
#' 
#' @details Checks for ID_Daten via id-srv and the IP-adress. If both are available, id-srv is preferred.
#' 
#' @export
id_srv_path <- function(verbose = FALSE) {
  y <- dir.exists("//id-srv/ID_Daten")
  z <- dir.exists("//192.168.1.101/ID_Daten")
  if (verbose) {
    if (y) message("ID_Daten available as: //id-srv/ID_Daten")
    if (z) message("ID_Daten available as: //192.168.1.101/ID_Daten")
    if (!y & !z) message("ID_Daten not available!")
  }
  if (y) return("//id-srv/ID_Daten")
  if (z) return("//192.168.1.101/ID_Daten")
  return(NA_character_)
}

