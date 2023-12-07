#' Encrypt PDF-files with QPDF
#' 
#' This is wrapper around the qpdf.exe shell command for Windows.
#' It will not work on other systems and requires that the QPDF program is available in a writable directory on the system.
#' QPDF can be downloaded from https://sourceforge.net/projects/qpdf/files/qpdf/10.3.2/ .
#' 
#' The function checks the availablility of the input file and output file. A success or error message is given upon exit with
#'
#' @param path The path to directory containing the input PDF document.
#' @param input Name of the input PDF document as a string.
#' @param output Name of the output PDF document as a string.
#' @param pw Passwort as a string. Beware of escape characters!
#' @param qpdf.path The path to the qpdf.exe file. If NULL and the user is "bwolf", the location where BW has his exe. Other user's paths locations may be added in the future.
#' @param overwrite Logical. Should the output file be overwritten if it already exists? Results in an error if `FALSE` and the file exists.
#'
#' @return Returns -- invisibly -- a 0 if QPDF ran successfully or else the error code (usually 127).
#'
#' @examples
#' QPDF("C:/Users/bwolf/Documents/", input = "mySecretDoc.pdf", output = "mySecretDoc_encrypted.pdf", pw = "123456")
#' 
#' @export
QPDF <- function(path, input, output, pw, qpdf.path = NULL, overwrite = FALSE) {
  
  if (is.null(qpdf.path) & Sys.info()["user"]=="bwolf") qpdf.path <- "C:/Users/bwolf/Documents/Verschlüsseln_QPDF/qpdf-10.3.2/bin"
  if (is.null(qpdf.path)) stop("No path to QPDF binary found for user ",Sys.info()["user"])
  if (!any("qpdf.exe" %in% list.files(qpdf.path))) stop("qpdf.exe not found in path:\n",qpdf.path)
  
  ipath <- file.path(path, input)
  if (!file.exists(ipath)) stop("Input file not found:\n", ipath)
  opath <- file.path(path, output)
  if (file.exists(opath) & !overwrite) stop("Output file already exists:\n",
                                            opath,
                                            "\nSet `overwrite = TRUE` if it should be replaced.")
  orig.wd <- getwd()
  bash <- paste("qpdf --encrypt", pw, pw, "256 --", gsub("/", "\\\\", ipath), gsub("/", "\\\\", opath))
  
  setwd(qpdf.path)
  answer <- system(bash)
  setwd(orig.wd)
  
  if (answer==0) {
    message("Success! Password-secured QPDF generated:\n",
            opath)
  } else {
    message("ERROR: QPDF did not run correctly. Please Check bash command:\n",
            bash)
  }
  
  invisible(answer)
  
}