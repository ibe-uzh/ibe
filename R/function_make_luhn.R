#' Make or check Luhn checksums as needed for HIDs (including scan-IDs)
#'
#' @param x A number or character string of numbers.
#' @param HID Set `TRUE` for HID-formatted input/output.
#' @param sep Separator between the original input `x` and the checksum that is appended.
#' 
#' All characters, spaces, and punctuation marks are ignored when calculating or testing the check sum.
#' The HID input indicates whether there is a 0 at the position between the actual number and the check sum.
#' 
#' @return For `make_luhn`, a character string pasted from the original input `x`, the separator `sep`, and the checksum.
#' For `check_luhn`, a logical vector.
#' 
#' @examples
#' sapply(c(2, 6, 13, 17), function(s) make_luhn(substring("01-999-88-777-666", 1, s)))
#' 
#' @export
make_luhn <- function(x, HID = TRUE, sep = ifelse(HID, "-0", "")) {
  makel <- function(x1, sep) {
    if (is.na(x1)) return(NA_character_)
    y <- strsplit(gsub("[[:blank:]]|[[:punct:]]|[[:alpha:]]", "", as.character(x1) ),
                  split = "")
    y <- as.numeric(unlist(y))
    if (length(y) < 2) stop("ERROR: Cannot make checksum for numbers of less than 2 digits.")
    if (length(y) %% 2 == 0) {
      i1 <- (1:(length(y)/2))*2
    } else {
      i1 <- (1:ceiling(length(y)/2))*2-1
    }
    y[i1] <- y[i1]*2
    y <- ifelse(y > 9, y-9, y)
    cs <- (sum(y)*9) %% 10
    paste0(x1,sep,cs)
  }
  if (length(x) == 0) return(NA_character_)
  if (length(x) > 1) sapply(x, makel, sep = sep) else makel(x, sep = sep)
}

#' @name make_luhn
#' @export
check_luhn <- function (x, HID = TRUE) { # adapted from CRAN package "checkLuhn"
  checkl <- function(x1, hid) {
    y <- strsplit(gsub("[[:blank:]]|[[:punct:]]|[[:alpha:]]", "", as.character(x1) ),
                  split = "")
    y <- as.numeric(unlist(y))
    if (hid) y <- y[-(length(y)-1)] # remove 0 at second-to-last position if it is an HID
    if (length(y) < 2) stop("ERROR: Cannot test checksum for numbers of less than 2 digits.")
    y <- y[length(y):1]
    to_replace <- seq(2, length(y), 2)
    y[to_replace] <- as.numeric(y[to_replace]) * 2
    y <- as.numeric(y)
    y <- ifelse(y > 9, y - 9, y)
    ((sum(y)%%10) == 0)
  }
  if (length(x) == 0) return(NA)
  if (length(x) > 1) sapply(x, checkl, hid = HID) else checkl(x, hid = HID)
}

