#' Make Luhn checksums as needed for HIDs (including scan-IDs)
#'
#' @param x A number or character string of numbers. All characters and special characters are ignored when calculating the check sum but preserved in the output.
#' @param HID Set `TRUE` for HID-formatted output.
#' @param sep Separator between the original input `x` and the checksum that is appended.
#'
#' @return A character string pasted from the original input `x`, the separator `sep`, and the checksum.
#' @export
#'
#' @examples

make_luhn <- function(x, HID=TRUE, sep=ifelse(HID, "-0", "")) {
  y <- strsplit(gsub("[[:punct:]]|[[:alpha:]]", "", as.character(x) ),
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
  paste0(x,sep,cs)
} 
