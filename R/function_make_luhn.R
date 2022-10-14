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
