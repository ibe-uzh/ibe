#' Translate between domain names and matrix codes
#' 
#' `domain_name2mx` translates domain names (e.g. "dles", "natw") to short matrix codes according to the Lehrplan 21 (e.g. "D.2", "NT").
#' `domain_mx2name` translates in the opposite direction. Domain name "natw" translates to "NT" and "math" to "MA".
#' 
#' @param x A character vector of domain names or codes.
#' @param stop.when.unknown Logical, whether to stop with an error when one or several values of x are invalid. If `FALSE` (default), warnings are given for unknown domains or competence areas.
#'
#' @return A character vector of the same length as x, potentially including new `NA`s.
#'
#' @name domain_name_matrix
NULL

#' @rdname domain_name_matrix
#' @export 
domain_name2mx <- function(x, stop.when.unknown = FALSE)
  {
  if (!is.character(x)) stop("x must be a character vector")
  x <- tolower(x)
  subj <- substring(x, 1, 1)
  if (! all(subj %in% c("d", "e", "f", "r", "m", "n"), na.rm=TRUE) ) {
    if (stop.when.unknown) stop("unknown subject detected") else message("WARNING: Some subjects are unknown, will result in NAs!")
    subj[which(! subj %in% c("d", "e", "f", "r", "m", "n"))] <- NA_character_
  }
  if (any(nchar(x) != 4)) {
    message("WARNING: Not all domain names have regular length (4). This may result in NAs.")
    x <- substring(x, 1, 4)
  }
  kber <- substring(x, 2, 4)
  if (! all(kber %in% c("hoe", "les", "spr", "sch", "sif", "fur", "gfd", "zuv", "ath", "atw"), na.rm=TRUE) ) {
    if (stop.when.unknown) stop("unknown competence area detected") else message("WARNING: Some competence areas are unknown, will result in NAs!")
    kber[which(! kber %in% c("hoe", "les", "spr", "sch", "sif", "fur", "gfd", "zuv", "ath", "atw"))] <- NA_character_
  }
  subj[which(subj=="m")] <- "ma"
  subj[which(subj=="n")] <- "nt"
  mx1 <- toupper(subj)
  mx2 <- sapply(kber, function(kb) switch(kb,
                                          hoe = "1", les = "2", spr = "3", sch = "4", sif = "5",
                                          zuv = "1", fur = "2", gfd = "3", ath = "_",
                                          atw = "_"))
  y <- paste(mx1, mx2, sep=".")
  y[which(subj %in% c("d", "e", "f", "r") & ! kber %in% c("hoe", "les", "spr", "sch", "sif"))] <- NA_character_
  y[which(subj=="ma" & ! kber %in% c("zuv", "fur", "gfd", "ath"))] <- NA_character_
  y[which(subj=="nt" & ! kber=="atw")] <- NA_character_
  if (any(mx2=="_")) y <- sub("\\._$", "", y)
  return(y)
}

#' @rdname domain_name_matrix
#' @export 
domain_mx2name <- function(x, stop.when.unknown = FALSE)
{
  x <- strsplit(toupper(x),
                split = ".", fixed = TRUE)
  mx1 <- sapply(x,
                function(x) x[1])
  if (! all(mx1 %in% c("D", "E", "F", "R", "MA", "NT"))) {
    if (stop.when.unknown) stop("unknown subject detected") else message("WARNING: Some subjects are unknown, will result in NAs!")
    mx1[which(! mx1 %in% c("D", "E", "F", "R", "MA", "NT"))] <- NA_character_
  }
  mx2 <- sapply(x,
                function(x) as.integer(x[2]) )
  mx2[which(mx1 == "MA" & mx2==0)] <- 9L
  mx2[which(mx1 == "MA" & is.na(mx2))] <- 0L
  if (! all(mx2[which(!mx1 %in% c("MA", "NT"))] %in% 1:5, na.rm=TRUE) | ! all(mx2[which(mx1=="MA")] %in% 0:3, na.rm=TRUE) ) {
    if (stop.when.unknown) stop("unknown competence area detected") else message("WARNING: Some competence areas are unknown, will result in NAs!")
    mx2[which(mx1 %in% c("D", "E", "F", "R") & ! mx2 %in% 1:5)] <- NA_integer_
    mx2[which(mx1 == "MA" & ! mx2 %in% 0:3)] <- NA_integer_
  }
  subj <- tolower(mx1)
  subj[which(subj=="ma")] <- "m"
  subj[which(subj=="nt")] <- "natw"
  lang <- subj %in% c("d", "e", "f", "r")
  lang[is.na(lang)] <- FALSE
  kber <- vector("character", length(x))
  kber[lang] <- unlist(sapply(mx2[lang],
                              function(mx) {
                                if(is.na(mx)) return("")
                                switch(mx,
                                       "hoe", "les", "spr", "sch", "sif")
                              } ))
  kber[!lang] <- unlist(sapply(mx2[!lang],
                               function(mx) {
                                 if(is.na(mx)) return("")
                                 if(mx==0L) return("ath")
                                 switch(mx,
                                        "zuv", "fur", "gfd", "ath")
                               } ))
  y <- paste0(subj, kber)
  y[which(subj %in% c("d", "e", "f", "r") & ! kber %in% c("hoe", "les", "spr", "sch", "sif"))] <- NA_character_
  y[which(subj=="m" & ! kber %in% c("zuv", "fur", "gfd", "ath"))] <- NA_character_
  y[which(subj=="natw" & kber != "")] <- NA_character_
  return(y)
}