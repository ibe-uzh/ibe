#' @rdname domain_name2mx
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