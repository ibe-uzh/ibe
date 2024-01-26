#' Makes evenly distributed group assignments for a given number of persons and days. Used for planning double-corrections in writing tests.
#'
#' @param npers number of persons (raters)
#' @param ntage number of days
#' @param maxzeit maximum time (seconds) to use for iterative process
#' @param seed initial seed to use. The search loop for valid combinations increments the seed by 1 after each iteration.
#'
#' @return a data frame with `npers` rows, a column `Gruppe` for group assignment (A and B), and `ntage` columns for days
#' 
#' @export
tageskombi <- function(npers, ntage, maxzeit=10, seed=123, raters=NULL) {
  grpsize <- floor(npers/2) # wie viele Personen in Gruppe A?
  kombis <- combn(1:npers, grpsize) # alle möglichen Kombinationen für Gruppe A
  minchose <- floor((grpsize*ntage)/npers) # jede Person sollte mindestens wie oft in Gruppe A sein?
  maxchose <- minchose+1 # jede Person sollte max. wie oft in Gruppe A sein?
  krit_test <- rep(FALSE, npers)
  starttime <- Sys.time()
  i <- 0
  while(any(!krit_test)) {
    set.seed(seed + i)
    gezogen <- kombis[, sample(1:ncol(kombis), ntage)]
    krit_test <- table(gezogen) %in% minchose:maxchose
    if(Sys.time()-starttime > maxzeit) stop("Keine Lösung innert ",maxzeit," Sekunden gefunden.")
    i <- i+1
  }
  cat("\nErgebnis nach", i+1, "Versuchen. Seed =", seed+i, "\n")
  cat("\nHäufigkeit Gruppe A:\n")
  print(table(gezogen))
  cat("\n")
  A <- data.frame(Gruppe = rep("A", grpsize),
                  as.data.frame(gezogen))
  B <- data.frame(Gruppe = rep("B", npers-grpsize),
                  sapply(1:ntage, function(tg) which(!(1:npers %in% A[,1+tg])) ))
  
  names(A)[-1] <- paste("Tag", 1:ntage)
  names(B)[-1] <- paste("Tag", 1:ntage)
  out <- rbind(A, B)
  
  duos <- combn(1:npers, 2) |>
    as.data.frame() |>
    as.list()
  duos_freq <- vector("integer")
  for (d in 1:length(duos)) {
    duos_freq[d] <- apply(A[,-1], 2,
                          function(x) as.numeric((duos[[d]][1] %in% x) == (duos[[d]][2] %in% x)) ) |>
      sum()
  }
  cat("Häufigkeiten Duos zusammen:\n")
  print(table(duos_freq))
  
  if (!is.null(raters)) {
    if (length(na.omit(raters)) != npers) stop("Length of raters vector must be npers.")
    raters <- as.character(na.omit(raters))
    for (col in setdiff(colnames(out), "Gruppe")) out[[col]] <- sapply(out[[col]], function(x, rs) rs[x], rs = raters)
  }
  out
}