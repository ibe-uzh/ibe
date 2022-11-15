#' Porportional and disproportional allocations of sample sizes to explicit strata
#'
#' @param df Data frame containing primary sampling units (schools) with stratum and size (enrollment) variables
#' @param n Numeric representing the total number of elements to sample (students)
#' @param stratVar Character string containing name of stratification variable
#' @param enrVar Character string containing name of cluster/unit size variable (enrollment number)
#' @param varVec Numeric vector containing estimated variance in strata
#'
#' @return List with 3 vectors: proportional allocations, neyman allocations and the corresponding ratios of both methods
#' @import dplyr
#' @export
#'
#' @examples
strata_allocation <- function(df, n, stratVar, enrVar, varVec) {
  
  # get df and rename variables
  frame <- df %>%
    rename(sc_npop_pre = all_of(enrVar), 
           stratum = all_of(stratVar))
  
  # available strata
  strata <- sort(unique(frame$stratum))
  
  # check number of strata and varVec length
  if(length(strata) != length(varVec)) stop("Number of strata does not correspond to number of variances entered")
  
  # population size
  N <- sum(frame$sc_npop_pre)
  
  # sampling rate
  sr <- n/N
  
  # proportional allocation
  propAlloc <- NULL
  for(i in 1:length(strata)) {
    x <- round(filter(frame, stratum == strata[i]) %>% pull(sc_npop_pre) %>% sum() * sr, 0)
    propAlloc <- c(propAlloc, x)
  }
  
  # disproportional allocation (neyman formula)
  denonimator <- NULL
  for(i in 1:length(strata)) {
    x <- round(filter(frame, stratum == strata[i]) %>% pull(sc_npop_pre) %>% sum() * sr, 0) * sqrt(varVec[i])
    denonimator <- sum(denonimator, x)
  }
  
  dispAlloc <- NULL
  for(i in 1:length(strata)) {
    x <- n * (round(filter(frame, stratum == strata[i]) %>% pull(sc_npop_pre) %>% sum() * sr, 0) * sqrt(varVec[i]))
    x <- round(x/denonimator, 0)
    dispAlloc <- c(dispAlloc, x)
  }
  
  # create list output (proportional allocations, disproportional allocations and corresponding ratios)
  ratioAlloc <- dispAlloc/propAlloc
  
  alloc <- list(propAlloc, dispAlloc, ratioAlloc)
  names(alloc) <- c("proportional allocation", "neyman allocation", "ratio of allocation methods")
  
  return(alloc)
  
}
