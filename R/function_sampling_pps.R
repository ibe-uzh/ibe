library(dplyr)
library(stringr)

#' Select PPS-Sample of clusters
#'
#' @param df Stratified data frame with clusters and corresponding MOS (measures of size) 
#' @param mos Column name containing MOS (measure of size)
#' @param tcs Target cluster size
#' @param n Total number of elements (in clusters) to sample
#'
#' @return Input data frame is completed with columns containing sampling information. Selected schools are flagged by sc_smp_selected.
#' @import dplyr
#' @import stringr
#' @export
#'
#' @examples
samplePPS <- function(df, mos, tcs, n) {
  
  frame <- df %>%
    rename(sc_smp_mos = all_of(mos)) %>%
    mutate(
      sc_smp_cum_mos = cumsum(sc_smp_mos),
      tempId = row_number()
      )
  
  # number of clusters to sample in stratum
  nClust <- n/tcs
  
  # define sampling interval
  si <- sum(frame$sc_smp_mos)/nClust
  
  # remove certainty clusters from frame as long as there are MOS > SI
  certainties <- data.frame(NULL)
  while(sum(frame$sc_smp_mos > si) > 0) {
    
    # add certainty clusters to certainties df
    certainties <- bind_rows(certainties, filter(frame, sc_smp_mos >= si))
    # remove certainty clusters from frame
    frame <- filter(frame, sc_smp_mos < si)
    # redefine number of elements to sample
    n <- n-tcs*nrow(certainties)
    # redefine number of clusters to sample
    nClust <- n/tcs
    # redefine sampling interval
    si <- sum(frame$sc_smp_mos)/nClust
    
  }
  
  frame <- mutate(frame, smp_ln = row_number(),)
  
  # define random number between 0 and 1
  rn <- runif(1, 0, 1)
  
  # define random start
  rs <- rn*si
  
  # define selected list numbers
  sln <- NULL
  for(i in 0:(nClust-1)) {
    sln <- c(sln, ceiling(rs+i*si))
  }
  
  # vector with line numbers of sampled schools
  sampVec <- NULL
  for(i in 1:length(sln)) {
    ln <- sum(frame$sc_smp_cum_mos <= sln[i])
    sampVec <- c(sampVec, ln)
  }
  
  # copy vector for replacement schools
  repVec <- sampVec
  # loop through vector and check whether replacements available
  for(i in 1:length(repVec)) {
    # if next school not in sampled vector and not outside of it
    if(!(repVec[i] + 1) %in% sampVec & (repVec[i] + 1) <= max(frame$smp_ln)) {
      repVec[i] <- (repVec[i] + 1) 
    } else {
      # else check if last school not in sampVec or repVec and not below 1
      if(!(repVec[i] - 1) %in% sampVec & !(repVec[i] - 1) %in% repVec & (repVec[i] - 1) >= 1) {
        repVec[i] <- (repVec[i] - 1)
      } else {
        repVec[i] <- NA
      }
    }
  }
  
  # flag sampled and replacement schools (sc_smp_selected)
  certainties <- mutate(certainties, sc_smp_selected = 1)
  frame <- mutate(frame,
                  sc_smp_selected = case_when(
                    smp_ln %in% sampVec ~ 1,
                    smp_ln %in% repVec ~ 2,
                    TRUE ~ 0)
                  )

  # Create ids and assign replacement schools to sampled schools (id_rep_sc, id_rep_sc_for)
  frame$id_school <- NA
  frame$id_rep_sc <- NA
  frame$id_rep_sc_for <- NA
  for(i in 1:length(sampVec)) {
    # id for sampled schools
    frame[frame$smp_ln == sampVec[i], "id_school"] <- str_pad(i, 3, "left", pad = "0")
    # if there's an replacement school
    if(!is.na(repVec[i])) {
      # id for replacement school
      frame[frame$smp_ln == repVec[i], "id_school"] <- i + 300
      # assing replacement to sampled school
      frame[frame$smp_ln == sampVec[i], "id_rep_sc"] <- i + 300
      # assing sampled to replacement school
      frame[frame$smp_ln == repVec[i], "id_rep_sc_for"] <- str_pad(i, 3, "left", pad = "0")
     }
  }
  
  # add samp vars to frame
  frame <- mutate(frame,
                  smp_rs = rs,
                  smp_si = si,
                  sc_smp_scselprob = round(sc_smp_mos/max(frame$sc_smp_cum_mos)*nClust, 4),
                  sc_smp_scweight = round(1/sc_smp_scselprob, 4)
                  )

  # ids for certainties  
  certainties$id_school <- str_pad((length(sampVec)+1):(length(sampVec) + nrow(certainties)), 3, "left", pad = "0")
  
  # add samp vars to certainties
  certainties <- mutate(certainties,
                        sc_smp_scselprob = 1,
                        sc_smp_scweight = 1)
  
  # rbind frame and certainties again, add sampling variables and do inititial sort
  df2 <- bind_rows(frame, certainties) %>%
    arrange(tempId) %>%
    select(-tempId)
  
  return(df2)
  
}