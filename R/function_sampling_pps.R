#' Select PPS-Sample of clusters
#'
#' @param df Stratified data frame with clusters and (estimated) elements per cluster
#' @param enrVar Character string with name of column containing student enrollment number
#' @param tcs Numeric target cluster size
#' @param n Total number of clusters to sample
#' @param undersampling Logical: If TRUE, very small and tiny schools are undersampled by factor 2 and 4
#' @param idStart Numeric at which identificators of selected clusters start.
#'
#' @return Input data frame is completed with columns containing sampling information. Selected schools are flagged by sc_smp_selected.
#' @import dplyr
#' @import stringr
#' @export
#'
#' @examples
sample_PPS <- function(df, enrVar, tcs, n, undersampling = FALSE, idStart = 1) {
  
  frame <- df %>%
    rename(sc_smp_mos = all_of(enrVar)) %>%
    mutate(
      sc_smp_mos = case_when(
        !undersampling ~ ifelse(sc_smp_mos < tcs, tcs, sc_smp_mos),
        undersampling & sc_smp_mos >= tcs/2 ~ sc_smp_mos,
        undersampling & sc_smp_mos < tcs/2 & sc_smp_mos >= tcs/4 ~ tcs/2,
        undersampling & sc_smp_mos < tcs/4 ~ tcs/4
      ),
      sc_smp_cum_mos = cumsum(sc_smp_mos),
      tempId = row_number()
      )

  # number of clusters to sample in stratum
  nClust <- n
  
  # define sampling interval
  si <- sum(frame$sc_smp_mos)/nClust
  
  # remove certainty clusters from frame as long as there are MOS > SI
  certainties <- data.frame(NULL)
  while(sum(frame$sc_smp_mos > si) > 0) {
    
    # add certainty clusters to certainties df
    certainties <- bind_rows(certainties, filter(frame, sc_smp_mos >= si))
    # remove certainty clusters from frame and recalculate cummos
    frame <- filter(frame, sc_smp_mos < si) %>%
      mutate(sc_smp_cum_mos = cumsum(sc_smp_mos))
    # redefine number of clusters to sample
    nClust <- nClust - nrow(certainties)
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
    ln <- which(frame$sc_smp_cum_mos > sln[i])[1]
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
      # else check if previous school not in sampVec or repVec and not below 1
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
    frame[frame$smp_ln == sampVec[i], "id_school"] <- str_pad(idStart + i - 1, 3, "left", pad = "0")
    # if there's an replacement school
    if(!is.na(repVec[i])) {
      # id for replacement school
      frame[frame$smp_ln == repVec[i], "id_school"] <- as.character(idStart + i - 1 + 300)
      # assing replacement to sampled school
      frame[frame$smp_ln == sampVec[i], "id_rep_sc"] <- as.character(idStart + i - 1 + 300)
      # assing sampled to replacement school
      frame[frame$smp_ln == repVec[i], "id_rep_sc_for"] <- str_pad(idStart + i - 1, 3, "left", pad = "0")
     }
  }
  
  # add samp vars to frame
  frame <- mutate(frame,
                  smp_rs = rs,
                  smp_si = si,
                  sc_smp_scselprob = round(sc_smp_mos/max(frame$sc_smp_cum_mos)*nClust, 4),
                  sc_smp_scweight = round(1/sc_smp_scselprob, 4)
                  )
  if(nrow(certainties) > 0) {
    # ids for certainties
    start <- as.numeric(max(str_sub(frame$id_school, 2, 3), na.rm = TRUE)) + 1
    end <- start + nrow(certainties) - 1
    certainties$id_school <- str_pad(start:end, 3, "left", pad = "0")
    
    # add samp vars to certainties
    certainties <- mutate(certainties,
                          sc_smp_scselprob = 1,
                          sc_smp_scweight = 1)
  }
  
  # rbind frame and certainties again, add sampling variables and do inititial sort
  df2 <- bind_rows(frame, certainties) %>%
    arrange(tempId) %>%
    select(-tempId) %>%
    mutate(sc_smp_mos2 = sc_smp_mos) %>%
    rename(!!enrVar := sc_smp_mos) %>%
    rename(sc_smp_mos = sc_smp_mos2)
  
  return(df2)
  
}
