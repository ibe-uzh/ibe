#' Mixed design cluster sampling with 3 strata: Small clusters (simple/systematic random sample), Medium clusters (PPS) and Large clusters (full census).
#'
#' @param df Stratified data frame with clusters and (estimated) elements per cluster
#' @param enrVar Character string with name of column containing student enrollment number
#' @param tcs Numeric target cluster size
#' @param n Total number of elements to sample
#'
#' @return Input data frame is completed with columns containing sampling information. Selected schools are flagged by sc_smp_selected.
#' @import dplyr
#' @import stringr
#'
#' @export
sample_PPS2 <- function(df, enrVar, tcs, n) {
  
  frame <- df %>%
    rename(sc_npop_pre = all_of(enrVar)) %>%
    mutate(sc_npop_pre = as.numeric(sc_npop_pre))
  
  # get sampling fraction
  p <- n/sum(frame$sc_npop_pre)
  
  # allocate school to one of three strata
  frame <- mutate(frame,
                  stratum = case_when(
                    # small schools where a simple random sample with p of schools is sampled
                    sc_npop_pre < tcs ~ 1,
                    # medium schools where a sort of PPS is done (selection probability = p*n/tcs) with tcs students sampled
                    sc_npop_pre >= tcs & sc_npop_pre < tcs/p ~ 2,
                    # large schools where all schools are selected and p students sampled
                    sc_npop_pre >= tcs/p ~ 3
                  )
  )
  
  # STRATUM 1 (SRS)
  
  sFrame <- filter(frame, stratum == 1) %>%
    mutate(smp_ln = row_number())
  
  # number of schools to sample
  nSmall <- round(p * nrow(sFrame), 0)
  
  # define sampling interval
  si <- nrow(sFrame)/nSmall
  
  # define random start between 0 and si
  rs <- runif(1, 0, si)
  
  # define selected row numbers
  sampVec <- NULL
  for(i in 0:(nSmall-1)) {
    sampVec <- c(sampVec, ceiling(rs+i*si))
  }
  
  # copy vector for possible replacement schools
  repVec <- sampVec
  # loop through vector and check whether replacements available
  for(i in 1:length(repVec)) {
    # if next school not in sampled vector and not outside of it
    if(!(repVec[i] + 1) %in% sampVec & (repVec[i] + 1) <= nrow(sFrame)) {
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
  
  sFrame <- mutate(sFrame,
                  sc_smp_selected = case_when(
                    smp_ln %in% sampVec ~ 1,
                    smp_ln %in% repVec ~ 2,
                    TRUE ~ 0)
  )
  
  # Create ids and assign replacement schools to sampled schools (id_rep_sc, id_rep_sc_for)
  sFrame$id_school <- NA
  sFrame$id_rep_sc <- NA
  sFrame$id_rep_sc_for <- NA
  for(i in 1:length(sampVec)) {
    # id for sampled schools
    sFrame[sFrame$smp_ln == sampVec[i], "id_school"] <- str_pad(i, 3, "left", pad = "0")
    # if there's an replacement school
    if(!is.na(repVec[i])) {
      # id for replacement school
      sFrame[sFrame$smp_ln == repVec[i], "id_school"] <- as.character(i + 300)
      # assing replacement to sampled school
      sFrame[sFrame$smp_ln == sampVec[i], "id_rep_sc"] <- as.character(i + 300)
      # assing sampled to replacement school
      sFrame[sFrame$smp_ln == repVec[i], "id_rep_sc_for"] <- str_pad(i, 3, "left", pad = "0")
    }
  }
  
  # add samp vars to frame
  sFrame <- mutate(sFrame,
                  smp_rs = rs,
                  smp_si = si,
                  sc_smp_scselprob = round(nSmall/nrow(sFrame), 4),
                  sc_smp_scweight = round(1/sc_smp_scselprob, 4)
  )
  
  # STRATUM 2 PPS
  mFrame <- filter(frame, stratum == 2) %>%
    mutate(
      smp_ln = row_number(),
      sc_smp_scselprob = round(p * sc_npop_pre / tcs, 4),
      sc_smp_mos = sc_npop_pre,
      sc_smp_cum_mos = cumsum(sc_smp_mos)
      )
  
  # number of schools to sample
  nMedium <- round(sum(mFrame$sc_smp_scselprob), 0)
  
  # define sampling interval
  si <- sum(mFrame$sc_smp_mos)/nMedium
  
  # define random start
  rs <- runif(1, 0, si)
  
  # define selected list numbers
  sln <- NULL
  for(i in 0:(nMedium-1)) {
    sln <- c(sln, ceiling(rs+i*si))
  }
  
  # vector with line numbers of sampled schools
  sampVec <- NULL
  for(i in 1:length(sln)) {
    ln <- which(mFrame$sc_smp_cum_mos > sln[i])[1]
    sampVec <- c(sampVec, ln)
  }
  
  # copy vector for possible replacement schools
  repVec <- sampVec
  # loop through vector and check whether replacements available
  for(i in 1:length(repVec)) {
    # if next school not in sampled vector and not outside of it
    if(!(repVec[i] + 1) %in% sampVec & (repVec[i] + 1) <= nrow(mFrame)) {
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
  
  mFrame <- mutate(mFrame,
                   sc_smp_selected = case_when(
                     smp_ln %in% sampVec ~ 1,
                     smp_ln %in% repVec ~ 2,
                     TRUE ~ 0)
  )
  
  # Create ids and assign replacement schools to sampled schools (id_rep_sc, id_rep_sc_for)
  mFrame$id_school <- NA
  mFrame$id_rep_sc <- NA
  mFrame$id_rep_sc_for <- NA
  
  # id start after ids for sampled schools in small stratum
  plusId <- max(as.numeric(str_sub(sFrame$id_school, 2, 3)), na.rm = TRUE)
  
  for(i in 1:length(sampVec)) {
    # id for sampled schools
    mFrame[mFrame$smp_ln == sampVec[i], "id_school"] <- str_pad(i + plusId, 3, "left", pad = "0")
    # if there's an replacement school
    if(!is.na(repVec[i])) {
      # id for replacement school
      mFrame[mFrame$smp_ln == repVec[i], "id_school"] <- as.character(i + plusId + 300)
      # assing replacement to sampled school
      mFrame[mFrame$smp_ln == sampVec[i], "id_rep_sc"] <- as.character(i + plusId + 300)
      # assing sampled to replacement school
      mFrame[mFrame$smp_ln == repVec[i], "id_rep_sc_for"] <- str_pad(i + plusId, 3, "left", pad = "0")
    }
  }
  
  # add samp vars to frame
  mFrame <- mutate(mFrame,
                   smp_rs = rs,
                   smp_si = si,
                   sc_smp_scweight = round(1/sc_smp_scselprob, 4)
  )
  
  # STRATUM 3 - All schools in sample
  
  # id start after ids for sampled schools in small stratum
  plusId <- max(as.numeric(str_sub(mFrame$id_school, 2, 3)), na.rm = TRUE)
  
  # add ids
  lFrame <- filter(frame, stratum == 3)
  lFrame$id_school <- str_pad((1 + plusId):(nrow(lFrame) + plusId), 3, "left", pad = "0")
  lFrame <- mutate(lFrame,
                   sc_smp_selected = 1,
                   sc_smp_scselprob = 1,
                   sc_smp_scweight = 1
    )
  
  # rbind the 3 strata again
  frame <- bind_rows(sFrame, mFrame, lFrame) %>%
    mutate(nStudents = case_when(
      stratum == 1 ~ sc_npop_pre,
      stratum == 2 ~ tcs,
      stratum == 3 ~ ceiling(p*sc_npop_pre)
    )) %>%
    # mutate(sc_npop_pre2 = sc_npop_pre) %>%
    rename(!!enrVar := sc_npop_pre)
    # rename(sc_npop_pre = sc_npop_pre2) 


  
}
