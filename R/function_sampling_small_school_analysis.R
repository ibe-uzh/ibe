#' Small school analysis for PPS school samples
#'
#' @param df Data frame containing school frame of ONE explicit stratum
#' @param enrVar Character string with column name containing student enrollment numbers
#' @param tcs Numeric target cluster size
#' @param n Numeric amount of students to sample in stratum
#'
#' @import dplyr
#' @return Table with number of schools to sample and info on undersampling of very small schools
#' @export
#'
#' @examples
ssa <- function(df, enrVar, tcs, n) {

  frame <- df %>%
    rename(sc_npop_pre = all_of(enrVar)) %>%
    mutate(schoolSize = case_when(
      sc_npop_pre >= tcs ~ "normal",
      sc_npop_pre < tcs & sc_npop_pre >= tcs/2 ~ "small",
      sc_npop_pre < tcs/2 & sc_npop_pre >= tcs/4 ~ "very small",
      sc_npop_pre < tcs/4 ~ "tiny",
      TRUE ~ as.character(NA)
    ))

  schoolSampleSize <- round(n/tcs, 0)
  
  smallSchoolAnalysis <- frame %>%
    group_by(schoolSize) %>%
    summarise(nSchools = n(),
              nStudents = sum(sc_npop_pre),
              menr = mean(sc_npop_pre)) %>%
    ungroup() %>%
    mutate(sumSchools = sum(nSchools),
           sumStudents = sum(nStudents),
           pSchools = nSchools/sumSchools,
           pStudents = nStudents/sumStudents)
  
  undersample <- FALSE
  increase <- FALSE
  
  if(pull(smallSchoolAnalysis[smallSchoolAnalysis$schoolSize == "very small", "pStudents"]) >= 0.01) {
    undersample <- TRUE
    increase <- TRUE
  }
  
  if(pull(smallSchoolAnalysis[smallSchoolAnalysis$schoolSize == "very small", "pStudents"]) < 0.01 &
     pull(smallSchoolAnalysis[smallSchoolAnalysis$schoolSize == "tiny", "pSchools"]) >= 0.2 &
     pull(smallSchoolAnalysis[smallSchoolAnalysis$schoolSize == "small", "pStudents"]) >= 0.4) {
    undersample <- TRUE
    increase <- TRUE
  }
  
  if(pull(smallSchoolAnalysis[smallSchoolAnalysis$schoolSize == "very small", "pStudents"]) < 0.01 &
     pull(smallSchoolAnalysis[smallSchoolAnalysis$schoolSize == "tiny", "pSchools"]) < 0.2 &
     pull(smallSchoolAnalysis[smallSchoolAnalysis$schoolSize == "small", "pStudents"]) >= 0.4) {
    increase <- TRUE
  }
  
  if(pull(smallSchoolAnalysis[smallSchoolAnalysis$schoolSize == "very small", "pStudents"]) < 0.01 &
     pull(smallSchoolAnalysis[smallSchoolAnalysis$schoolSize == "tiny", "pSchools"]) >= 0.2 &
     pull(smallSchoolAnalysis[smallSchoolAnalysis$schoolSize == "small", "pStudents"]) < 0.4) {
    undersample <- TRUE
    increase <- TRUE
  }
  
  L <- 1 + 3 * filter(smallSchoolAnalysis, schoolSize == "tiny") %>% pull(pStudents)/4 + 
      filter(smallSchoolAnalysis, schoolSize == "very small") %>% pull(pStudents)/2
  
  minNormal <- round(schoolSampleSize * L * filter(smallSchoolAnalysis, schoolSize == "normal") %>% pull(pStudents), digits = 0)
  minSmall <- round(n * L *  filter(smallSchoolAnalysis, schoolSize == "small") %>% pull(pStudents)/
            filter(smallSchoolAnalysis, schoolSize == "small") %>% pull(menr), digits = 0)
  minVerySmall <- round(n * L *  filter(smallSchoolAnalysis, schoolSize == "very small") %>% pull(pStudents)/
            filter(smallSchoolAnalysis, schoolSize == "very small") %>% pull(menr), digits = 0)
  minTiny <- round(n * L *  filter(smallSchoolAnalysis, schoolSize == "tiny") %>% pull(pStudents)/
            filter(smallSchoolAnalysis, schoolSize == "tiny") %>% pull(menr), digits = 0)
  
  if(!increase) {
    minNormal <- round(n/tcs, 0)
    minSmall <- 0
    minVerySmall <- 0
    minTiny <- 0
  }
  
  nSchools <- data.frame(
    size = c("normal", "small", "very small", "tiny", "total", "undersample"),
    n = c(minNormal, minSmall, minVerySmall, minTiny, sum(minNormal, minSmall, minVerySmall, minTiny), undersample)
  )

  return(nSchools)
  
}




