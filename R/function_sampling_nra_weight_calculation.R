# calculate non-response adjusted weights based on user input:
# df = data frame containing nra cells (use function nra_cell to create these)
# cellVar = name of nra cell variable
# weightVar = name of weighting variable that needs adjustments
# nraWeightVar = name of output variable
# statusVar = name of participating status variable (must be integer)
# nonRespValue = value(s) of statusVar that represent non-responding cases for which responder weights are adjusted
# respValue = value of statusVar that represents respondents

#' Calculates non-response adjusted weights
#'
#' @param df Weighted data frame with nra cells defined; use nra_cells() for cell definition
#' @param cellVar Name of nra cell variable
#' @param weightVar Name of weight variable that needs adjustments
#' @param nraWeightVar Name of output variable
#' @param statusVar Name of integer variable containing participation codes, where one code represents respondents
#' @param nonRespValue Integer or integer vector containing statusVar values that represent non-responding cases for which responder weights are adjusted
#' @param respValue Integer for respVar value that represents responding cases
#'
#' @return Inout data frame with additional column nraWeightVar containing adjusted weights
#' @import dplyr
#' @export
#'
#' @examples
nra_weights <- function(df, cellVar, weightVar, nraWeightVar, statusVar, nonRespValue, respValue) {

  # get data frame and rename variables
  df <- df %>%
    rename(nraCell = all_of(cellVar), wgt = all_of(weightVar), status = all_of(statusVar))

  # vector with all nra cells
  cells <- unique(df$nraCell)

  # empty df that will grow
  nraFactors <- data.frame(NULL)

  # loop calculating factors
  for(i in 1:length(cells)) {
    # filter current cell and group by status
    cell <- filter(df, nraCell == cells[i]) %>%
      group_by(status) %>%
      summarise(sumW = sum(wgt)) %>%
      # factor = 1 in case no adjustments needed
      mutate(nraFactor = 1)
    # check if there are non-respondents
    if(sum(nonRespValue %in% cell$status) > 0) {
      # get sum of respondent weights
      sumWpre <- filter(cell, status == respValue) %>% pull(sumW)
      # get sum of respondents and non-respondent weights
      sumWpost <- sum(filter(cell, status %in% c(respValue, nonRespValue)) %>% pull(sumW))
      # calculate nra factor
      cell$nraFactor <- sumWpost/sumWpre
    }
    # reduce cell info to factor and cell name
    cell <- select(cell, nraFactor) %>%
      mutate(nraCell = cells[i]) %>%
      unique()

    # rbind factors to growing df
    nraFactors <- rbind(nraFactors, cell)

  }

  # bind factors to df
  df2 <- left_join(df, nraFactors, by = "nraCell") %>%
    mutate(nraWgt = case_when(
      # respondents get new weights
      status == respValue ~ wgt*nraFactor,
      # non-respondents get zero-weight
      status == nonRespValue ~ 0,
      # all others keep their weights
      TRUE ~ wgt)) %>%
    # use desired out variable name
    rename(!!nraWeightVar := nraWgt, !!weightVar := wgt)

  return(df2)

}

