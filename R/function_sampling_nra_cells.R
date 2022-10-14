#' Create non-response adjustment cells
#'
#' @param df A data frame with one case per row, where respondents can be identified using a participation status variable.
#' @param nResp The minimum number of respondents that an nra cell must contain; default is 15.
#' @param nra.sep character string or vector with variable names based on which separate cell formations are created (e.g. regions, gender, etc.).
#' @param nra.sort character string or vector with variable names on the basis of which the data is sorted so that cases which are similar with respect to this variables end up in the same nra cells.
#' @param respVar character string with name of variable that defines respondents
#' @param respValue numeric value of respVar defining respondents
#' @param clustVar character string with name of clustering variable; clusters are not split across cells (e.g. schools)
#'
#' @return Input data frame is completed with two columns (nraGroup, nraCell). nraGroup contains the combinations of values in nra.sep for which separate cell splits were performed. nraCell contains the name of the nra cell.
#' @import dplyr
#' @import stringr
#' @import rlang
#' @export
#'
#' @examples
nra_cells <- function(df, nResp = 15, nra.sep, nra.sort, respVar, respValue, clustVar) {

  # unquoted grouping variables
  sepVars <- rlang::syms(nra.sep)

  allGroups <- df %>%
    # sort data
    arrange(across(all_of(nra.sort))) %>%
    # define group names
    mutate(nraGroup = paste("g", !!!sepVars, sep = "_")) %>%
    # rename respVar
    rename(status = all_of(respVar))

  # separate allGroups into different df based on nra.sep => list (nraList)
  gNames <- arrange(allGroups, nraGroup) %>% pull(nraGroup) %>% unique()
  nraList <- list()
  for(i in 1:length(gNames)) {
    x <- filter(allGroups, nraGroup == gNames[i])
    if(nrow(filter(x, status == respValue)) < nResp) warning(paste0("less than ", nResp, " respondents in group ", gNames[i], "!"))
    nraList <- c(nraList, list(x))
  }

  # loop across all df; create new list with nra cells
  nraList2 <- list()
  for(i in 1:length(nraList)) {

    # get each df in list and rename cluster variable
    x <- nraList[[i]] %>%
      rename(clust = all_of(clustVar))

    # vector with unassigned
    clusters <- unique(pull(x, clust))

    # cell ids
    cellId <- 1

    # empty df
    withCellTotal <- slice(x, -(1:nrow(x)))

    # while not all clusters assigned to nra cell...
    while(length(clusters) > 0) {

      # create empty df
      withCell <- slice(x, -(1:nrow(x)))

      # while not all clusters assigned and current cluster(s) have not enough respondents...
      while(sum(filter(withCell, status == respValue) %>% pull(status)) < nResp & length(clusters) > 0) {

        # add next cluster to nra cell
        withCell <- bind_rows(withCell, filter(x, clust == clusters[1])) %>%
          mutate(nraCell = paste0(nraGroup, "_", cellId))
        # remove cluster from un-assigned clusters
        clusters <- clusters[-1]
      }

      # increase id number of nra cell
      cellId <- cellId + 1

      # rbind clusters to one nra cell
      withCellTotal <- bind_rows(withCellTotal, withCell)

    }

    # check if last cell has enough respondents; if not: collapse with second last cell
    lastCells <- tail(unique(withCellTotal$nraCell), 2)
    if((sum(filter(withCellTotal, status == respValue & nraCell == lastCells[2]) %>% pull(status)) < nResp) & length(lastCells) > 1) {
      withCellTotal <- mutate(withCellTotal,
                              nraCell = ifelse(nraCell == lastCells[2], lastCells[1], nraCell))
    }

    # rbind all data frames on list to one data frame
    nraList2 <- c(nraList2, list(withCellTotal))
  }

  # return input df with additional variable: nraCell and rename respVar to original name
  return(bind_rows(nraList2) %>% rename(!!respVar := status))

}
