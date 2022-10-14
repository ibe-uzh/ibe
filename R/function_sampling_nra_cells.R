# function that defines nra cells based on user input:
# respondents are identified by variable "status" having numeric value == 1
# df = must be dataframe
# nResp = minimum number of respondents in each cell
# nra.sep = string vector containing variables that define groups for which separate nra_cell_calculations are done (regions, gender etc.)
# nra.sort = string vector containing variables that identify similar clusters (sorting variables)
# respVar = name of variable that defines respondents
# respValue = value of respVar defining respondents
# clustVar = variable name of clustering variable; clusters are not split across cells (e.g. schools)

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