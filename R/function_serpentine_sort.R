############sort list by N implicit strata#####################################

serpentine_sort <- function(data, implstrat) {
  if ( !(require(dplyr) & require(forcats)) ) stop("Required packages 'dplyr' and 'forcats' not available!")
  has.na <- select(data, !!implstrat) %>%
    sapply(function(x) sum(is.na(x))>0)
  if (any(has.na)) stop(paste("The following stratificators have NA values:", paste(implstrat[has.na], collapse=", ") ))
  for (i in 1:length(implstrat)) { # make numeric variables for all implicit stratificators...
    imp.i <- data[, implstrat[i]]
    if (is.logical(imp.i)) imp.i <- as.numeric(imp.i)
    if (is.character(imp.i)) imp.i <- as.factor(imp.i)
    if (is.factor(imp.i)) imp.i <- forcats::fct_inorder(imp.i) %>% as.numeric()
    if (i%%2 == 0) imp.i <- max(imp.i)+1 - imp.i #...and invert every even stratificator's numerical value
    data[, paste0("num.imp_",i)] <- imp.i
  }; rm(imp.i, i)
  data <- data %>%
    arrange(across(all_of(paste0("num.imp_", 1:length(implstrat)))))
  return(data)
}
