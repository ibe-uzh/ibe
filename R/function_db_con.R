#' Connect to IBEs databases
#' 
#' Wrapper around RMariaDB::dbConnect to connect to itembank, checkauswertung (sessions / results), and check_data_revisions (mutterdatei). An active SSH tunnel is required!
#' 
#' @param user Username to log on to DB server with.
#' @param db Character: Which database to connect to. If several or "all" are chosen, the output will be a list of connections.
#'
#' @return A connection object of list of connection objects.
#' 
#' @export
db_con <- function(user, db = c("all", "itembank", "checkauswertung", "mutterdatei", "tracking", "check_data_revisions") ) {
  
  db <- match.arg(db, several.ok =  TRUE)
  db[db %in% c("mutterdatei", "tracking")] <- "check_data_revisions"
  db <- unique(db)
  if (any(db == "all")) db <- c("itembank", "checkauswertung", "check_data_revisions")
  pw <- rstudioapi::askForPassword(paste("DB server password for", user))
  cons <- list()
  for (dbi in db) {
    hostname <- switch(dbi,
                       itembank = "127.0.0.1",
                       checkauswertung = "127.0.0.1",
                       check_data_revisions = "ibedb-exoscale-a9317092-eb47-4f96-875c-c8641b2337e7.a.aivencloud.com")
    portnr = switch(dbi,
                    itembank = 13306,
                    checkauswertung = 13306,
                    check_data_revisions = 21699)
    cons[[dbi]] <- RMariaDB::dbConnect(drv = RMariaDB::MariaDB(),
                                       dbname = dbi,
                                       username = user,
                                       password = pw,
                                       host = hostname,
                                       port = portnr)
    message(paste("Created connection to", dbi))
  }
  if (length(dbi) == 1) dbi <- dbi[[1]]
  invisible(cons)
  
}
