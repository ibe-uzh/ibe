#' Check if table with particular name exists in database
#'
#' @param conn Established connection to database
#' @param table_name Name of table in database
#' @param database_name Name of database
#'
#' @return Logical - TRUE if database table exists
#' @import DBI
#'
#' @export
table_exists <- function(conn, table_name, database_name) {
  query <- sprintf("SELECT COUNT(*) FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = '%s' AND TABLE_NAME = '%s'", 
                   database_name, table_name)
  exists <- dbGetQuery(conn, query)
  return(exists[1,1] > 0)
}