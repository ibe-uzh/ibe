#' Create empty sql table based on r data frame data types
#'
#' @param df Data frame with defined column data types
#' @param conn Established connection to database
#' @param table_name Name of table in database
#'
#' @return SQL query that was used for creating empty table
#' @import DBI
#' @export
df_to_empty_sql <- function(df, conn, table_name) {
  
  # function that maps R to SQL data types - will be applied to all columns later
  get_sql_type <- function(v) {
    if (is.numeric(v)) {
      return("DOUBLE")
    } else if (is.integer(v)) {
      return("INT")
    } else if (is.character(v)) {
      max_length <- max(nchar(as.character(v), 'bytes'), na.rm = TRUE)
      if (is.na(max_length) || max_length == 0 || max_length == -Inf) {
        max_length <- 255  # Set a default length
      }
      return(paste("VARCHAR(", max_length, ")", sep=""))
    } else if (is.logical(v)) {
      return("BOOLEAN")
    } else if (is.factor(v)) {
      max_length <- max(nchar(as.character(v), 'bytes'), na.rm = TRUE)
      if (is.na(max_length) || max_length == 0 || max_length == -Inf) {
        max_length <- 255  # Set a default length
      }
      return(paste("VARCHAR(", max_length, ")", sep=""))
    } else if (is.Date(v)) {
      return("DATE")
    } 
    else {
      stop("Unsupported column type")
    }
  }
  
  # function that creates the sql create table statement
  create_table_sql <- function(df, table_name) {
    cols <- sapply(df, get_sql_type)
    col_defs <- paste(names(cols), cols, collapse = ", ")
    return(paste("CREATE TABLE", table_name, "(", col_defs, ")", sep=" "))
  }
  
  sql <- create_table_sql(df, table_name)
  dbExecute(conn, sql)
  
  print(sql)
}
