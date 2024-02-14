#' Saves df as temporary table and loads its content into an existing database table (OVERWRITE!) - use df_to_empty_sql if necessary
#'
#' @param df Data frame that needs to be written into a database
#' @param conn_name Established connection to database
#' @param table_name Name of table in database
#' @return SQL query that was used for writing table
#' @import DBI
#' @export
write_df_to_db <- function(df, conn_name, table_name) {
  # Save sessions as a temporary file
  tempPath <- tempfile("temporary_data_frame", fileext = ".csv")
  tempPath <- gsub("\\", "/", tempPath, fixed = TRUE)
  write.table(df, tempPath, row.names = FALSE, quote = FALSE, sep = "\t", col.names = FALSE)
  
  # Delete possible previous entries
  query <- sprintf("DELETE FROM %s", table_name)
  dbExecute(conn_name, query) 
  
  # Prepare column names, adding backticks around reserved words
  vars <- colnames(df)
  vars <- ifelse(vars %in% c("check", "index"), sprintf("`%s`", vars), vars)
  vars_string <- paste(vars, collapse = ", ")
  
  # Upload temporary file to database (faster than dbWriteTable)
  query <- sprintf("LOAD DATA LOCAL INFILE '%s' INTO TABLE %s FIELDS TERMINATED BY '\t' ENCLOSED BY '\"' LINES TERMINATED BY '\n' (%s)",
                   tempPath, table_name, vars_string)
  dbExecute(conn_name, query)
  
  print(query)
}
