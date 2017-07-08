#'create_indexes creates indexes onto an existing table
#'
#'Creates indexes onto an existing table within a database
#'@return Indexes on a table
#'@param conn: Connection to database. Can used assigned output from function connectR for the connection.
#'@param table: Name of table on the database
#'@param indexes: a vector of column names to become indexes for a table.
#'@param unique: Specifies if the indexes are unique or not (TRUE or FALSE).
#'
#'@export
#'@examples
#'  #Creates a table with no data on the database, but allows primary
#'  keys/indexes to be created:
#'    create_indexes(post, "testdata", testdata, c("col1","col2"))



#---- db_create_indexes ----
create_indexes<- function (con, table, indexes= NULL, unique = FALSE,...){
  assertthat::assert_that(assertthat::is.string(table),
                          assertthat::is.flag(unique))
  
  if(unique==F){
    cre<-"CREATE INDEX "
  } else {
    cre<-"CREATE UNIQUE INDEX "
  }
  
  cols<-paste0(indexes, collapse = ", ")
  dplyr::sql(paste0(cre, "(", cols ,")" ," ON ", table,";"))->sql
  suppressWarnings(DBI::dbExecute(con, sql,...))
}
