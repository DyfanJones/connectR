#'db_send_query sends SQL
#'
#'Sends sql queries to the database.
#'@return Sends sql queries directly to the database.
#'@param con: Connection to database. Can used assigned output from function connectR for the connection.
#'@param statement: Sql query to be sent to the database.
#'@param ...: Other parameters passed on to methods.
#'
#'@examples
#'  #Sending sql query to create table on database.
#'    db_send_query(post, "create table hello as (select * from world) with no data;")

#'@export
db_send_query <- 
  function(conn, statement, ...) {
  UseMethod("db_send_query")
}

#'@export
db_send_query.src_connectR <- 
  function(conn, statement, ...) {
  assertthat::assert_that(assertthat::is.string(statement))
  suppressWarnings(DBI::dbSendStatement(con = conn$con, statement, ...))
}