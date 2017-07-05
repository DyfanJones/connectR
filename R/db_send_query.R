#'db_send_query sends SQL
#'
#'Sends sql queries to the database.
#'@return Sends sql queries directly to the database.
#'@param con: Connection to database. Can used assigned output from function connectR for the connection.
#'@param statement: Sql query to be sent to the database.
#'@param ...: Other parameters passed on to methods.
#'
#'@export
#'@examples
#'  #Sending sql query to create table on database.
#'    db_send_query(post$con, "create table hello as (select * from world) with no data;)


#---- db_send_query ----
db_send_query<-function(conn, statement, ...){
  assertthat::assert_that(
    assertthat::is.string(statement))
  suppressWarnings(DBI::dbSendStatement(con=conn$con,statement, ...))
}