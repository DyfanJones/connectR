#'copy_loop_to transfers table from one database to another
#'
#'Transfers table from one database to the another without overloading R memory. Best used when databases don't have a connection to eachother.
#'@return Transfers table from intital database to another.
#'@param conn1: Connection to initial database where the table is coming from.
#'@param conn2: Connection to the database where the table is transferring to. If conn2 is NULL then conn2 will default to initial database.
#'@param name: Name of table on conn2 database
#'@param n: Batch loader, amount of data to be transferred with each cycle.
#'@param statement: SQL statement on initial database.
#'
#'@export
#'@examples
#'  #Sends table (created from sql statement) from post database to tera database with a batch load of 100.
#'    copy_loop_to(post, tera, "testdata", 100, "SELECT * FROM TESTTABLE")



copy_loop_to<-function(conn1,conn2=NULL, name, n=NULL, statement){
  assertthat::assert_that(assertthat::is.string(name),
                          assertthat::is.string(statement))
  if(is.null(n)){
    stop("Batch load is unknown, please give n a value.", call. = FALSE)
  }
  
  res<-DBI::dbSendQuery(conn1$con, statement)
  
  if(is.null(conn2)){
    conn=conn1
  } else {conn=conn2}
  
  while(!DBI::dbHasCompleted(res)){
    chunk<-DBI::dbFetch(res,n=n)
    
    if(conn$info$dbms.name=="PostgreSQL"){names(chunk)<-tolower(names(chunk))}
    
    suppressWarnings(DBI::dbWriteTable(conn=conn$con,
                                       name=name,
                                       value=chunk,
                                       overwrite=FALSE,
                                       temporary=FALSE,
                                       append=TRUE))
  }
  
  print(paste0("Looping complete data frame transferred to ",conn$info$dbms.name))
  
}
