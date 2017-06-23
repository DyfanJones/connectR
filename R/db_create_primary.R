#'db_create_primary search database for user created tables
#'
#'Creates a table in the database from a dataframe and creates primary keys/ indexes
#'@return Connection to database
#'@param conn: Connection to database. Can used assigned output from function connectR for the connection.
#'@param name: Name of table on the database
#'@param value: Data.frame
#'@param primary: Column(s) to created to primary keys / indexes.
#'
#'@export
#'@examples
#'  #Search for default user (account signed into the computer)
#'   tables in the database default.
#'    db_create_primary(post, "testdata", testdata, c("col1","col2"))

db_create_primary<-function(conn, name=NULL, value=NULL, primary=NULL){
  assertthat::assert_that(assertthat::is.string(name),
                          is.data.frame(value),
                          is.character(primary))
  
  dplyr::db_data_type(conn$con,value)->dbtype
  
  data_frame(name=c(""),
             type=c(""))->t
  
  for(i in 1:length(dbtype)){
    t[i,1]<-names(value[i])
    t[i,2]<-dbtype[i]
  }
  
  rows <- apply(t, 1, paste0, collapse = " ")
  values <- paste0(rows, collapse = ",\n ")
  
  crt<-paste0("CREATE TABLE ", name ," ( \n ")
  p<-paste0(primary,collapse = ", ")
  if(conn$info$dbms.name=="Teradata"){
    prim<-")\n PRIMARY INDEX ("
    k<- paste0(p, ");")
  } else {
    prim<-", \n PRIMARY KEY ("
    k<- paste0(p, "));") 
  }
  
  dplyr::sql(paste0(crt, values, prim, k))->SQL
  SQL
  DBI::dbExecute(conn$con, SQL)
}