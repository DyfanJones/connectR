#'db_create_primary creates empty table with primary indexes/ keys
#'
#'Creates a table in the database from a dataframe and creates primary keys/ indexes
#'@return Empty table in database with primary keys/ indexes
#'@param conn: Connection to database. Can used assigned output from function connectR for the connection.
#'@param name: Name of table on the database
#'@param value: Data.frame
#'@param primary: Column(s) to created to primary keys / indexes.
#'
#'@export
#'@examples
#'  #Creates a table with no data on the database, but allows primary
#'  keys/indexes to be created:
#'    db_create_primary(post, "testdata", testdata, c("col1","col2"))

db_create_primary<-function(conn, name=NULL, value=NULL, primary=NULL, query = FALSE){
  assertthat::assert_that(assertthat::is.string(name),
                          is.data.frame(value),
                          is.character(primary))
  
  vars <- names(value)
  assertthat::assert_that(all(tolower(unlist(primary)) %in% tolower(vars)))
  
  dplyr::db_data_type(conn$con,value)->dbtype
  
  data_frame(name=c(""),
             type=c(""))->t
  
  for(i in 1:length(dbtype)){
    t[i,1]<-names(value[i])
    t[i,2]<-dbtype[i]
  }
  
  gsub("\\.","",t[[1]])->t[[1]]
  gsub(" ","_",trimws(t[[1]]))->t[[1]]
  
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
  
  if(query==T){
    return(SQL)
  }
  
  DBI::dbExecute(conn$con, SQL)
  
  print(paste0("Table ", name, " has been created on ",
               conn$info$dbms.name, " with primary indexes / keys (",
               p,")"))
}