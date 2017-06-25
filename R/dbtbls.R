#'dbtbls Searches the databases and schema 
#'
#'Searches the databases and schema and returns creator's name 
#'@return Returns all tables from the database.
#'@param conn: Connection to database. Can used assigned output from function connectR for the connection.
#'@param db: Database name i.e. default
#'@param schema: Database schema i.e public
#'@export
#'@examples
#'  #Search for default user (account signed into the computer)
#'   tables in the database default.
#'    dbtbls(post)->dbtables
#'
#'  #Search for default user (account signed into the computer)
#'   tables in the database c("postgres","postgres1"), also searches for schema public.
#'    dbtbls(post,db=c("postgres","postgres1"),schema="public")->dbtables



dbtbls<-function(conn, db=NULL, schema=NULL){
  
   if (conn$info$dbms.name=="Teradata"){
    
    db<-dbase(conn,db)
    
    sel<-"SELECT A.TABLENAME, B.CREATORNAME, B.CREATETIMESTAMP"
    fro<-" FROM DBC.TABLESIZE A, DBC.TABLES B"
    wh<-paste0(" WHERE A.DATABASENAME IN (\'", db, "\') AND A.TABLENAME=B.TABLENAME")
    
    query<-paste0(sel, fro, wh)
    
    collect(tbl(conn,sql(query)))->result
    result<-arrange(result,desc(SkewFactor))
    
  }
  
  if(conn$info$dbms.name=="PostgreSQL") {
    
    schema<-schem(schema)
    
    sel<-"SELECT Schemaname, Tablename, tableowner"
    fro<-" FROM pg_catalog.pg_tables"
    wh<-paste0(" where schemaname IN (\'",schema,"\')")
    
    query<-paste0(sel,fro,wh)
    
    collect(tbl(conn,sql(query)))->result
    result<-arrange(result, tableowner)
  }
  return(result)
}


