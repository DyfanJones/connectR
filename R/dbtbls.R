#'dbtbls function
#'
#'Function searches tables in entire datadata / schema.
#'@return Connection to database
#'@param conn: Connection to database. Can used assigned output from function connectR for the connection.
#'@param db: Database name i.e. default
#'@param schema: Database schema i.e public
#'
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
    wh<-paste0(" WHERE A.DATABASENAME IN (\'", db, "\') AND A.TABLENAME=B.TABLENAME GROUP BY 1,4,5")
    
    query<-paste0(sel, fro, wh)
    
    as.data.frame(tbl(conn,sql(query)))->result
    result<-arrange(result,desc(SkewFactor))
    
  }
  
  if(conn$info$dbms.name=="PostgreSQL") {
    
    db<-dbase(conn,db)
    schema<-schem(schema)
    
    sel<-"SELECT a.relname as Table, pg_size_pretty(pg_total_relation_size(relid)) As Size, pg_size_pretty(pg_total_relation_size(relid) - pg_relation_size(relid)) as External_Size, b.tableowner"
    fro<-" FROM pg_catalog.pg_statio_user_tables as a inner join pg_tables as b on a.relname = b.tablename"
    wh<-paste0(" where a.schemaname IN (\'",schema,"\')")
    
    query<-paste0(sel,fro,wh)
    
    tbl(conn,sql(query))->result
    as.data.frame(tbl(conn,sql(query)))->result
    dplyr::mutate(result,type = substr(size,nchar(size)-2,nchar(size)),
                  N = ifelse(type==tolower("kb"),as.numeric(substr(size, 1, nchar(size)-3)),
                             ifelse(type==tolower("mb"),as.numeric(substr(size, 1, nchar(size)-3))*0.001,
                                    ifelse(type==tolower("gb"),as.numeric(substr(size, 1, nchar(size)-3))*0.00001,1))))->result
    result<-arrange(result, desc(N)) %>% select(-N,-type)
  }
  return(result)
}

UID<-function(conn, uid){
  if(conn$info$dbms.name=="Teradata"){
    if(is.null(uid)){UID<-paste0(toupper(Sys.getenv('USERNAME')),"\'",", \'","SAS_",Sys.getenv('USERNAME'))
    } else {UID<-paste0(uid,collapse="\', \'")}
  } else {
    if(is.null(uid)){UID<-paste0((Sys.getenv('USERNAME')))
    } else {UID<-paste0(uid,collapse="\', \'")}}
  UID
}

sbstr<-function(x){
  if(is.null(x)){sbstr<-""} else {x<-paste0(x, collapse="\', \'")
  sbstr<-paste0(" OR SUBSTR(A.TABLENAME,1,3) IN (\'",x,"\')")}
  sbstr
}

dbase<-function(conn,db){
  if(is.null(db)){datab<-conn$info$dbname} else {
    datab<-paste0(db, collapse="\', \'")}
  datab
}

schem<-function(schema){
  if(is.null(schema)){schema<-"public"} else {
    schema<-paste0(schema, collapse="\', \'")}
  schema
}