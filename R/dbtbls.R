#'dbtbls function
#'
#'Function that connects to data bases, and also ecrypt password for safe storage.
#'@return Connection to database
#'@param conn: Connection to database. Can used assigned output from function connectR for the connection.
#'@param uid: User id for database
#'@param sbstr: A 3 character string, that searches table names i.e. "DL_". Can set sbstr to be a vector for multiple search i.e. c("DL_","DJ_")
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
#'   tables in the database default, also searches for c("DJ_","DRJ") and
#'    at start of table name variable.
#'    dbtbls(post,sbstr=c("DJ_","DRJ"))->dbtables
#'
#'  #Search for default user, (account signed into the computer)
#'   tables in the database postgres, shopdirect and also searches in the schema public
#'    at start of table name variable.
#'    dbtbls(post,sbstr=c("DJ_","DRJ"), db=c("postgres","shopdirect"), schema="public")->dbtables


dbtbls<-function(conn, uid=NULL,sbstr=NULL, db=NULL, schema=NULL){

  if (!is.null(sbstr) && nchar(sbstr[1])!=3) {
    stop("sbstr need to be vector with each element 3 nchar or set to default", call. = FALSE)
  }

  if (conn$info$dbms.name=="Teradata"){

    UID<-UID(conn, uid)
    sbstr<-sbstr(sbstr)
    db<-dbase(db)

    sel<-"SELECT A.TABLENAME, SUM(A.CURRENTPERM) AS CURRENTPERM, (100 - (AVG(CurrentPerm)/MAX(CurrentPerm)*100)) AS SkewFactor  , B.CREATORNAME, B.CREATETIMESTAMP"
    fro<-" FROM DBC.TABLESIZE A, DBC.TABLES B"
    wh<-paste0(" WHERE A.DATABASENAME IN (\'", db, "\') AND A.TABLENAME=B.TABLENAME AND (CREATORNAME IN (\'",UID,"\')",sbstr,") GROUP BY 1,4,5")

    query<-paste0(sel, fro, wh)

    as.data.frame(tbl(conn,sql(query)))->result
    result<-arrange(result,desc(SkewFactor))

  }

  if(conn$info$dbms.name=="PostgreSQL") {

    UID<-UID(conn,uid)
    schema<-schem(schema)

    sel<-"SELECT a.relname as Table, pg_size_pretty(pg_total_relation_size(relid)) As Size, pg_size_pretty(pg_total_relation_size(relid) - pg_relation_size(relid)) as External_Size, b.tableowner"
    fro<-" FROM pg_catalog.pg_statio_user_tables as a inner join pg_tables as b on a.relname = b.tablename"
    wh<-paste0(" where b.tableowner IN (\'",UID,"\')"," and a.schemaname IN (\'",schema,"\')")

    query<-paste0(sel,fro,wh)

    tbl(conn,sql(query))->result
    as.data.frame(tbl(conn,sql(query)))->result
    result<-arrange(result,desc(size))
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

dbase<-function(db){
  if(is.null(db)){datab<-"default"} else {
    datab<-paste0(db, collapse="\', \'")}
  datab
}

schem<-function(schema){
  if(is.null(schema)){schema<-"public"} else {
    schema<-paste0(schema, collapse="\', \'")}
  schema
}
