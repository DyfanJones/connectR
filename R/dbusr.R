#'dbusr search database for user created tables
#'
#'Searches the databases and schema for user created tables, returns skewness and size of tables.
#'@return Returns all tables user has created from the database.
#'@param conn: Connection to database. Can used assigned output from function connectR for the connection.
#'@param uid: User id for database
#'@param sbstr: A 3 character string, that searches table names i.e. "DL_". Can set sbstr to be a vector for multiple search i.e. c("DL_","DJ_")
#'@param db: Database name i.e. default
#'@param schema: Database schema i.e public
#'
#'@examples
#'  #Search for default user (account signed into the computer)
#'   tables in the database default.
#'    dbusr(post)->dbtables
#'
#'  #Search for default user (account signed into the computer)
#'   tables in the database default, also searches for c("DJ_","DRJ") and
#'    at start of table name variable.
#'    dbusr(post,sbstr=c("DJ_","DRJ"))->dbtables
#'
#'  #Search for default user, (account signed into the computer)
#'   tables in the database postgres, shopdirect and also searches in the schema public
#'    at start of table name variable.
#'    dbusr(post,sbstr=c("DJ_","DRJ"), db=c("postgres","postgres1"), schema="public")->dbtables

#'@export
dbusr <-
  function(conn,
           uid = NULL,
           sbstr = NULL,
           db = NULL,
           schema = NULL,
           query = FALSE) {
    UseMethod("dbusr")
  }

#'@export
dbusr.src_connectR <-
  function(conn,
           uid = NULL,
           sbstr = NULL,
           db = NULL,
           schema = NULL,
           query = FALSE) {
    
    if (conn$info$dbms.name == "Teradata") {
      UID <- UID(conn, uid)
      sbstr <- sbstr(sbstr)
      db <- dbase(conn, db)
      
      sel <-
        "SELECT \n A.DATABASENAME, \n A.TABLENAME, \n SUM(A.CURRENTPERM) AS CURRENTPERM,  \n (100 - (AVG(CurrentPerm)/MAX(CurrentPerm)*100)) AS SkewFactor, \n B.CREATORNAME, \n B.CREATETIMESTAMP"
      fro <- "\n FROM DBC.TABLESIZE A, \n DBC.TABLES B"
      wh <-
        paste0(
          "\n WHERE A.DATABASENAME IN (\'",
          db,
          "\') \n AND A.TABLENAME=B.TABLENAME \n AND (CREATORNAME IN (\'",
          UID,
          "\')",
          sbstr,
          ") \n GROUP BY 1,2,5,6"
        )
      
      sqlquery <- sql(paste0(sel, fro, wh))
      
      if(query==TRUE){return(sqlquery)}
      
      collect(tbl(conn, sqlquery)) -> result
      result <- arrange(result, desc(SkewFactor))
      
    }
    
    if (conn$info$dbms.name == "PostgreSQL") {
      UID <- UID(conn, uid)
      schema <- schem(schema)
      
      sel <-
        "SELECT \n a.relname as Table, \n pg_size_pretty(pg_total_relation_size(relid)) As Size, \n pg_size_pretty(pg_total_relation_size(relid) - pg_relation_size(relid)) as External_Size, \n b.tableowner"
      fro <-
        "\n FROM pg_catalog.pg_statio_user_tables as a \n inner join pg_tables as b \n on a.relname = b.tablename"
      wh <-
        paste0("\n where b.tableowner IN (\'",
               UID,
               "\')",
               "\n and a.schemaname IN (\'",
               schema,
               "\')")
      
      sqlquery <- sql(paste0(sel, fro, wh))
      
      if(query==TRUE){return(sqlquery)}
      
      collect(tbl(conn, sqlquery)) -> result
      dplyr::mutate(
        result,
        type = substr(size, nchar(size) - 2, nchar(size)),
        N = ifelse(
          type == tolower("kb"),
          as.numeric(substr(size, 1, nchar(size) - 3)),
          ifelse(
            type == tolower("mb"),
            as.numeric(substr(size, 1, nchar(size) - 3)) * 0.001,
            ifelse(type == tolower("gb"), as.numeric(substr(
              size, 1, nchar(size) - 3
            )) * 0.00001, 1)
          )
        )
      ) -> result
      result <- arrange(result, desc(N)) %>% select(-N, -type)
    }
    
    return(result)
  }

sbstr <- function(x) {
  if (is.null(x)) {
    sbstr <- ""
  } else {
    x <- paste0(x, collapse = "\', \'")
    sbstr <- paste0(" OR \n SUBSTR(A.TABLENAME,1, (CASE WHEN POSITION('_'  IN  A.TABLENAME )=0 THEN 0 ELSE POSITION('_'  IN  A.TABLENAME )-1 END) ) IN (\'", x, "\')")
  }
  sbstr
}
