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
dbusr <- function(conn,
                  uid = NULL,
                  sbstr = NULL,
                  db = NULL,
                  schema = NULL) {
  UseMethod("dbusr")
}

#'@export
dbusr.src_connectR <-
  function(conn,
           uid = NULL,
           sbstr = NULL,
           db = NULL,
           schema = NULL) {
    if (!is.null(sbstr) && any(lapply(sbstr, nchar) != 3)) {
      stop("sbstr need to be vector with each element 3 nchar or set to default",
           call. = FALSE)
    }
    
    if (conn$info$dbms.name == "Teradata") {
      UID <- UID(conn, uid)
      sbstr <- sbstr(sbstr)
      db <- dbase(conn, db)
      
      sel <-
        "SELECT A.TABLENAME, SUM(A.CURRENTPERM) AS CURRENTPERM, (100 - (AVG(CurrentPerm)/MAX(CurrentPerm)*100)) AS SkewFactor  , B.CREATORNAME, B.CREATETIMESTAMP"
      fro <- " FROM DBC.TABLESIZE A, DBC.TABLES B"
      wh <-
        paste0(
          " WHERE A.DATABASENAME IN (\'",
          db,
          "\') AND A.TABLENAME=B.TABLENAME AND (CREATORNAME IN (\'",
          UID,
          "\')",
          sbstr,
          ") GROUP BY 1,4,5"
        )
      
      query <- paste0(sel, fro, wh)
      
      collect(tbl(conn, sql(query))) -> result
      result <- arrange(result, desc(SkewFactor))
      
    }
    
    if (conn$info$dbms.name == "PostgreSQL") {
      UID <- UID(conn, uid)
      schema <- schem(schema)
      
      sel <-
        "SELECT a.relname as Table, pg_size_pretty(pg_total_relation_size(relid)) As Size, pg_size_pretty(pg_total_relation_size(relid) - pg_relation_size(relid)) as External_Size, b.tableowner"
      fro <-
        " FROM pg_catalog.pg_statio_user_tables as a inner join pg_tables as b on a.relname = b.tablename"
      wh <-
        paste0(" where b.tableowner IN (\'",
               UID,
               "\')",
               " and a.schemaname IN (\'",
               schema,
               "\')")
      
      query <- paste0(sel, fro, wh)
      
      tbl(conn, sql(query)) -> result
      collect(tbl(conn, sql(query))) -> result
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
    sbstr <- paste0(" OR SUBSTR(A.TABLENAME,1,3) IN (\'", x, "\')")
  }
  sbstr
}
