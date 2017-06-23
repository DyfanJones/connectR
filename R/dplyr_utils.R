#---- tbl -----

tbl.src_connectR <- function(src, from, ...) {
  tryCatch(
    {
      dbplyr::tbl_sql("connectR", src=src, from=from, ...)
    }
  )
}


#,
#error=function(cond) {
#  if (dplyr:::isFALSE(db_has_table(src$con, from))) {
#    stop("Table ", from, " not found in database ", src$path, call. = FALSE)
#  }
#  return(cond)
#}
#---- copy_to ----

copy_to.src_connectR <-
  function(conn,
           df,
           name = deparse(substitute(df)),
           overwrite=FALSE,
           temporary = FALSE,
           types = NULL,
           unique_indexes = NULL,
           indexes = NULL,
           force=FALSE,
           append=FALSE,
           primary=NULL,
           ...){

    assertthat::assert_that(is.data.frame(df),
                            assertthat::is.string(name),
                            assertthat::is.flag(temporary),
                            assertthat::is.flag(overwrite))

    # cannot add indexes to volatile tables
    if(temporary == TRUE && (!is.null(indexes) || !is.null(unique_indexes))){
      stop('Teradata cannot add indexes to volatile tables.', call. = FALSE)
    }

    # prevent confusion from having multiple classes
    class(df) <- "data.frame"
    if(!is.null(primary)){
      
      db_create_primary(conn,name=name,value=df,primary=primary)
      if(conn$info$dbms.name=="PostgreSQL"){names(df)<-tolower(names(df))}
      
      DBI::dbWriteTable(conn=conn$con,
                        name=name,
                        value=df,
                        overwrite=overwrite,
                        temporary=temporary,
                        types=types,
                        unique_indexes=unique_indexes,
                        indexes = indexes,
                        force=force,
                        append=T,
                        ...)
      print(paste0("data.frame ", name,
                   " with rows:",nrow(df)," size:",
                   format(object.size(df), units = "auto"),
                   " has been created in the database"))
      
    }else{
    DBI::dbWriteTable(conn=conn$con,
                      name=name,
                      value=df,
                      overwrite=overwrite,
                      temporary=temporary,
                      types=types,
                      unique_indexes=unique_indexes,
                      indexes = indexes,
                      force=force,
                      append=append,
                      ...)
    
    print(paste0("data.frame ", name,
                 " with rows:",nrow(df)," size:",
                 format(object.size(df), units = "auto"),
                 " has been created in the database"))
    }
  }


#---- copied from dplyr ----
"%||%" <- function(x, y) if(is.null(x)) y else x


sql_mutating_join <- function(type, x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"),
                              auto_index = FALSE, ...) {
  by <- dplyr:::common_by(by, x, y)
  y <- dplyr:::auto_copy(x, y, copy, indexes = if (auto_index) list(by$y))
  sql <- sql_join(x$src$con, x, y, type = type, by = by, suffix = suffix)
  update(tbl(x$src, sql), group_by = groups(x))
}


#---- drop_table ----
db_drop_table.connectR_connection <-
  function(con,
           table,
           force = FALSE,
           purge = FALSE,
           ...) {
    sql <- dbplyr::build_sql("DROP TABLE ",
                     if (force) {
                       sql("IF EXISTS ")
                     },
                     ident(table),
                     if (purge) {
                       sql(" PURGE")
                     },
                     con = con)
    DBI::dbExecute(con, sql)
    print(paste0("Table ", table," has been deleted from the database"))
  }

#---- sql_escape_ident ----
sql_escape_ident.src_connectR <- function(con, x) {
  sql_quote(x, "`")
}


