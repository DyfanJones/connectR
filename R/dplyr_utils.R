#---- tbl -----
tbl.src_connectR <- function(src, from, ...) {
  tryCatch(
    {
      dbplyr::tbl_sql("connectR", src=src, from=from, ...)
    }
  )
}

#---- copy_to ----

copy_to.src_connectR <-
  function(conn,
           df,
           name = deparse(substitute(df)),
           overwrite=FALSE,
           temporary = FALSE,
           types = NULL,
           indexes = NULL,
           unique_indexes = NULL,
           force=FALSE,
           append=FALSE,
           primary=NULL,
           ...){
    
    assertthat::assert_that(is.data.frame(df),
                            assertthat::is.string(name),
                            assertthat::is.flag(temporary),
                            assertthat::is.flag(overwrite),
                            assertthat::is.flag(append))
    
    # cannot add indexes to volatile tables
    if(temporary == TRUE && (!is.null(indexes) || !is.null(unique_indexes))){
      stop(paste0(conn$info$dbms.name," cannot add indexes to volatile tables."), call. = FALSE)
    }
    
    # prevent confusion from having multiple classes
    class(df) <- "data.frame"
    
    #overwrite functionality
    if(append==TRUE && overwrite==TRUE){
      stop(
        "Overwrite and append can't both be TRUE."
      )
    }
    
    # PRIMARY INDEX
    if(!is.null(primary)){
      
      if(overwrite==TRUE){db_drop_table(conn,name)}
      
      gsub("\\.","",names(df))->names(df)
      gsub(" ","_",trimws(names(df)))->names(df)
      
      vars <- names(df)
      assertthat::assert_that(all(tolower(unlist(primary)) %in% tolower(vars)))
      
      db_create_primary(conn,name=name,value=df,primary=primary)
      if(conn$info$dbms.name=="PostgreSQL"){names(df)<-tolower(names(df))}
      
      DBI::dbWriteTable(conn=conn$con,
                        name=name,
                        value=df,
                        temporary=temporary,
                        types=types,
                        unique_indexes=unique_indexes,
                        indexes = indexes,
                        force=force,
                        append=TRUE,
                        ...)
      #crude method to add indexes
      if(!is.null(indexes)|!is.null(unique_indexes)){
        if(is.null(unique_indexes)){
          assertthat::assert_that(all(tolower(unlist(indexes)) %in% tolower(vars)))
          db_create_indexes(conn$con,name,indexes,unique=FALSE)
        } else {
          assertthat::assert_that(all(tolower(unlist(unique_indexes)) %in% tolower(vars)))
          db_create_indexes(conn$con,name,unique_indexes,unique=TRUE)
        }
      }
      
      print(paste0("data.frame ", name,
                   " with rows: ",nrow(df)," size: ",
                   format(object.size(df), units = "auto"),
                   " has been created in ", conn$info$dbms.name))
      
      # No Primary Index
    }else{
      
      gsub("\\.","",names(df))->names(df)
      gsub(" ","_",trimws(names(df)))->names(df)
      if(conn$info$dbms.name=="PostgreSQL"){names(df)<-tolower(names(df))}
      
      vars <- names(df)
      assertthat::assert_that(all(tolower(unlist(primary)) %in% tolower(vars)))
      
      DBI::dbWriteTable(conn=conn$con,
                        name=name,
                        value=df,
                        overwrite=overwrite,
                        temporary=temporary,
                        types=types,
                        force=force,
                        append=append,
                        ...)
      #crude method to add indexes
      if(!is.null(indexes)|!is.null(unique_indexes)){
        if(is.null(unique_indexes)){
          assertthat::assert_that(all(tolower(unlist(indexes)) %in% tolower(vars)))
          db_create_indexes(conn$con,name,indexes,unique=FALSE)
        } else {
          assertthat::assert_that(all(tolower(unlist(unique_indexes)) %in% tolower(vars)))
          db_create_indexes(conn$con,name,unique_indexes,unique=TRUE)
        }
      }
      
      print(paste0("data.frame ", name,
                   " with rows: ",nrow(df)," size: ",
                   format(object.size(df), units = "auto"),
                   " has been created in ", conn$info$dbms.name))
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


#---- db_drop_table ----
db_drop_table.src_connectR <-
  function(conn,
           table,
           force = FALSE,
           purge = FALSE,
           ...) {
    SQL <- sql(paste0("DROP TABLE ",if (force) {"IF EXISTS "},table,if (purge) {" PURGE"}))
    DBI::dbExecute(conn$con, SQL)
    print(paste0("Table ", table," has been deleted from the ", conn$info$dbms.name))
  }

#---- db_drop_view ----
db_drop_view <-
  function(conn,
           table,
           force = FALSE,
           purge = FALSE,
           ...) {
    SQL <- sql(paste0("DROP VIEW ",if (force) {"IF EXISTS "},table,if (purge) {" PURGE"}))
    DBI::dbExecute(conn$con, SQL)
    print(paste0("View ", table," has been deleted from the ", conn$info$dbms.name))
  }

#---- sql_escape_ident ----
#
sql_escape_ident.src_connectR<- function(con, x) {
  dbplyr::sql_quote(x, "`")
}

#---- sql_escape_string ----
sql_escape_string.src_connectR <- function(con, x) {
  dbplyr::sql_quote(x, "'")
}

#---- compute ----
compute.tbl_connectR <-
  function(x,
           name,
           primary = NULL,
           temporary = FALSE,
           append = FALSE,
           overwrite = FALSE,
           force = FALSE,
           type = NULL,
           data = FALSE,
           query= FALSE,
           ...) {
    # TBD: add params to control location and other CREATE TABLE options
    
    assertthat::assert_that(
      assertthat::is.string(name),
      assertthat::is.flag(temporary),
      assertthat::is.flag(append),
      assertthat::is.flag(force),
      assertthat::is.flag(data),
      assertthat::is.flag(query)
    )
    
    if(is.null(type)){type<-"table"}
    
    if (temporary) {
      stop(
        "connectR does not support temporary tables. Set temporary = FALSE in compute().",
        call. = FALSE)
    }
    
    #overwrite functionality
    if(append==TRUE && overwrite==TRUE){
      stop(
        "Overwrite and append can't both be TRUE."
      )
    }
    
    #primary functionality
    if(append==TRUE  && !is.null(primary)){
      stop(
        "can't create a primary index when appending to existing table",
        call. = FALSE)
    }
    
    if(trimws(tolower(type))=="view" && !is.null(primary)){
      stop(
        "can't create a primary index when on a view",
        call. = FALSE)
    }
    
    
    if(trimws(tolower(type))=="table" && overwrite==TRUE){
      db_drop_table(x$src,name)
    } else if (trimws(tolower(type))=="view" && overwrite==TRUE) {
      db_drop_view(x$src,name)
    }
    
    #variables from query
    vars <- dbplyr::op_vars(x)
    assertthat::assert_that(all(tolower(unlist(primary)) %in% tolower(vars)))
    
    x_aliased <- select(x, !!! rlang::syms(vars))
    SQL <- dbplyr::db_sql_render(x$src$con, x_aliased$ops)
    
    # TBD: implement db_compute.impala_connection and call it here instead of db_save_query
    
    
    
    name <- db_save_query(
      conn = x$src,
      sql = SQL,
      name = name,
      primary= primary,
      temporary = FALSE,
      append = append,
      force = force,
      type=trimws(type),
      data = data,
      query = query,
      ...
    )
    if(query==TRUE)(
      return(name)
    )
  }


#---- db_save_query ----
db_save_query.src_connectR <-
  function(conn,
           sql,
           name,
           primary = NULL,
           temporary = FALSE,
           append = FALSE,
           force = FALSE,
           type = NULL,
           data = FALSE,
           query = FALSE,
           ...) {
    # TBD: add params to control location and other CREATE TABLE options
    
    assertthat::assert_that(
      assertthat::is.string(name),
      assertthat::is.flag(temporary),
      assertthat::is.flag(append),
      assertthat::is.flag(force),
      assertthat::is.flag(data),
      assertthat::is.flag(query)
    )
    if (temporary) {
      stop(
        "connectR does not support temporary tables. Set temporary = FALSE in db_save_query().",
        call. = FALSE
      )
    }
    
    prep<-paste0("CREATE ",if(is.null(type) || tolower(type)=="table"){
      sql("TABLE ")
    } else if (tolower(type)=="view"){
      sql("VIEW ")
    } else {
      stop("Check type, connectR currently only supports table or view",call. = FALSE)
    },
    name,
    " ",
    "AS ("
    )
    
    dat<-if(data==FALSE){""} else {"NO"}
    
    sqlend<-paste0(if(is.null(type) || (tolower(type) =="table") && is.null(primary)){
      sql(paste0("WITH ", dat ," DATA;"))
    } else if (tolower(type)=="view"){
      sql(";")
    } else {
      pri(primary, data)
    }
    )
    
    tt_sql <- sql(paste0(if(append == T){
      paste0("INSERT INTO ", name, " ")} else {prep},
      sql, if(append == FALSE){paste0(") ", sqlend)} else{
        ""}
    )
    )
    
    if(query==TRUE){
      return(tt_sql)
    }
    
    DBI::dbExecute(conn$con, tt_sql)
    name
  }

pri<-function(primary, data){
  x<-paste0(primary, collapse="\', \'")
  if(data == TRUE) {
    pri<-paste0("WITH DATA \n PRIMARY INDEX (\'",x,"\');")
  } else {
    pri<-paste0("WITH NO DATA \n PRIMARY INDEX (\'",x,"\');")
  }
  pri
}

#---- db_data_type ----

db_data_type.src_connectR<- function(con, fields, ...) {
  char_type <- function(x) {
    n <- max(nchar(as.character(x), "bytes"), 0L, na.rm = TRUE)
    if (n <= 65535) {
      paste0("varchar(", n, ")")
    } else {
      "mediumtext"
    }
  }
  
  data_type <- function(x) {
    switch(
      class(x)[1],
      logical =   "boolean",
      integer =   "integer",
      numeric =   "float",
      factor =    char_type(x),
      character = char_type(x),
      Date =      "date",
      POSIXct =   "datetime",
      stop("Unknown class ", paste(class(x), collapse = "/"), call. = FALSE)
    )
  }
  vapply(fields, data_type, character(1))
}
