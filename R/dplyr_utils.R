#---- tbl -----

tbl.src_connectR <- function(src, from, ...) {
  tryCatch(
    {
      dbplyr::tbl_sql("connectR", src=src, from=from, ...)
    },
    error=function(cond) {
      if (dplyr:::isFALSE(db_has_table(src$con, from))) {
        stop("Table ", from, " not found in database ", src$path, call. = FALSE)
      }
      return(cond)
    }
  )
}

#---- copy_to ----

copy_to.connectR_connection <-
  function(conn,
           df,
           name = deparse(substitute(df)),
           overwrite=FALSE,
           temporary = FALSE,
           types = NULL,
           unique_indexes = NULL,
           indexes = NULL,
           force=FALSE,
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

    DBI::dbWriteTable(conn=conn,
                      name=name,
                      value=df,
                      overwrite=overwrite,
                      temporary=temporary,
                      types=types,
                      unique_indexes=unique_indexes,
                      indexes = indexes,
                      force=force,
                      ...)

    print(paste0("data.frame ", name,
                 " with rows:",nrow(df)," size:",
                 format(object.size(df), units = "auto"),
                 " has been created in the database"))
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


#---- db_insert_into ----
# needs work on to iron out the bugs
db_insert_into.connectR_connection <-
  function(con, table, values, overwrite=F, ...) {
    assertthat::assert_that(assertthat::is.string(table),
                            is.data.frame(values),
                            assertthat::is.flag(overwrite))
    if (nrow(values) == 0) {
      return(NULL)
    }
    DBI::sqlAppendTable(con,table,values,...)->sql

    DBI::dbExecute(con, sql)
  }

#---- Create_table ----
#needs work on to iron out the bugs
db_create_table.connectR_connection <-
  function (con,
            table,
            types,
            temporary = FALSE,
            external = FALSE,
            force = FALSE,
            field_terminator = NULL,
            line_terminator = NULL,
            file_format = NULL,
            ...) {
    # TBD: add params to control location and other CREATE TABLE options

    assertthat::assert_that(
      assertthat::is.string(table),
      assertthat::is.character(types),
      assertthat::is.flag(temporary),
      assertthat::is_string_or_null(file_format),
      assertthat::is_nchar_one_string_or_null(field_terminator),
      assertthat::is_nchar_one_string_or_null(line_terminator)
    )
    if (temporary) {
      stop(
        "connectR does not support temporary tables. Set temporary = FALSE in db_create_table().",
        call. = FALSE
      )
    }
    field_names <-
      escape(ident(names(types)), collapse = NULL, con = con)
    fields <- sql_vector(
      paste0(field_names, " ", types),
      parens = TRUE,
      collapse = ", ",
      con = con
    )
    sql <- dbplyr::build_sql("CREATE ",
                     if (external) {
                       sql("EXTERNAL ")
                     },
                     "TABLE ",
                     if (force) {
                       sql("IF NOT EXISTS ")
                     },
                     ident(table),
                     " ",
                     if (!is.null(field_terminator) ||
                         !is.null(line_terminator)) {
                       sql("ROW FORMAT DELIMITED ")
                     },
                     if (!is.null(field_terminator)) {
                       sql(paste0("FIELDS TERMINATED BY \"", field_terminator, "\" "))
                     },
                     if (!is.null(line_terminator)) {
                       sql(paste0("LINES TERMINATED BY \"", line_terminator, "\" "))
                     },
                     if (!is.null(file_format)) {
                       sql(paste0("STORED AS ", file_format, " "))
                     },
                     fields,
                     con = con)
    DBI::dbExecute(con, sql)
  }
