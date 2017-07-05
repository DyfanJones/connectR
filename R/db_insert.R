#'db_insert inserts data into a database table from a data.frame
#'
#'Inserts data into a database table from a data.frame
#'@return Inserts data.
#'@param conn: Connection to database. Can used assigned output from function connectR for the connection.
#'@param name: Table name on database data is inserting into.
#'@param values: Data.frame that is being sent to database.
#'@export
#'@examples
#'  #Inserts testtable data into the table "testtable" on the database.
#'    db_insert(post,"testtabe",testtable)


#---- db_insert_into ----
db_insert<-
  function(con, name, values, ...) {
    assertthat::assert_that(assertthat::is.string(table),
                            is.data.frame(values))
    
    cols <- lapply(values,dbplyr::escape,collapse=NULL,parens = FALSE)
    cols[[1]]<-paste0("INSERT INTO ", name, " VALUES (",cols[[1]])
    col_mat <-  matrix(unlist(cols, use.names = FALSE), nrow = nrow(test1))
    rows <- apply(col_mat, 1, paste0, collapse = ", ")
    Values <- paste0(rows, ");", collapse = "\n ")
    sql(Values)->sql
    
    suppressWarnings(DBI::dbExecute(con, sql,...))
  }