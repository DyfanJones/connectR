#'db_rename_column_name will reformat column names align to databases
#'
#'Creates a vector with reformatted columns names from input data.frame
#'@return A vector with reformatted columns names from input data.frame
#'@param conn: Connection to database. Can used assigned output from function connectR for the connection.
#'@param value: Data.frame
#'@param col_name: Column name format for columns that start with a number
#'
#'@examples
#'  #Creates a vector with reformatted column names and any column begining with a
#'    number with renamed to `Col[0-9]`:
#'    db_rename_column(post, testdata)



db_rename_column <-
  function(con,
           value,
           col_name = "Col") {
    UseMethod("db_rename_column")
  }


db_rename_column.src_connectR <-
  function(conn,
           value,
           col_name = "Col") {
    assertthat::assert_that(is.data.frame(value),
                            is.character(col_name))
    rename <- names(value)
    for (i in 1:length(names(value))) {
      if (grepl("^[0-9]", names(value[i]))) {
        rename[i] <- paste0(col_name, names(value[i]))
      } else {
        rename[i] <- names(value[i])
      }
    }
    gsub("\\.", "", rename) -> rename
    gsub(" ", "_", trimws(rename)) -> rename
    return(rename)
  }
