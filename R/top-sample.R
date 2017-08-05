#'Select top n rows returns data.frame
#'
#'top_df gets the top n of rows from lazy evaluation object and returns a data.frame.
#'@return Returns a data.frame with n rows from a lazy evaluation object.
#'@param x: Lazy evaluation object. Object created from the function `tbl`.
#'@param n: Number of rows, n defaults to 10.
#'
#'@examples
#'#Top 20 rows from lazy evaluation object
#'    tbl(post,sql("SELECT * FROM
#'                  CALENDAR")) %>%
#'    group_by(ACCOUNT_YEAR_WEEK,
#'             SEASON) %>%
#'    summarise(WEEK_ENDING=max(CAL_DATE)) %>%
#'    top_df(n=20)

#'@export
top_df <-
  function(x = NULL,
           n = NULL) {
    UseMethod("top_df")
  }

top_df.tbl_connectR <-
  function(x,
           n = 10) {
    n<-as.integer(n)
    assertthat::assert_that(is.integer(n))
    vars <- dbplyr::op_vars(x)
    x_aliased <- select(x, !!!rlang::syms(vars))
    SQL <- dbplyr::db_sql_render(x$src$con, x_aliased$ops)
    
    res <- DBI::dbSendQuery(x$src$con, SQL)
    tryCatch({
      out <- DBI::dbFetch(res, n = n)
    }, finally = {
      DBI::dbClearResult(res)
    })
    return(tbl_df(out))
  }


#'Select top n rows returns a lazy evaluation object, Teradata database only
#'
#'top gets the top n of rows from lazy evaluation object and returns a lazy evaluation object.
#'@return Returns a lazy evaluation object with n rows from a lazy evaluation object.
#'@param x: Lazy evaluation object. Object created from the function `tbl`.
#'@param n: Number of rows, n defaults to 10.
#'
#'@examples
#'#Top 20 rows from lazy evaluation object
#'    tbl(post,sql("SELECT * FROM
#'                  CALENDAR")) %>%
#'    group_by(ACCOUNT_YEAR_WEEK,
#'             SEASON) %>%
#'    summarise(WEEK_ENDING=max(CAL_DATE)) %>%
#'    top(n=20)

#'@export
top <-
  function(x = NULL,
           n = NULL) {
    UseMethod("top")
  }

top.tbl_connectR <-
  function(x,
           n = 10) {
    n<-as.integer(n)
    assertthat::assert_that(is.integer(n))
    vars <- dbplyr::op_vars(x)
    x_aliased <- select(x,!!!rlang::syms(vars))
    SQL <- dbplyr::db_sql_render(x$src$con, x_aliased$ops)
    gsub("^SELECT", paste0("SELECT TOP ", n, " "), SQL) -> SQL
    tryCatch({
      dbplyr::tbl_sql("connectR", src = x$src, from = SQL)
    })
  }

#'Select sample n rows returns a lazy evaluation object
#'
#'sample_sql gets a sample of n rows from lazy evaluation object and returns a lazy evaluation object.
#'@return Returns a lazy evaluation object with n rows from a lazy evaluation object.
#'@param x: Lazy evaluation object. Object created from the function `tbl`.
#'@param n: Number of rows, n defaults to 10.
#'
#'@examples
#'#Sample 20 rows from lazy evaluation object
#'    tbl(post,sql("SELECT * FROM
#'                  CALENDAR")) %>%
#'    group_by(ACCOUNT_YEAR_WEEK,
#'             SEASON) %>%
#'    summarise(WEEK_ENDING=max(CAL_DATE)) %>%
#'    sample_sql(n=20)


#'@export
sample_sql <-
  function(x = NULL,
           n = NULL) {
    UseMethod("sample_sql")
  }

sample_sql.tbl_connectR <-
  function(x,
           n = 10) {
    n<-as.integer(n)
    assertthat::assert_that(is.integer(n))
    vars <- dbplyr::op_vars(x)
    x_aliased <- select(x,!!!rlang::syms(vars))
    SQL <- dbplyr::db_sql_render(x$src$con, x_aliased$ops)
    sql(paste0(SQL, "\nSAMPLE ", n)) -> SQL
    tryCatch({
      dbplyr::tbl_sql("connectR", src = x$src, from = SQL)
    })
  }

#'Select sample n rows returns a data.frame
#'
#'sample_df gets a sample of n rows from lazy evaluation object and returns a data.frame.
#'@return Returns a lazy evaluation object with n rows from a data.frame.
#'@param x: Lazy evaluation object. Object created from the function `tbl`.
#'@param n: Number of rows, n defaults to 10.
#'
#'@examples
#'#Sample 20 rows from lazy evaluation object
#'    tbl(post,sql("SELECT * FROM
#'                  CALENDAR")) %>%
#'    group_by(ACCOUNT_YEAR_WEEK,
#'             SEASON) %>%
#'    summarise(WEEK_ENDING=max(CAL_DATE)) %>%
#'    sample_df(n=20)


#'@export
sample_df <-
  function(x = NULL,
           n = NULL) {
    UseMethod("sample_df")
  }

sample_df.tbl_connectR <-
  function(x,
           n = 10) {
    n<-as.integer(n)
    assertthat::assert_that(is.integer(n))
    vars <- dbplyr::op_vars(x)
    x_aliased <- select(x,!!!rlang::syms(vars))
    SQL <- dbplyr::db_sql_render(x$src$con, x_aliased$ops)
    sql(paste0(SQL, "\nSAMPLE ", n)) -> SQL
    res <- DBI::dbSendQuery(x$src$con, SQL)
    tryCatch({
      out <- DBI::dbFetch(res, n = n)
    }, finally = {
      DBI::dbClearResult(res)
    })
    return(tbl_df(out))
  }
