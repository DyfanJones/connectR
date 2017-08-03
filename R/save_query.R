#'save_query saves connectR's sql to string for tbl, and optional saves to local file.
#'
#'Allows to save connectR's sql query in local file and save to a string for further tbl's. NOTE: save_query will only work with lazy evaluation objects.
#'@return Returns string format of connectR's sql, and optional save to local file.
#'@param x: Lazy evaluation object. Object created from the function `tbl`.
#'@param save_sql: Saves connectR sql when set to `TRUE`, default set to `FALSE`.
#'@param path: Path to save file, default set to work directory `getwd()`.
#'@param name: Name of the file, default set to "connectR_sql". Name is only needed and file type is not needed.
#'@param type: File type for saving local file for example `.txt`, defaults to `.SQL`.
#'
#'@examples
#'#Save lazy evaluation sql to a string
#'    tbl(post,sql("SELECT * FROM
#'                  CIS.CMI_CALENDAR")) %>%
#'    group_by(ACCOUNT_YEAR_WEEK,
#'             SEASON) %>%
#'    summarise(WEEK_ENDING=max(CAL_DATE)) %>%
#'    save_query ->SQL
#'#Uses sql string for further lazy_evaluation querying
#'    tbl(post,sql(SQL))
#'
#'#Save lazy evaluation sql to default location `getwd()`
#'#with default name `connectR_sql`
#'    tbl(post,sql("SELECT * FROM
#'                  CIS.CMI_CALENDAR")) %>%
#'    group_by(ACCOUNT_YEAR_WEEK,
#'             SEASON) %>%
#'    summarise(WEEK_ENDING=max(CAL_DATE)) %>%
#'    save_query(save_query=TRUE)
#'
#'#Save lazy evaluation sql to default location `C:/R_CODE`
#'#with default name `connectR_sql`
#'    tbl(post,sql("SELECT * FROM
#'                  CIS.CMI_CALENDAR")) %>%
#'    group_by(ACCOUNT_YEAR_WEEK,
#'             SEASON) %>%
#'    summarise(WEEK_ENDING=max(CAL_DATE)) %>%
#'    save_query(save_query=TRUE, path="C:/R_CODE")
#'
#'#Save lazy evaluation sql to default location `C:/R_CODE`
#'#with name `Test_sql`
#'    tbl(post,sql("SELECT * FROM
#'                  CIS.CMI_CALENDAR")) %>%
#'    group_by(ACCOUNT_YEAR_WEEK,
#'             SEASON) %>%
#'    summarise(WEEK_ENDING=max(CAL_DATE)) %>%
#'    save_query(save_query=TRUE, path="C:/R_CODE", name="Test_sql")
#'
#'#Save lazy evaluation sql to default location `C:/R_CODE`
#'#with name `Test_sql` and type ".txt" text file.
#'    tbl(post,sql("SELECT * FROM
#'                  CIS.CMI_CALENDAR")) %>%
#'    group_by(ACCOUNT_YEAR_WEEK,
#'             SEASON) %>%
#'    summarise(WEEK_ENDING=max(CAL_DATE)) %>%
#'    save_query(save_query=TRUE, path="C:/R_CODE", name="Test_sql",type=".txt")

#'@export
save_query <-
  function(x,
           save_sql = FALSE,
           path = NULL,
           name = NULL,
           type = NULL) {
    UseMethod("save_query")
  }

save_query.tbl_connectR <-
  function(x,
           save_sql = FALSE,
           path = NULL,
           name = NULL,
           type = NULL) {
    assertthat::assert_that(assertthat::is.flag(save_sql))
    
    vars <- dbplyr::op_vars(x)
    x_aliased <- select(x, !!!rlang::syms(vars))
    SQL <- dbplyr::db_sql_render(x$src$con, x_aliased$ops)
    gsub(",", ",\n   ", paste0(SQL)) -> SQL
    
    if (save_sql == TRUE && is.null(path) && is.null(name) && is.null(type)) {
      writeLines(sql(SQL), paste0(getwd(), "/connectR_sql.SQL"))
    }
    
    if (save_sql == TRUE && is.null(path) && !is.null(name) && is.null(type)) {
      writeLines(sql(SQL), paste0(getwd(), "/", name, ".SQL"))
    }
    
    if (save_sql == TRUE && is.null(path) && is.null(name) && !is.null(type)) {
      writeLines(sql(SQL), paste0(getwd(), "/connectR_sql" ,type))
    }
    
    if (save_sql == TRUE && is.null(path) && !is.null(name) && !is.null(type)) {
      writeLines(sql(SQL), paste0(getwd(), "/", name, type))
    }
    
    if (save_sql == TRUE && !is.null(path) && is.null(name) && is.null(type)) {
      writeLines(sql(SQL), paste0(path, "/connectR_sql.SQL"))
    }
    
    if (save_sql == TRUE && !is.null(path) && !is.null(name) && is.null(type)) {
      writeLines(sql(SQL), paste0(path, "/", name, ".SQL"))
    }
    
    if (save_sql == TRUE && !is.null(path) && is.null(name) && !is.null(type)) {
      writeLines(sql(SQL), paste0(path, "/connectR_sql" ,type))
    }
    
    if (save_sql == TRUE && !is.null(path) && !is.null(name) && !is.null(type)) {
      writeLines(sql(SQL), paste0(path, "/", name, type))
    }
    SQL
  }
