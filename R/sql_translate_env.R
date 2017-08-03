#' @export
sql_translate_env.connectR_connection<-
  function(con){
    dbplyr::sql_variant(
      scalar = dbplyr::sql_translator(.parent = dbplyr::base_odbc_scalar,
                                      `!=`    = dbplyr::sql_infix("<>"),
                                      #types
                                      as.character=function(x,y){
                                        dbplyr::build_sql("CAST(",x," AS CHAR(",as.integer(y),"))")
                                      },
                                      as.varchar=function(x,y){
                                        dbplyr::build_sql("CAST(",x," AS VARCHAR(",as.integer(y),"))")
                                      },
                                      as.int = function(x){
                                        dbplyr::build_sql("CAST(", x, " AS INT)")
                                      },
                                      as.integer = function(x){
                                        dbplyr::build_sql("CAST(", x, " AS INT)")
                                      },
                                      as.bigint = function(x){
                                        dbplyr::build_sql("CAST(", x, " AS BIGINT)")
                                      },
                                      as.smallint = function(x){
                                        dbplyr::build_sql("CAST(", x, " AS SMALLINT)")
                                      },
                                      as.tinyint = function(x){
                                        dbplyr::build_sql("CAST(", x, " AS TINYINT)")
                                      },
                                      as.float = function(x){
                                        dbplyr::build_sql("CAST(", x, " AS FLOAT)")
                                      },
                                      as.decimal = function(x, pre = NULL, sca = NULL) {
                                        if (is.null(pre)) {
                                          dbplyr::build_sql("CAST(", x, " AS DECIMAL)")
                                        } else {
                                          if (is.null(sca)) {
                                            dbplyr::build_sql("CAST(", x, " AS DECIMAL(", as.integer(pre), "))")
                                          } else {
                                            dbplyr::build_sql("CAST(",
                                                              x,
                                                              " AS DECIMAL(",
                                                              as.integer(pre),
                                                              ",",
                                                              as.integer(sca),
                                                              "))")
                                          }
                                        }
                                      },
                                      as.timestamp = function(x)
                                        build_sql("CAST(", x, " AS TIMESTAMP)"),
                                      #functions
                                      paste=dbplyr::sql_prefix("CONCAT"),
                                      paste0= function(...){
                                        args<-list(...)
                                        connectR_build_sql(args)
                                      },
                                      grepl = function (x,y){
                                        if(length(x)>1){
                                          dbplyr::build_sql(y ," LIKE IN ",x)
                                        } else{
                                          dbplyr::build_sql(y ," LIKE ",x)
                                        }
                                      },
                                      # date and time functions (work like lubridate)
                                      yeardiff = function(x, y = NULL){
                                        if (is.null(y)){
                                          y<-dbplyr::build_sql("DATE")
                                        } else {y}
                                        dbplyr::build_sql("(", y," - ", x, ") YEAR(4)")
                                      },
                                      monthdiff = function(x, y=NULL){
                                        if (is.null(y)){
                                          y<-dbplyr::build_sql("DATE")
                                        } else {y}
                                        dbplyr::build_sql("(", y," - ", x, ") MONTH(4)")
                                      },
                                      datediff = function (x, y=NULL){
                                        if (is.null(y)){
                                          y<-dbplyr::build_sql("DATE")
                                        } else {y}
                                        dbplyr::build_sql("(TD_DAY_OF_CALENDAR(", y,") - TD_DAY_OF_CALENDAR(", x, "))")
                                      },
                                      d_wk = function (x=NULL){
                                        if(is.null(x)){
                                          dbplyr::build_sql("TD_DAY_OF_WEEK(DATE)")
                                        }else{
                                          dbplyr::build_sql("TD_DAY_OF_WEEK(",x,")")}
                                      },
                                      d_mth = function (x=NULL){
                                        if(is.null(x)){
                                          dbplyr::build_sql("TD_DAY_OF_MONTH(DATE)")
                                        }else{
                                          dbplyr::build_sql("TD_DAY_OF_MONTH(",x,")")}
                                      },
                                      d_yr = function (x=NULL){
                                        if(is.null(x)){
                                          dbplyr::build_sql("TD_DAY_OF_YEAR(DATE)")
                                        }else{
                                          dbplyr::build_sql("TD_DAY_OF_YEAR(",x,")")}
                                      },
                                      wd_mth = function (x=NULL){
                                        if(is.null(x)){
                                          dbplyr::build_sql("TD_WEEKDAY_OF_MONTH(DATE)")
                                        }else{
                                          dbplyr::build_sql("TD_WEEKDAY_OF_MONTH(",x,")")}
                                      },
                                      w_mth = function (x=NULL){
                                        if(is.null(x)){
                                          dbplyr::build_sql("TD_WEEK_OF_MONTH(DATE)")
                                        }else{
                                          dbplyr::build_sql("TD_WEEK_OF_MONTH(",x,")")}
                                      },
                                      w_yr = function (x=NULL){
                                        if(is.null(x)){
                                          dbplyr::build_sql("TD_WEEK_OF_YEAR(DATE)")
                                        }else{
                                          dbplyr::build_sql("TD_WEEK_OF_YEAR(",x,")")}
                                      },
                                      m_yr = function (x=NULL){
                                        if(is.null(x)){
                                          dbplyr::build_sql("TD_MONTH_OF_YEAR(DATE)")
                                        }else{
                                          dbplyr::build_sql("TD_MONTH_OF_YEAR(",x,")")}
                                      },
                                      year = function (x=NULL){
                                        if(is.null(x)){
                                          dbplyr::build_sql("TD_YEAR_OF_CALENDAR(DATE)")
                                        }else{
                                          dbplyr::build_sql("TD_YEAR_OF_CALENDAR(",x,")")}
                                      },
                                      #Influence from SAS
                                      scan = function(x,position,pattern){
                                        dbplyr::build_sql("STRTOK(",x,",",pattern,",",as.integer(position),")")
                                      }
      ),
      dbplyr::base_odbc_agg,
      dbplyr::base_odbc_win
    )
  }

#Adapted from dbplyr
connectR_build_sql<-
  function (..., .env = parent.frame())
  {
    escape_expr <- function(x) {
      if (is.character(x))
        return(x)
      val <- rlang::eval_bare(x, .env)
      if (is.null(val))
        return("")
      dbplyr::escape(val, collapse="||")
    }
    pieces <- vapply(list(...), escape_expr, character(1))
    sql(paste0(pieces))
  }
