#'db_column_info returns column information from a SQL query
#'
#'Creates a tibble with information about the columns from a SQL query
#'@return A Tibble containing column information from a SQL query
#'@param conn: Connection to database. Can used assigned output from function connectR for the connection.
#'@param statement: SQL query
#'
#'@export
#'@examples
#'  #Creates a tibble/ data.frame with information about
#'   the columns in a SQL query
#'    db_column_info(zeus,"select * from cis.cmi_daily_demand")

db_column_info<-function(conn, statement){
  assertthat::assert_that(assertthat::is.string(statement))
  
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr is required to use db_column_info", call. = FALSE)
  }
  
  if(conn$info$dbms.name=="Teradata"){
    
    if(grepl("select", tolower(statement))){
      end<-" as A"
    } else {
      end<-""
    }
    
    statement<-paste0("help column * from (",statement,")", end)
    suppressWarnings(DBI::dbSendStatement(con = conn$con, statement))->stage1
    
    suppressWarnings(DBI::dbFetch(stage1))->stage2
    stage2$Type<-trimws(stage2$Type)
    
    data_frame(help_type=c("A1","AN","AT","BF","BO","BV","CF","CO","CV","D","DA","DH","DM","DS",
                           "DY","F","HM","HS","HR","I","I1","I2","I8","JN","MI","MO","MS","N",
                           "PD","PM","PS","PT","PZ","SC","SZ","TS","TZ","UT","XM","YM","YR","=++"),
               TYPE=c("ARRAY","MULTI-DIMENSIONAL ARRAY","TIME","BYTE","BLOB","VARBYTE","CHARACTER",
                      "CLOB","VARCHAR","DECIMAL","DATE","INTERVAL DAY TO HOUR","INTERVAL DAY TO MINUTE",
                      "INTERVAL DAY TO SECOND","INTERVAL DAY","FLOAT","INTERVAL HOUR TO MINUTE",
                      "INTERVAL HOUR TO SECOND","INTERVAL HOUR","INTEGER","BYTEINT","SMALLINT",
                      "BIGINT","JSON","INTERVAL MINUTE","INTERVAL MONTH","INTERVAL MINUTE TO SECOND",
                      "NUMBER","PERIOD(DATE)","PERIOD(TIMESTAMP WITH TIME ZONE)","PERIOD(TIMESTAMP)",
                      "PERIOD(TIME)","PERIOD(TIME WITH TIME ZONE)","INTERVAL SECOND",
                      "TIMESTAMP WITH TIME ZONE","TIMESTAMP","TIME WITH TIME ZONE",
                      "UDT Type","XML","INTERVAL YEAR TO MONTH","INTERVAL YEAR",
                      "TD_ANYTYPE"))->lookup
    
    left_join(stage2,lookup, by=c("Type"="help_type")) %>%
      mutate (FORMAT=ifelse(TYPE=="DATE",Format,`Max Length`),
              test=ifelse(!is.na(`Decimal Fractional Digits`),
                          stringr::str_c(FORMAT,`Decimal Fractional Digits`,sep=", "),trimws(FORMAT)),
              Column_format=ifelse(TYPE=="DATE",test,paste0("(",test,")"))) %>%
      select(Column_name=`Column Name`,TYPE, Column_format)->table_info
    
    return(tbl_df(table_info))
  } else{
    
    if(grepl("select", tolower(statement))){
      stop("Unable to search columns from a select statement in postgres", call. = FALSE)
    }
    
    sel<-"select column_name, data_type, character_maximum_length"
    fro<-" from INFORMATION_SCHEMA.COLUMNS"
    whe<-paste0(" where table_name = \'", statement ,"\'")
    
    statement<-paste0(sel,fro,whe)
    
    tbl(conn,sql(statement)) %>% collect->table_info
    return(table_info)
  }
}
