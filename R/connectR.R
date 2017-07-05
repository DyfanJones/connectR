#'src_connectR connect to Databases
#'
#'Connects to databases currently using the odbc api, and also ecrypt password for safe storage.
#'@return Connection to database
#'@param dsn:The Data Source Name.
#'@param uid: User id for database connecting to.
#'@param pwd: User password for database
#'@param db: Database name i.e. TMG
#'@param Update: Update encrypted password
#'@param ...: Additional ODBC keywords, these will be joined with the other arguments to form the final connection string.
#'
#'@export
#'@examples
#'  #Standard connection to teradata and to database "postgres",
#'  unadviced as password in contained within the code:
#'    src_connectR(dsn = "POSTGRES", uid = "USERNAME", pwd = "PaSsword",
#'     db = "postgres", Update = F)
#'
#'  #Standard connection to teradata and to all database,
#'  unadviced as password in contained within the code:
#'    src_connectR(dsn = "POSTGRES", uid = "USERNAME", pwd = "PaSsword",
#'     Update = F)
#'
#'  #Connection to teradata and to all databases,
#'  R will take store encrypted username and password:
#'    connectR(dsn = "POSTGRES")
#'
#'  #Connection to teradata and to all databases,
#'  as Update = TRUE, stored password will be updated:
#'    connectR(dsn = "POSTGRES",Update=T)


#----Wrapper for connecting for a connection function -----
src_connectR <- function(dsn=NULL, uid=NULL, pwd=NULL, Update=FALSE,db=NULL,..., auto_disconnect = FALSE) {
  if (!requireNamespace("assertthat", quietly = TRUE)) {
    stop("assertthat is required to use src_connectR", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr is required to use src_connectR", call. = FALSE)
  }
  if (!requireNamespace("dbplyr", quietly = TRUE)) {
    stop("dbplyr is required to use src_connectR", call. = FALSE)
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("DBI is required to use src_connectR", call. = FALSE)
  }
  if (!requireNamespace("sodium", quietly = TRUE)) {
    stop("sodium is required to use src_connectR", call. = FALSE)
  }
  if(!is.character(dsn)){
    stop("dsn must be character. Please check your odbc", call. = FALSE)
  }
  
  #Simplity for user
  if (is.null(pwd) && rstudioapi::isAvailable() && !file.exists(paste0(Sys.getenv("USERPROFILE"),"\\HOST_R64.csv"))) {
    if(grepl("zeus",tolower(dsn))){pwd <- rstudioapi::askForPassword("Input Password for Teradata")}
    else{pwd <- rstudioapi::askForPassword("Input Password for Postgres")}
    store(dsn,pwd)
  }
  
  if (is.null(pwd) && rstudioapi::isAvailable() && Update==T){
    if(grepl("zeus",tolower(dsn))){pwd <- rstudioapi::askForPassword("Input Password for Teradata")}
    else{pwd <- rstudioapi::askForPassword("Input Password for Postgres")}
    store(dsn,pwd)
  }
  
  
  # build the connection string - we need the dsn to be defined
  uid<-if(is.null(uid)) {Sys.getenv("USERNAME")} else {uid}
  pwd<-if(is.null(pwd)){HOST_R64(dsn)} else {pwd}
  
  con<-DBI::dbConnect(odbc::odbc(),dsn=toupper(dsn),uid=uid, pwd=pwd,database=db,...)
  
  ##details for class
  info<-DBI::dbGetInfo(con)
  info$package<-attr(attr(getClass(class(con)[1]), "className"), "package")
  info[c("host","port")]<-NULL
  
  disco <- db_disconnector(con)
  
  if (isClass("connectR_connection", where = .GlobalEnv)) {
    removeClass("connectR_connection", where = .GlobalEnv)
  }
  setClass("connectR_connection",
           contains=class(con),
           where=.GlobalEnv)
  
  con<-structure(con,class=c("connectR_connection",class(con)))
  attributes(con)$info<-info
  
  dbplyr::src_sql("connectR",
                  con = con,
                  disco = disco,
                  info = info)
}

#---- encoding -----

store<-function(dsn,pwd){
  nw(dsn,pwd)->table
  write.csv(table,
            file=paste0(Sys.getenv("USERPROFILE"),"\\HOST_R64.csv"))
}

stage<-function(pwd){
  msg <- charToRaw(pwd)
  pad <- sodium::random(length(msg))
  text <- base::xor(msg, pad)
  pad2<-sodium::random(length(text))
  text2<-base:::xor(text, pad2)
  table<-data.frame(HOST_R64K=rawToChar(pad),
                    HOST_R64L=rawToChar(pad2),
                    HOST_R64=rawToChar(text2),
                    stringsAsFactors = F)
}

nw<-function(dsn,pwd){
  if(grepl("zeus",tolower(dsn))){
    if(!file.exists(paste0(Sys.getenv("USERPROFILE"),"\\HOST_R64.csv"))){
      table<-stage(pwd)
    }
    else {
      table<-read.csv(paste0(Sys.getenv("USERPROFILE"),"\\HOST_R64.csv"),stringsAsFactors = F)
      t<-stage(pwd)
      table[,-1]->table
      table[1,]<-t
    }
  }
  else {
    if(!file.exists(paste0(Sys.getenv("USERPROFILE"),"\\HOST_R64.csv"))){
      t<-stage(pwd)
      data.frame(HOST_R64K=NA,
                 HOST_R64L=NA,
                 HOST_R64=NA,
                 stringsAsFactors = F)->table
      table<-rbind(table,t)}
    else {
      table<-read.csv(paste0(Sys.getenv("USERPROFILE"),"\\HOST_R64.csv"),stringsAsFactors = F)
      t<-stage(pwd)
      table[,-1]->table
      table[2,]<-t
    }
  }
  return(table)
}

HOST_R64<-function(dsn){
  t<-read.csv(paste0(Sys.getenv("USERPROFILE"),"\\HOST_R64.csv"),stringsAsFactors = F)
  if(grepl("zeus",tolower(dsn))){
    t<-t[1,]
  } else {
    t<-t[2,]
  }
  
  if(is.na(t$HOST_R64)){
    pwd <- rstudioapi::askForPassword("Input Password for Database")
    store(dsn,pwd)
    t<-read.csv(paste0(Sys.getenv("USERPROFILE"),"\\HOST_R64.csv"),stringsAsFactors = F)
    if(grepl("zeus",tolower(dsn))){
      t<-t[1,]
    } else {
      t<-t[2,]
    }
  }
  st<-base::xor(charToRaw(t$HOST_R64),charToRaw(t$HOST_R64L))
  pwd<-rawToChar(base::xor(st,charToRaw(t$HOST_R64K)))
  return(pwd)
}

#---- disconnector ----

db_disconnector <- function(con, quiet = FALSE) {
  reg.finalizer(environment(), function(...) {
    if (!quiet) {
      message("Auto-disconnecting ", class(con)[[1]])
    }
    DBI::dbDisconnect(con)
  })
  environment()
}