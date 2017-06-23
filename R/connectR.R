#'src_connectR connect to Databases
#'
#'Connects to databases currently using the odbc api, and also ecrypt password for safe storage.
#'@return Connection to database
#'@param dsn:The Data Source Name.
#'@param uid: User id for database connecting to.
#'@param pwd: User password for database
#'@param db: Database name i.e. default
#'@param Update: Update encrypted password
#'@param ...: Additional ODBC keywords, these will be joined with the other arguments to form the final connection string.
#'
#'@export
#'@examples
#'  #Standard connection to Teradata and to database "postgres",
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
src_connectR <- function(dsn=NULL, uid=NULL, pwd=NULL, Update=F,db=NULL,..., auto_disconnect = FALSE) {
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

  #Simplity for user
  if (is.null(pwd) && rstudioapi::isAvailable() && !file.exists(paste0(Sys.getenv("USERPROFILE"),"\\HOST_R64.csv"))) {
    pwd <- rstudioapi::askForPassword("Input Password for Teradata")
    store(pwd)
  }

  if (is.null(pwd) && rstudioapi::isAvailable() && Update==T){
    pwd <- rstudioapi::askForPassword("Input Password for Teradata")
    store(pwd)
  }

  # build the connection string - we need the dsn to be defined
  stopifnot(is.character(dsn))
  uid<-if(is.null(uid)) {Sys.getenv("USERNAME")} else {uid}
  pwd<-if(is.null(pwd)){HOST_R64()} else {pwd}


  #db default for Teradata and postgres
  if(grepl("Teradata",tolower(dsn)) && is.null(db)){
    db<-"default" #will need changing dependent on user's default database.
  } else {db}

  if(grepl("post",tolower(dsn)) && is.null(db)){
    db<-"postgres"
  } else {db}

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

store<-function(pwd){
  msg <- charToRaw(pwd)
  pad <- sodium::random(length(msg))
  text <- base::xor(msg, pad)
  pad2<-sodium::random(length(text))
  text2<-base:::xor(text, pad2)
  table<-data.frame(HOST_R64K=rawToChar(pad),
                    HOST_R64L=rawToChar(pad2),
                    HOST_R64=rawToChar(text2),
                    stringsAsFactors = F)
  table<-write.csv(table,
                   file=paste0(Sys.getenv("USERPROFILE"),"\\HOST_R64.csv"))
}

HOST_R64<-function(){
  t<-read.csv(paste0(Sys.getenv("USERPROFILE"),"\\HOST_R64.csv"),stringsAsFactors = F)
  stage<-base::xor(charToRaw(t$HOST_R64),charToRaw(t$HOST_R64L))
  rawToChar(base::xor(stage,charToRaw(t$HOST_R64K)))
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
