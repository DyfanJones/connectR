% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbusr.R
\name{dbusr}
\alias{dbusr}
\title{dbusr search database for user created tables}
\usage{
dbusr(conn, uid = NULL, sbstr = NULL, db = NULL, schema = NULL,
  query = FALSE)
}
\arguments{
\item{conn:}{Connection to database. Can used assigned output from function connectR for the connection.}

\item{uid:}{User id for database}

\item{sbstr:}{A 3 character string, that searches table names i.e. "DL_". Can set sbstr to be a vector for multiple search i.e. c("DL_","DJ_")}

\item{db:}{Database name i.e. default}

\item{schema:}{Database schema i.e public}
}
\value{
Returns all tables user has created from the database.
}
\description{
Searches the databases and schema for user created tables, returns skewness and size of tables.
}
\examples{
 #Search for default user (account signed into the computer)
  tables in the database default.
   dbusr(post)->dbtables

 #Search for default user (account signed into the computer)
  tables in the database default, also searches for c("DJ_","DRJ") and
   at start of table name variable.
   dbusr(post,sbstr=c("DJ_","DRJ"))->dbtables

 #Search for default user, (account signed into the computer)
  tables in the database postgres, shopdirect and also searches in the schema public
   at start of table name variable.
   dbusr(post,sbstr=c("DJ_","DRJ"), db=c("postgres","postgres1"), schema="public")->dbtables
}
