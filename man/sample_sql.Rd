% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/top-sample.R
\name{sample_sql}
\alias{sample_sql}
\title{Select sample n rows returns a lazy evaluation object}
\usage{
sample_sql(x = NULL, n = NULL)
}
\arguments{
\item{x:}{Lazy evaluation object. Object created from the function `tbl`.}

\item{n:}{Number of rows, n defaults to 10.}
}
\value{
Returns a lazy evaluation object with n rows from a lazy evaluation object.
}
\description{
sample_sql gets a sample of n rows from lazy evaluation object and returns a lazy evaluation object.
}
\examples{
#Sample 20 rows from lazy evaluation object
   tbl(post,sql("SELECT * FROM
                 CALENDAR")) \%>\%
   group_by(ACCOUNT_YEAR_WEEK,
            SEASON) \%>\%
   summarise(WEEK_ENDING=max(CAL_DATE)) \%>\%
   sample_sql(n=20)
}
