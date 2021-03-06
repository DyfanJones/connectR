
connectR <img src="man/figures/logo.png" align="right" width="150px"/>
======================================================================

August 03, 2017

Overview
--------

connectR is a wrapper package built on `dplyr`, `dbplyr`, `DBI` and `odbc` packages to give it the functionality to connect to databases. connectR attempts to give a user friendly approach by removing the need to hard code passwords and userid for the connection, to this it utilies the package `sodium`.

Installation
------------

You can install the current development version from github:

    devtools::install_github("DyfanJones/connectR")

You must also install drivers to provide connectivity to the databases you are wanting to connect to. To connect to databases, you need to double check that you have 64 bit odbc drivers, if not this will restrict what version of R you can use.

Connecting to Databases
-----------------------

To make a connection to a database, call `src_connectR()`. `src_connectR()` has been design to be as user friendly as possible. `src_connectR()` has the following arguments:

    - dsn: Data Source Name (refer to odbc names i.e. "teradata")
    - uid: User ID on the database (Optional)
    - pwd: User's password (Optional)
    - Update: Update encrypted password (defaulted to FALSE)
    - db: Database, this will default user's default database on the database i.e. POST

This means you can connect to a database by only providing the dsn. connectR's function `src_connectR()` will handle the rest.

``` r
library(tidyverse)
library(connectR)

src_connectR(dsn="PostgreSQL35W")->post
```

Using dplyr
-----------

Now you can use dplyr functions against the tables in the database.

To see what tables are in the current database, you can issue the command:

    src_tbls(post)

This will return a list of tables. connectR offers extra methods, you can use one of the following functions `dbtbls` and `dbusr`. The first function will bring back all the tables in the default database as a tibble, it will also give extra information such as table creator and timestamp of when the table was created. `dbusr` will bring back all tables you the user have created, it will also give more information for example current permitted, skew factor, table creator and timstamp. They can be used in the following manner:

    dbtbls(post)->post_tables
    dbusr(post)->my_post_tables

Next we can create a lazy tbl from any table that exists inside the database.

    testtable<-tbl(post,"testtable")
    testtable<-tbl(post,sql("select * from testable"))

For more information about dplyr and connectR, read vignettes in package. Or go to the following guides built be Rstudio:

-   [Introduction to dplyr](https://blog.rstudio.org/2017/06/13/dplyr-0-7-0/amp/)
-   [Programming with dplyr](http://dplyr.tidyverse.org/articles/programming.html)

Loading Local Data into a Database
----------------------------------

connectR has two key methods from uploading data from R to a database. The main one is the `copy_to` function. This function has been overloaded from the dplyr package. `copy_to` function has the following arguements:

    conn: connetion to a database
    df: Dataframe in R
    name: Name to be created on the database
    overwrite: Overwrites any tables with the a name of the database.
    temporary: create a temporary or permanent table
    indexes: Any non unique indexes (need to be in list format)
    unique_indexes: Any unique indexes (need to be in list format)
    force: force the upload of the table
    append: append to any existing table
    primary: primary indexes (doesn't need to be list format)

Here is an example of uploadeding a complicated table. We can see we want to send the data.frame `test` to the database with a name "drj\_testtable", also with `indexes :list(c("type_1","type_2"))` and `primary indexes: c("x","name"))`.

    copy_to(post,test,"drj_testtable",indexes=list(c("type_1","type_2")),primary=c("x","name"))

This one line of code will do all the work for you so you don't need to think about column classes etc...

If you want to know more about connectR then look into the vigenettes that have been built with the package.
