connectR initial build
================

Overview
--------

connectR is a wrapper package built on the packages dplyr and dbplyr connecting to databases using the package DBI, initially using odbc connections. connectR attempts to give a user friendly approach by removing the need to hard code passwords and userid for the connection to databases.

Usage
-----

The src\_connectR function attempts to be as user friendly as possible. By reducing the need of hard coding userid and password. Also defaulting to as standard database.

``` r
library(tidyverse)
library(connectR)

src_connectR("PostgreSQL35W")->post
```

**Note: connectR is in infancy it's infancy**
