library(RMySQL)
require(DBI)

conn <- dbConnect(MySQL(), 
                  dbname = "lahman",
                  user = "redacted", 
                  password = "redacted",
                  host = "redacted",
                  port = 3306)