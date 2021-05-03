library(RMySQL)
require(DBI)

gameday_upload <- function(year_list) {

  for (i in 1:(length(year_list)-1)){
    df <- get_payload(start = year_list[[i]], end = year_list[[i+1]])
    
    conn <- dbConnect(MySQL(), 
                      dbname = "redacted",
                      user = "redacted", 
                      password = "redacted",
                      host = "redacted",
                      port = 3306)
    dbWriteTable(conn, name = 'atbat', value = df$atbat, append = TRUE, row.names = FALSE)
    dbWriteTable(conn, name = 'action', value = df$action, append = TRUE, row.names = FALSE)
    dbWriteTable(conn, name = 'pitch', value = df$pitch, append = TRUE, row.names = FALSE)
    dbWriteTable(conn, name = 'runner', value = df$runner, append = TRUE, row.names = FALSE)
    dbWriteTable(conn, name = 'po', value = df$po, append = TRUE, row.names = FALSE)
    dbDisconnect(conn)
  }
}